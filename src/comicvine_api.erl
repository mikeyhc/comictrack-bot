-module(comicvine_api).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("types.hrl").

% Public API
-export([start_link/1]).
-export([issues/1, volume/1, volumes/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER_NAME, ?MODULE).
-define(COMICVINE_HOST, "comicvine.gamespot.com").
-define(COMICVINE_PORT, 443).
% this needs to be a well known agent
-define(USER_AGENT, "curl/8.7.1").
-define(DEFAULT_FORMAT, "json").
-define(DEFAULT_TIMEOUT, 10_000).
-define(DEFAULT_RETRIES, 3).

-define(ISSUE_TYPE, 4000).
-define(VOLUME_TYPE, 4050).

-record(connection, {pid  :: pid(),
                     mref :: reference()
                    }).

-record(configuration, {token :: string() | binary()}).

-record(request, {pid              :: pid(),
                  status=undefined :: optional(non_neg_integer()),
                  body=[]          :: [binary()]
                 }).

-record(state, {connection    :: optional(#connection{}),
                requests=#{}  :: #{reference() => #request{}},
                configuration :: #configuration{}
               }).

% Public API
-spec start_link(string() | binary()) -> {ok, pid()}.
start_link(Token) ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [Token], []).

-spec volume(non_neg_integer()) -> {ok, #{}} | {error, not_found}.
volume(VolumeId) ->
    {ok, {200, Body}} = api_call(get, build_url("/volume/~p-~p",
                                                [?VOLUME_TYPE, VolumeId])),
    Reply = jsone:decode(Body),
    case maps:get(<<"error">>, Reply) of
        <<"OK">> -> {ok, Reply};
        <<"Object Not Found">> -> {error, not_found}
    end.

-spec volumes([{string(), string()}]) -> {ok, #{}}.
volumes(Filters) ->
    {ok, {200, Body}} = api_call(get, "/volumes", [build_filter(Filters),
                                                   {"sort", "date_added:desc"}
                                                  ]),
    Reply = jsone:decode(Body),
    #{<<"error">> := <<"OK">>} = Reply,
    {ok, Reply}.

-spec issues([{string(), string()}]) -> {ok, #{}}.
issues(Filters) ->
    {ok, {200, Body}} = api_call(get, "/issues", [build_filter(Filters)]),
    Reply = jsone:decode(Body),
    #{<<"error">> := <<"OK">>} = Reply,
    {ok, Reply}.

% gen_server callbacks
init([Token]) ->
    gen_server:cast(self(), connect),
    {ok, #state{configuration=#configuration{token=Token}}}.

handle_call({get, Resource, Query}, {Pid, _Tags},
            State=#state{connection=Conn, requests=Reqs}) ->
    QueryString = build_query_params(default_query_params(State) ++ Query),
    FullUrl = "/api" ++ Resource ++ "/" ++ QueryString,
    ?LOG_DEBUG("sending GET to ~p", [FullUrl]),
    StreamRef = gun:get(Conn#connection.pid, FullUrl, build_headers(State)),
    {reply, {ok, StreamRef},
     State#state{requests=Reqs#{StreamRef => #request{pid=Pid}}}}.

handle_cast(connect, State0) ->
    {ok, ConnPid} = gun:open(?COMICVINE_HOST, ?COMICVINE_PORT, #{}),
    MRef = monitor(process, ConnPid),
    {ok, Protocol} = gun:await_up(ConnPid),
    ?LOG_INFO("~p connected to ~s:~p with protocol ~p",
              [ConnPid, ?COMICVINE_HOST, ?COMICVINE_PORT, Protocol]),
    State1 = State0#state{connection=#connection{pid=ConnPid,
                                                 mref=MRef
                                                }},
    {noreply, State1}.

% TODO handle resending any failed streamrefs
handle_info({gun_down, ConnPid, _Protocol, normal, _StreamRefs=[]},
            State=#state{connection=#connection{pid=ConnPid}}) ->
    ?LOG_INFO("~p temporarily disconnected from ~s:~p",
              [ConnPid, ?COMICVINE_HOST, ?COMICVINE_PORT]),
    {ok, Protocol} = gun:await_up(ConnPid),
    ?LOG_INFO("~p reconnected to ~s:~p with protocol ~p",
              [ConnPid, ?COMICVINE_HOST, ?COMICVINE_PORT, Protocol]),
    {noreply, State};
% TODO handle resending any failed streamrefs
handle_info({gun_down, ConnPid, _Protocol, Err={error, _Error}, _StreamRefs=[]},
            _State) ->
    ?LOG_INFO("~p permanently disconnected from ~s:~p",
              [ConnPid, ?COMICVINE_HOST, ?COMICVINE_PORT]),
    throw(Err);
handle_info({gun_response, ConnPid, StreamRef, Fin, Status, _Headers},
            State=#state{connection=#connection{pid=ConnPid},
                         requests=ActiveRequests}) ->
    ?LOG_DEBUG("headers: ~p", [_Headers]),
    case {Fin, maps:get(StreamRef, ActiveRequests, undefined)} of
        {_Fin, undefined} ->
            ?LOG_ERROR("~p stream ref ~p matches no active requests",
                       [ConnPid, StreamRef]),
            {noreply, State};
        {fin, #request{pid=Pid}} ->
            Pid ! {reply, StreamRef, {Status, no_data}},
            {noreply,
             State#state{requests=maps:remove(StreamRef, ActiveRequests)}};
        {nofin, Request0} ->
            Request1 = Request0#request{status=Status},
            {noreply,
             State#state{requests=ActiveRequests#{StreamRef => Request1}}}
    end;
handle_info({gun_data, ConnPid, StreamRef, Fin, Data},
            State=#state{connection=#connection{pid=ConnPid},
                         requests=ActiveRequests}) ->
    case {Fin, maps:get(StreamRef, ActiveRequests, undefined)} of
        {_Fin, undefined} ->
            ?LOG_ERROR("~p stream ref ~p matches no active requests",
                       [ConnPid, StreamRef]),
            {noreply, State};
        {fin, #request{pid=Pid, status=Status, body=Body}} ->
            Pid ! {reply, StreamRef, {Status, build_body([Data|Body])}},
            {noreply,
             State#state{requests=maps:remove(StreamRef, ActiveRequests)}};
        {nofin, Request0=#request{body=Body}} ->
            Request1 = Request0#request{body=[Data|Body]},
            {noreply,
             State#state{requests=ActiveRequests#{StreamRef => Request1}}}
    end.

% helper functions
api_call(Method, Resource) ->
    api_call(Method, Resource, []).

api_call(Method, Resource, QueryParams) ->
    api_call(Method, Resource, QueryParams, ?DEFAULT_RETRIES).

api_call(_Method, _Resource, _QueryParams, 0) -> {error, timeout};
api_call(Method, Resource, QueryParams, Retries) ->
    {ok, Ref} = gen_server:call(?SERVER_NAME, {Method, Resource, QueryParams}),
    case await_response(Ref) of
        {error, timeout} ->
            api_call(Method, Resource, QueryParams, Retries - 1);
        Reply -> Reply
    end.

build_body(Parts) ->
    lists:foldl(fun(X, Acc) -> <<X/binary, Acc/binary>> end, <<>>, Parts).

build_headers(State) ->
    build_headers(State, []).

build_headers(_State, Extra) ->
    [{<<"user-agent">>, ?USER_AGENT}] ++ Extra.

await_response(Ref) ->  await_response(Ref, ?DEFAULT_TIMEOUT).

await_response(Ref, Timeout) ->
    receive
        {reply, Ref, Response} -> {ok, Response}
    after
        Timeout -> {error, timeout}
    end.

build_query_params(QueryParams) ->
    Joiner = fun({Key, Val}, Acc) ->
                     uri_string:quote(Key) ++ "=" ++ uri_string:quote(Val)
                     ++ "&" ++ Acc
             end,
    "?" ++ lists:foldl(Joiner, "", QueryParams).

default_query_params(#state{configuration=#configuration{token=Token}}) ->
    [{"api_key", Token},
     {"format", ?DEFAULT_FORMAT}].

build_filter(Filters) ->
    MapFn = fun({K, V}) -> K ++ ":" ++ V end,
    {"filter", lists:flatten(lists:join(",", lists:map(MapFn, Filters)))}.

build_url(String, Args) ->
    lists:flatten(io_lib:format(String, Args)).
