-module(http_client).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("types.hrl").

% Public API
-export([start_link/2, api_call/3, api_call/4]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

% this needs to be a well known agent
-define(USER_AGENT, "curl/8.7.1").
-define(DEFAULT_TIMEOUT, 10_000).
-define(DEFAULT_RETRIES, 3).

-record(connection, {pid  :: pid(),
                     mref :: reference()
                    }).

-record(request, {pid              :: pid(),
                  status=undefined :: optional(non_neg_integer()),
                  body=[]          :: [binary()]
                 }).

-record(state, {connection    :: optional(#connection{}),
                requests=#{}  :: #{reference() => #request{}},
                configuration :: configuration()
               }).

-type configuration() :: #{host := iolist(),
                           port := pos_integer(),
                           headers := {iolist(), iolist()},
                           query_params := {iolist(), iolist()}}.

% Public API
-spec start_link(Name, Configuration) -> {ok, pid()}
    when Name :: atom(), Configuration :: configuration().
start_link(Name, Configuration) ->
    gen_server:start_link({local, Name}, ?MODULE, [Configuration], []).

api_call(ServerName, Method, Resource) ->
    api_call(ServerName, Method, Resource, []).

api_call(ServerName, Method, Resource, QueryParams) ->
    api_call_(ServerName, Method, Resource, QueryParams, ?DEFAULT_RETRIES).

% gen_server callbacks
init([Configuration]) ->
    {ok, #state{configuration=Configuration}}.

handle_call({get, Resource, Query}, {Pid, _Tags},
            State=#state{requests=Reqs}) ->
    Conn = get_connection(State),
    QueryString = build_query_params(default_query_params(State) ++ Query),
    FullUrl = "/api" ++ Resource ++ "/" ++ QueryString,
    ?LOG_DEBUG("sending GET to ~p", [FullUrl]),
    StreamRef = gun:get(Conn#connection.pid, FullUrl, build_headers(State)),
    {reply, {ok, StreamRef},
     State#state{connection=Conn,
                 requests=Reqs#{StreamRef => #request{pid=Pid}}}}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({gun_down, ConnPid, _Protocol, Reason, _StreamRefs},
            State=#state{configuration=#{host := Host,
                                         post := Port},
                         connection=#connection{pid=ConnPid,
                                                mref=MRef}}) ->
    case gun_util:handle_down(ConnPid, Reason, Host, Port) of
        connected -> {noreply, State};
        disconnected ->
            gun:close(ConnPid),
            gun_util:await_down(ConnPid, MRef),
            {noreply, State#state{connection=undefined}}
    end;
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
api_call_(_ServerName, _Method, _Resource, _QueryParams, 0) -> {error, timeout};
api_call_(ServerName, Method, Resource, QueryParams, Retries) ->
    {ok, Ref} = gen_server:call(ServerName, {Method, Resource, QueryParams}),
    case await_response(Ref) of
        {error, timeout} ->
            api_call_(ServerName, Method, Resource, QueryParams, Retries - 1);
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

default_query_params(#state{configuration=#{query_params := Params}}) -> Params.

connect(#state{configuration=#{host := Host, port := Port}}) ->
    {ok, ConnPid} = gun:open(Host, Port, #{}),
    MRef = monitor(process, ConnPid),
    {ok, Protocol} = gun:await_up(ConnPid),
    ?LOG_INFO("~p connected to ~s:~p with protocol ~p",
              [ConnPid, Host, Port, Protocol]),
    #connection{pid=ConnPid, mref=MRef}.

get_connection(State=#state{connection=undefined}) -> connect(State);
get_connection(State) -> State#state.connection.
