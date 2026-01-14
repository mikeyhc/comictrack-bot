-module(discord_api).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("types.hrl").

% Public API
-export([start_link/1]).
-export([get_gateway_bot/0]).
-export([register_command/2, interaction_callback/3]).
-export([direct_message/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER_NAME, ?MODULE).
-define(DISCORD_HOST, "discord.com").
-define(DISCORD_PORT, 443).
-define(DISCORD_API_VERSION, "v10").
-define(USER_AGENT, "comictrack/1.0").
-define(ACCEPT_STRING, "application/json").
-define(DEFAULT_TIMEOUT, 10_000).

-record(connection, {pid  :: pid(),
                     mref :: reference()
                    }).

-record(configuration, {bot_token :: string() | binary()}).

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
start_link(BotToken) ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [BotToken], []).

-spec get_gateway_bot() -> {ok, #{}}.
get_gateway_bot() ->
    {ok, {200, Body}} = api_call(get, "/gateway/bot"),
    {ok, json:decode(Body)}.

-spec register_command(non_neg_integer(), #{}) -> {ok, #{}}.
register_command(ApplicationId, Command) ->
    Url = build_url("/applications/~p/commands", [ApplicationId]),
    post_api_call(Url, jsone:encode(Command)).

-spec interaction_callback(iolist(), iolist(), #{}) -> ok.
interaction_callback(InteractionId, InteractionToken, Body) ->
    Url = build_url("/interactions/~s/~s/callback",
                    [InteractionId, InteractionToken]),
    ok = post_api_call(Url, jsone:encode(Body)),
    ok.

-spec direct_message(iolist(), iolist()) -> {ok, #{}}.
direct_message(UserId, Message) ->
    Url = build_url("/users/@me/channels", []),
    Body = #{<<"recipient_id">> => UserId},
    {ok, Response} = post_api_call(Url, jsone:encode(Body)),
    #{<<"id">> := ChannelId} = Response,
    DmUrl = build_url("/channels/~s/messages", [ChannelId]),
    DmBody = #{<<"content">> => Message},
    post_api_call(DmUrl, jsone:encode(DmBody)).

% gen_server callbacks
init([BotToken]) ->
    gen_server:cast(self(), connect),
    {ok, #state{configuration=#configuration{bot_token=BotToken}}}.

handle_call({get, Resource}, {Pid, _Tags},
            State=#state{connection=Conn, requests=Reqs}) ->
    ?LOG_DEBUG("sending GET to ~p", [Resource]),
    StreamRef = gun:get(Conn#connection.pid,
                        "/api/" ++ ?DISCORD_API_VERSION ++ Resource,
                        build_headers(State)),
    {reply, {ok, StreamRef},
     State#state{requests=Reqs#{StreamRef => #request{pid=Pid}}}};
handle_call({post, Resource, Body}, {Pid, _Tags},
            State=#state{connection=Conn, requests=Reqs}) ->
    ?LOG_DEBUG("sending POST to ~p", [Resource]),
    ?LOG_DEBUG("included body ~p", [Body]),
    StreamRef = gun:post(Conn#connection.pid,
                         "/api/" ++ ?DISCORD_API_VERSION ++ Resource,
                         build_headers(State, post_headers()),
                         Body),
    {reply, {ok, StreamRef},
     State#state{requests=Reqs#{StreamRef => #request{pid=Pid}}}}.

handle_cast(connect, State0) ->
    {ok, ConnPid} = gun:open(?DISCORD_HOST, ?DISCORD_PORT, #{}),
    MRef = monitor(process, ConnPid),
    {ok, Protocol} = gun:await_up(ConnPid),
    ?LOG_INFO("~p connected to ~s:~p with protocol ~p",
              [ConnPid, ?DISCORD_HOST, ?DISCORD_PORT, Protocol]),
    State1 = State0#state{connection=#connection{pid=ConnPid,
                                                 mref=MRef
                                                }},
    {noreply, State1}.

% TODO handle resending any failed streamrefs
handle_info({gun_down, ConnPid, _Protocol, normal, _StreamRefs=[]},
            State=#state{connection=#connection{pid=ConnPid}}) ->
    ?LOG_INFO("~p temporarily disconnected from ~s:~p",
              [ConnPid, ?DISCORD_HOST, ?DISCORD_PORT]),
    {ok, Protocol} = gun:await_up(ConnPid),
    ?LOG_INFO("~p reconnected to ~s:~p with protocol ~p",
              [ConnPid, ?DISCORD_HOST, ?DISCORD_PORT, Protocol]),
    {noreply, State};
% TODO handle resending any failed streamrefs
handle_info({gun_down, ConnPid, _Protocol, Err={error, _Error}, _StreamRefs=[]},
            _State) ->
    ?LOG_INFO("~p permanently disconnected from ~s:~p",
              [ConnPid, ?DISCORD_HOST, ?DISCORD_PORT]),
    throw(Err);
handle_info({gun_response, ConnPid, StreamRef, Fin, Status, _Headers},
            State=#state{connection=#connection{pid=ConnPid},
                         requests=ActiveRequests}) ->
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
    {ok, Ref} = gen_server:call(?SERVER_NAME, {Method, Resource}),
    await_response(Ref).

api_call(Method, Resource, Body) ->
    {ok, Ref} = gen_server:call(?SERVER_NAME, {Method, Resource, Body}),
    await_response(Ref).

await_response(Ref) ->  await_response(Ref, ?DEFAULT_TIMEOUT).

await_response(Ref, Timeout) ->
    receive
        {reply, Ref, Response} -> {ok, Response}
    after
        Timeout -> {error, timeout}
    end.

build_headers(State) ->
    build_headers(State, []).

build_headers(#state{configuration=Config}, Extra) ->
    [{<<"user-agent">>, ?USER_AGENT},
     {<<"accept">>, ?ACCEPT_STRING},
     {<<"authorization">>, "Bot " ++ Config#configuration.bot_token}
    ] ++ Extra.

post_headers() ->
    [{<<"content-type">>, <<"application/json">>}].

build_body(Parts) ->
    lists:foldl(fun(X, Acc) -> <<X/binary, Acc/binary>> end, <<>>, Parts).

build_url(String, Args) ->
    lists:flatten(io_lib:format(String, Args)).

post_api_call(Url, Body) ->
    {ok, {Code, Reply}} = api_call(post, Url, Body),
    ?LOG_DEBUG("~p recevied ~p", [Url, Code]),
    if Code >= 200 andalso Code < 300 ->
           case Reply of
               no_data -> ok;
               _ -> {ok, jsone:decode(Reply)}
           end;
       true -> {error, {invalid_code, Code, Reply}}
    end.
