-module(discord_gateway).
-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").
-include("types.hrl").

% Public API
-export([start_link/2, behaviour_info/1]).

% gen_statem callbacks
-export([callback_mode/0, init/1]).
-export([disconnected/3, await_hello/3, await_ready/3, await_heartbeat_ack/3,
         connected/3, await_hello_reconnect/3]).

-define(DEFAULT_TIMEOUT, 5000).
-define(WSS_PORT, 443).
-define(WEBSOCKET_API_VERSION, "10").
-define(RESPONSE_ENCODING, "json").
-define(SERVER_NAME, ?MODULE).
-define(UPGRADE_TIMEOUT, ?DEFAULT_TIMEOUT).
-define(LIBRARY_NAME, <<"comictrack/1.0">>).
-define(INTENTS, 0).
-define(HEARTBEAT_ACK_TIMEOUT, ?DEFAULT_TIMEOUT).
-define(HELLO_TIMEOUT, ?DEFAULT_TIMEOUT).

% op codes
-define(MESSAGE_OP, 0).
-define(HEARTBEAT_OP, 1).
-define(IDENTIFY_OP, 2).
-define(RESUMSE_OP, 6).
-define(RECONNECT_OP, 7).
-define(INVALID_SESSION_OP, 9).
-define(HELLO_OP, 10).
-define(HEARTBEAT_ACK_OP, 11).

% ws codes
-define(WS_SERVICE_RESTART, 1012).

-record(connection, {pid  :: pid(),
                     mref :: reference(),
                     sref :: reference(),
                     host :: binary()
                    }).

-record(configuration, {bot_token :: string() | binary()}).

-record(data, {connection      :: optional(#connection{}),
               heartbeat       :: optional(reference()),
               sequence        :: optional(non_neg_integer()),
               resume_url      :: optional(binary()),
               session_id      :: optional(binary()),
               previous_state  :: optional(atom()),
               module          :: atom(),
               configuration   :: #configuration{}
               }).

% Public API
-spec start_link(atom(), string() | binary()) -> {ok, pid()}.
start_link(Module, BotToken) ->
    gen_statem:start_link({local, ?SERVER_NAME}, ?MODULE, [Module, BotToken],
                          []).

-spec behaviour_info(callbacks) -> [{atom(), non_neg_integer()}].
behaviour_info(callbacks) -> [{process, 1}].

% gen_statem callbacks
callback_mode() -> [state_functions, state_enter].

init([Module, BotToken]) ->
    gen_statem:cast(self(), connect),
    {ok, disconnected, #data{module=Module,
                             configuration=#configuration{bot_token=BotToken}}}.

disconnected(enter, _OldState, Data) ->
    {keep_state, Data};
disconnected(cast, connect, Data) ->
    {ok, Gateway} = discord_api:get_gateway_bot(),
    #{<<"url">> := Url} = Gateway,
    Connection = open_ws_connection(Url),
    {next_state, await_hello, Data#data{connection=Connection}};
disconnected(cast, reconnect, Data=#data{resume_url=Url}) ->
    Connection = open_ws_connection(Url),
    {next_state, await_hello_reconnect, Data#data{connection=Connection}}.

await_hello(enter, _OldState, Data) ->
    {keep_state, Data, [{state_timeout, ?HELLO_TIMEOUT, hello_timeout}]};
await_hello(info, {gun_ws, ConnPid, StreamRef, {text, Msg}},
            Data0=#data{connection=#connection{pid=ConnPid, sref=StreamRef}}) ->
    #{<<"op">> := ?HELLO_OP, <<"d">> := D} = jsone:decode(Msg),
    Data = start_heartbeat(D, Data0),
    Identify = build_identify_message(Data),
    send_ws_message(?IDENTIFY_OP, Identify, Data),
    {next_state, await_ready, Data};
await_hello(state_timeout, hello_timeout, Data) ->
    {stop, {error, hello_timeout}, Data};
await_hello(info, Msg, Data) ->
    handle_common(Msg, Data).

await_hello_reconnect(enter, _OldState, Data) ->
    {keep_state, Data, [{state_timeout, ?HELLO_TIMEOUT, hello_timeout}]};
await_hello_reconnect(info, {gun_ws, ConnPid, StreamRef, {text, Msg}},
                      Data0=#data{connection=#connection{pid=ConnPid,
                                                         sref=StreamRef}}) ->
    #{<<"op">> := ?HELLO_OP, <<"d">> := D} = jsone:decode(Msg),
    Data = start_heartbeat(D, Data0),
    Resume = build_resume_message(Data),
    send_ws_message(?RESUMSE_OP, Resume, Data),
    {next_state, await_ready, Data};
await_hello_reconnect(info, Msg, Data) ->
    handle_common(Msg, Data).

await_ready(enter, _OldState, Data) ->
    {keep_state, Data};
await_ready(info, {gun_ws, ConnPid, StreamRef, {text, JsonMsg}},
            Data0=#data{connection=#connection{pid=ConnPid, sref=StreamRef}}) ->
    Msg = jsone:decode(JsonMsg),
    case maps:get(<<"op">>, Msg) of
        ?MESSAGE_OP ->
            Data = update_session_data(Data0, maps:get(<<"d">>, Msg)),
            ?LOG_INFO("discord gateway is now connected"),
            {next_state, connected, update_seq(Msg, Data)};
        ?INVALID_SESSION_OP ->
            Data = prepare_reconnect(Data0),
            ?LOG_INFO("received invalid state, reconnecting"),
            {next_state, disconnected, Data};
        Op ->
            {stop, {unexpected_op_code, Op}, Data0}
    end;
await_ready(info, Msg, Data) ->
    handle_common(Msg, Data).

await_heartbeat_ack(enter, OldState, Data) ->
    {keep_state, Data#data{previous_state=OldState},
     [{state_timeout, ?HEARTBEAT_ACK_TIMEOUT, ack_timeout}]};
await_heartbeat_ack(info, {gun_ws, ConnPid,StreamRef, {text, JsonMsg}},
                    Data=#data{connection=#connection{pid=ConnPid,
                                                      sref=StreamRef}}) ->
    Msg = jsone:decode(JsonMsg),
    case maps:get(<<"op">>, Msg) of
        ?HEARTBEAT_ACK_OP ->
            ?LOG_INFO("received heartbeat ack"),
            {next_state, Data#data.previous_state,
             update_seq(Msg, Data#data{previous_state=undefined})};
        _ ->
            {keep_state, Data, [postpone]}
    end;
await_heartbeat_ack(info, heartbeat, Data0) ->
    ?LOG_WARNING("heartbeat received during heartbeat_ack, zombie connection, "
                 "reconnecting"),
    ws_close(?WS_SERVICE_RESTART, <<"heartbeat_ack not received">>, Data0),
    Data = prepare_reconnect(Data0),
    {next_state, disconnected, Data};
await_heartbeat_ack(state_timeout, ack_timeout, Data0) ->
    ?LOG_WARNING("heartbeat_ack not received, zombie connection, reconnecting"),
    ws_close(?WS_SERVICE_RESTART, <<"heartbeat_ack not received">>, Data0),
    Data = prepare_reconnect(Data0),
    {next_state, disconnected, Data};
await_heartbeat_ack(info, Msg, Data) ->
    handle_down(Msg, Data).

connected(enter, _OldState, Data) ->
    {keep_state, Data};
connected(info, {gun_ws, ConnPid,StreamRef, {text, JsonMsg}},
          Data0=#data{module=Module,
                      connection=#connection{pid=ConnPid,
                                             sref=StreamRef}}) ->
    Msg = jsone:decode(JsonMsg),
    case maps:get(<<"op">>, Msg) of
        ?MESSAGE_OP ->
            ?LOG_DEBUG("received message: ~p", [Msg]),
            ?LOG_DEBUG("JSON message: ~s", [jsone:encode(Msg)]),
            Module:process(maps:get(<<"d">>, Msg)),
            {keep_state, update_seq(Msg, Data0)};
        ?RECONNECT_OP ->
            Data = prepare_reconnect(Data0),
            {next_state, disconnected, Data};
        OpCode -> throw({unexpected_op_code, OpCode, Msg})
    end;
connected(info, Msg, Data) ->
    handle_common(Msg, Data).

% helper methods

handle_down({gun_down, ConnPid, ws, closed, _Remaining},
            Data0=#data{connection=#connection{pid=ConnPid,
                                               host=Host}}) ->
    ?LOG_INFO("~p temporarily disconnected from ~s:~p",
              [ConnPid, Host, ?WSS_PORT]),
    Data = remove_heartbeat(Data0),
    {ok, Protocol} = gun:await_up(ConnPid),
    ?LOG_INFO("~p reconnected to ~s:~p with protocol ~p",
              [ConnPid, Host, ?WSS_PORT, Protocol]),
    Conn = upgrade_ws_connection(Protocol, Data),
    {next_state, await_hello, Data#data{connection=Conn}};
handle_down({gun_down, ConnPid, _Protocol, Err={error, _Error},
               _StreamRefs=[]},
            #data{connection=#connection{pid=ConnPid, host=Host}}) ->
    ?LOG_INFO("~p permanently disconnected from ~s:~p",
              [ConnPid, Host, ?WSS_PORT]),
    throw(Err);
handle_down({gun_ws, ConnPid, StreamRef, {close, Code, <<>>}},
            Data0=#data{connection=#connection{pid=ConnPid,
                                               sref=StreamRef}}) ->
    if Code < 1000 orelse Code > 1001 -> throw({unexpected_close_code, Code});
       true -> ok
    end,
    ?LOG_INFO("received close(~p) message, reconnecting", [Code]),
    Data = prepare_reconnect(Data0),
    {next_state, disconnected, Data}.

await_ws_upgrade(ConnPid) ->
    Path = string:join(["/?v=", ?WEBSOCKET_API_VERSION, "&encoding=",
                        ?RESPONSE_ENCODING], ""),
    ?LOG_INFO("~p connecting to URI ~p", [ConnPid, Path]),
    StreamRef = gun:ws_upgrade(ConnPid, Path),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} -> ok
    after
        ?UPGRADE_TIMEOUT -> throw({timeout, gun_upgrade})
    end,
    ?LOG_INFO("~p upgraded to websockets (~p)", [ConnPid, StreamRef]),
    StreamRef.


handle_common(heartbeat, Data) ->
    send_heartbeat(Data),
    {next_state, await_heartbeat_ack, Data};
handle_common(Msg, Data) ->
    handle_down(Msg, Data).

send_heartbeat(Data=#data{sequence=Seq}) ->
    ?LOG_INFO("sending heartbeat"),
    send_ws_message(?HEARTBEAT_OP, Seq, Data).

build_os() ->
    {Family, Name} = os:type(),
    FamilyBin = atom_to_binary(Family),
    NameBin = atom_to_binary(Name),
    <<FamilyBin/binary, "/", NameBin/binary>>.

send_ws_message(OpCode, D,
                #data{connection=#connection{pid=ConnPid, sref=StreamRef}}) ->
    Msg = #{<<"op">> => OpCode, <<"d">> => D},
    ?LOG_DEBUG("sending message: ~p", [jsone:encode(Msg, [undefined_as_null])]),
    gun:ws_send(ConnPid, StreamRef,
                {text, jsone:encode(Msg, [undefined_as_null])}).

update_seq(#{<<"s">> := null}, Data) -> Data;
update_seq(#{<<"s">> := Seq}, Data) -> Data#data{sequence=Seq}.

update_session_data(Data, MsgData) ->
    SessionId = maps:get(<<"session_id">>, MsgData, Data#data.session_id),
    Resume = maps:get(<<"resume_gateway_url">>, MsgData, Data#data.resume_url),
    Data#data{session_id=SessionId,
              resume_url=Resume
             }.

remove_heartbeat(Data=#data{heartbeat=HeartbeatPid}) ->
    discord_heartbeat:stop(HeartbeatPid),
    ?LOG_INFO("heartbeat removed"),
    Data#data{heartbeat=undefined}.

start_heartbeat(#{<<"heartbeat_interval">> := HeartbeatIV},
                Data=#data{heartbeat=undefined}) ->
    {ok, HeartbeatPid} = discord_heartbeat:start(HeartbeatIV),
    ?LOG_INFO("heartbeat started with interval: ~p", [HeartbeatIV]),
    Data#data{heartbeat=HeartbeatPid}.

prepare_reconnect(Data0=#data{connection=Connection}) ->
    #connection{pid=ConnPid, mref=MRef} = Connection,
    gen_statem:cast(self(), reconnect),
    gun:close(ConnPid),
    Data = remove_heartbeat(Data0),
    gun_util:await_down(ConnPid, MRef),
    ?LOG_INFO("connection successfully closed"),
    Data#data{connection=undefined}.

open_ws_connection(Url) ->
    #{host := GatewayHost} = uri_string:parse(Url),
    {ok, ConnPid} = gun:open(binary_to_list(GatewayHost), ?WSS_PORT,
                             #{protocols => [http]}),
    MRef = monitor(process, ConnPid),
    {ok, Protocol} = gun:await_up(ConnPid),
    ?LOG_INFO("~p connected to ~s:~p with protocol ~p",
              [ConnPid, GatewayHost, ?WSS_PORT, Protocol]),
    StreamRef = await_ws_upgrade(ConnPid),
    #connection{pid=ConnPid, mref=MRef, sref=StreamRef, host=GatewayHost}.

build_resume_message(#data{configuration=Config,
                           session_id=SessionId,
                           sequence=Seq}) ->
    #{<<"token">> => list_to_binary(Config#configuration.bot_token),
      <<"session_id">> => SessionId,
      <<"seq">> => Seq}.

build_identify_message(#data{configuration=Config}) ->
    Properties = #{<<"os">> => build_os(),
                   <<"browser">> => ?LIBRARY_NAME,
                   <<"device">> => ?LIBRARY_NAME
                  },
    #{<<"token">> => list_to_binary(Config#configuration.bot_token),
      <<"properties">> => Properties,
      <<"intents">> => ?INTENTS
     }.

upgrade_ws_connection(Protocol, Data) ->
    ConnPid = Data#data.connection#connection.pid,
    case Protocol of
        ws -> Data#data.connection;
        http ->
            StreamRef = await_ws_upgrade(ConnPid),
            Data#data.connection#connection{sref=StreamRef};
        http2 ->
            StreamRef = await_ws_upgrade(ConnPid),
            Data#data.connection#connection{sref=StreamRef};
        _ -> throw({unsupported_protocol, Protocol})
    end.

ws_close(Code, Msg, #data{connection=#connection{pid=ConnPid, sref=SRef}}) ->
    gun:ws_send(ConnPid, SRef, {close, Code, Msg}).
