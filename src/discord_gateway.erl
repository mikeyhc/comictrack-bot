-module(discord_gateway).
-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").
-include("types.hrl").

% Public API
-export([start_link/1]).

% gen_statem callbacks
-export([callback_mode/0, init/1]).
-export([disconnected/3, await_hello/3, await_ready/3]).

-define(WSS_PORT, 443).
-define(WEBSOCKET_API_VERSION, "10").
-define(RESPONSE_ENCODING, "json").
-define(SERVER_NAME, ?MODULE).
-define(UPGRADE_TIMEOUT, 5000).
-define(LIBRARY_NAME, <<"comictrack/1.0">>).
-define(INTENTS, 2048).

% op codes
-define(MESSAGE_OP, 0).
-define(HEARTBEAT_OP, 1).
-define(IDENTIFY_OP, 2).
-define(HELLO_OP, 10).
-define(HEARTBEAT_ACK_OP, 11).


-record(connection, {pid  :: pid(),
                     mref :: reference(),
                     sref :: reference(),
                     host :: binary()
                    }).

-record(configuration, {bot_token :: string() | binary()}).

-record(data, {connection    :: optional(#connection{}),
               hearbeat      :: optional(reference()),
               sequence      :: optional(non_neg_integer()),
               configuration :: #configuration{}
               }).

% Public API
-spec start_link(string() | binary()) -> {ok, pid()}.
start_link(BotToken) ->
    gen_statem:start_link({local, ?SERVER_NAME}, ?MODULE, [BotToken], []).

% gen_statem callbacks
callback_mode() -> state_functions.

init([BotToken]) ->
    gen_statem:cast(self(), connect),
    {ok, disconnected, #data{configuration=#configuration{bot_token=BotToken}}}.

disconnected(cast, connect, Data) ->
    {ok, Gateway} = discord_api:get_gateway_bot(),
    #{<<"url">> := Url} = Gateway,
    #{host := GatewayHost} = uri_string:parse(Url),
    {ok, ConnPid} = gun:open(binary_to_list(GatewayHost), ?WSS_PORT,
                             #{protocols => [http]}),
    MRef = monitor(process, ConnPid),
    {ok, Protocol} = gun:await_up(ConnPid),
    ?LOG_INFO("~p connected to ~s:~p with protocol ~p",
              [ConnPid, GatewayHost, ?WSS_PORT, Protocol]),
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
    Connection = #connection{pid=ConnPid, mref=MRef, sref=StreamRef,
                             host=GatewayHost},
    {next_state, await_hello, Data#data{connection=Connection}}.

await_hello(info, {gun_ws, ConnPid, StreamRef, {text, Msg}},
            Data=#data{connection=#connection{pid=ConnPid, sref=StreamRef},
                       configuration=Config}) ->
    #{<<"op">> := ?HELLO_OP, <<"d">> := D} = jsone:decode(Msg),
    #{<<"heartbeat_interval">> := HeartbeatIV} = D,
    HeartbeatPid = discord_heartbeat:start(HeartbeatIV),
    ?LOG_INFO("heartbeat started with interval: ~p", [HeartbeatIV]),
    Properties = #{<<"os">> => build_os(),
                   <<"browser">> => ?LIBRARY_NAME,
                   <<"device">> => ?LIBRARY_NAME
                  },
    Identify = #{<<"token">> => list_to_binary(Config#configuration.bot_token),
                 <<"properties">> => Properties,
                 <<"intents">> => 0
                },
    send_ws_message(?IDENTIFY_OP, Identify, Data),
    {next_state, await_ready, Data#data{hearbeat=HeartbeatPid}};
await_hello(info, Msg, Data) ->
    handle_common(Msg, Data).

await_ready(info, {gun_ws, ConnPid, StreamRef, {text, Msg}},
            Data=#data{connection=#connection{pid=ConnPid, sref=StreamRef}}) ->
    #{<<"op">> := Op, <<"s">> := Seq0} = jsone:decode(Msg),
    case Op of
        ?MESSAGE_OP ->
            ?LOG_INFO("discord gateway is now connected");
        ?HEARTBEAT_ACK_OP ->
            ?LOG_INFO("receive heartbeat ack")
    end,
    Seq1 = if Seq0 =:= null -> Data#data.sequence;
              true -> Seq0
           end,
    {keep_state, Data#data{sequence=Seq1}};
await_ready(info, Msg, Data) ->
    handle_common(Msg, Data).

% helper methods

handle_common({gun_down, ConnPid, http, closed, _Remaining},
              Data=#data{connection=#connection{pid=ConnPid, host=Host}}) ->
    ?LOG_INFO("~p temporarily disconnected from ~s:~p",
              [ConnPid, Host, ?WSS_PORT]),
    {ok, Protocol} = gun:await_up(ConnPid),
    ?LOG_INFO("~p reconnected to ~s:~p with protocol ~p",
              [ConnPid, Host, ?WSS_PORT, Protocol]),
    {keep_state, Data};
handle_common({gun_down, ConnPid, _Protocol, Err={error, _Error},
               _StreamRefs=[]},
            #data{connection=#connection{pid=ConnPid, host=Host}}) ->
    ?LOG_INFO("~p permanently disconnected from ~s:~p",
              [ConnPid, Host, ?WSS_PORT]),
    throw(Err);
handle_common(heartbeat, Data) ->
    send_heartbeat(Data),
    {keep_state, Data}.

send_heartbeat(Data=#data{sequence=Seq}) ->
    send_ws_message(?HEARTBEAT_OP, Seq, Data).

build_os() ->
    {Family, Name} = os:type(),
    FamilyBin = atom_to_binary(Family),
    NameBin = atom_to_binary(Name),
    <<FamilyBin/binary, "/", NameBin/binary>>.

send_ws_message(OpCode, D,
                #data{connection=#connection{pid=ConnPid, sref=StreamRef}}) ->
    Msg = #{<<"op">> => OpCode, <<"d">> => D},
    ?LOG_INFO("sending message: ~p", [jsone:encode(Msg, [undefined_as_null])]),
    gun:ws_send(ConnPid, StreamRef,
                {text, jsone:encode(Msg, [undefined_as_null])}).
