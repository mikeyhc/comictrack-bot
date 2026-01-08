-module(discord_gateway).
-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").
-include("types.hrl").

% Public API
-export([start_link/1]).

% gen_statem callbacks
-export([callback_mode/0, init/1]).
-export([disconnected/3, awaiting_hello/3]).

-define(WSS_PORT, 443).
-define(WEBSOCKET_API_VERSION, "10").
-define(RESPONSE_ENCODING, "json").
-define(SERVER_NAME, ?MODULE).

-record(connection, {pid  :: pid(),
                     mref :: reference(),
                     sref :: reference()
                    }).

-record(configuration, {bot_token :: string() | binary()}).

-record(data, {connection    :: optional(#connection{}),
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
    ?LOG_INFO("~p upgraded to websockets (~p)", [ConnPid, StreamRef]),
    Connection = #connection{pid=ConnPid, mref=MRef, sref=StreamRef},
    {next_state, awaiting_hello, Data#data{connection=Connection}}.

awaiting_hello(Source, Msg, Data) ->
    ?LOG_INFO("received ~p ~p", [Source, Msg]),
    {keep_state, Data}.
