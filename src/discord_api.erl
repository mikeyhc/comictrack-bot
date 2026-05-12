-module(discord_api).

-include_lib("kernel/include/logger.hrl").
-include("types.hrl").

% Public API
-export([start_link/1]).
-export([get_gateway_bot/0]).
-export([register_command/2, interaction_callback/3, interaction_update/4]).
-export([direct_message/2]).

-define(SERVER_NAME, ?MODULE).
-define(DISCORD_HOST, "discord.com").
-define(DISCORD_PORT, 443).
-define(DISCORD_API_VERSION, "v10").
-define(ACCEPT_STRING, "application/json").

% Public API
-spec start_link(string()) -> {ok, pid()}.
start_link(BotToken) ->
    Configuration = #{
        host => ?DISCORD_HOST,
        port => ?DISCORD_PORT,
        headers =>[
            {<<"accept">>, ?ACCEPT_STRING},
            {<<"authorization">>, "Bot " ++ BotToken}
        ]
    },
    http_client:start_link(?SERVER_NAME, Configuration).

-spec get_gateway_bot() -> {ok, #{}}.
get_gateway_bot() ->
    {ok, {200, Body}} = http_client:get(?SERVER_NAME,
                                        build_url("/gateway/bot", [])),
    {ok, json:decode(Body)}.

-spec register_command(non_neg_integer(), #{}) -> {ok, #{}}.
register_command(ApplicationId, Command) ->
    Url = build_url("/applications/~s/commands", [ApplicationId]),
    post_api_call(Url, jsone:encode(Command)).

-spec interaction_callback(iolist(), iolist(), #{}) -> ok.
interaction_callback(InteractionId, InteractionToken, Body) ->
    Url = build_url("/interactions/~s/~s/callback",
                    [InteractionId, InteractionToken]),
    ok = post_api_call(Url, jsone:encode(Body)),
    ok.

-spec interaction_update(iolist(), iolist(), iolist(), #{}) -> ok.
interaction_update(ApplicationId, InteractionToken, MessageId, Body) ->
    Url = build_url("/webhooks/~s/~s/messages/~s",
                    [ApplicationId, InteractionToken, MessageId]),
    ok = patch_api_call(Url, jsone:encode(Body)),
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

% helper functions

write_headers() ->
    [{<<"content-type">>, <<"application/json">>}].


build_url(Template, Args) ->
    Full = "/api/" ++ ?DISCORD_API_VERSION ++ Template,
    lists:flatten(io_lib:format(Full, Args)).

post_api_call(Url, Body) ->
    Options = #{headers => write_headers()},
    {ok, {Code, Reply}} = http_client:post(?SERVER_NAME, Url, Body,
                                           Options),
    ?LOG_DEBUG("~p recevied ~p", [Url, Code]),
    if Code >= 200 andalso Code < 300 ->
           case Reply of
               no_data -> ok;
               _ -> {ok, jsone:decode(Reply)}
           end;
       true -> {error, {invalid_code, Code, Reply}}
    end.

patch_api_call(Url, Body) ->
    Options = #{headers => write_headers()},
    {ok, {Code, Reply}} = http_client:patch(?SERVER_NAME, Url, Body,
                                            Options),
    ?LOG_DEBUG("~p recevied ~p", [Url, Code]),
    if Code >= 200 andalso Code < 300 ->
           case Reply of
               no_data -> ok;
               _ -> {ok, jsone:decode(Reply)}
           end;
       true -> {error, {invalid_code, Code, Reply}}
    end.
