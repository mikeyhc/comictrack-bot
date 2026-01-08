%%%-------------------------------------------------------------------
%% @doc comictrack_bot public API
%% @end
%%%-------------------------------------------------------------------

-module(comictrack_bot_app).

-behaviour(application).

-export([start/2, stop/1, install/3]).

start(_StartType, _StartArgs) ->
    RequiredEnv = ["DISCORD_BOT_TOKEN", "COMICVINE_API_KEY"],
    Lookup = fun(EnvVar, Map) ->
                     case os:getenv(EnvVar) of
                         false -> error({missing_envvar, EnvVar});
                         Val -> Map#{EnvVar => Val}
                     end
             end,
    Env = lists:foldl(Lookup, #{}, RequiredEnv),
    #{
      "DISCORD_BOT_TOKEN" := DiscordBotToken,
      "COMICVINE_API_KEY" := ComicvineApiKey
     } = Env,
    comictrack_bot_sup:start_link(DiscordBotToken, ComicvineApiKey).

stop(_State) ->
    ok.

install(Nodes, DiscordAppId, DiscordBotToken) ->
    DBs = [comicvine_db, user_db],
    lists:foreach(fun(DB) -> DB:install(Nodes) end, DBs),
    discord_commands:install(DiscordAppId, DiscordBotToken).

%% internal functions
