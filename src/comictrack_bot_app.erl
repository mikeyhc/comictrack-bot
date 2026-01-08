%%%-------------------------------------------------------------------
%% @doc comictrack_bot public API
%% @end
%%%-------------------------------------------------------------------

-module(comictrack_bot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    RequiredEnv = ["DISCORD_BOT_TOKEN"],
    Lookup = fun(EnvVar, Map) ->
                     case os:getenv(EnvVar) of
                         false -> error({missing_envvar, EnvVar});
                         Val -> Map#{EnvVar => Val}
                     end
             end,
    Env = lists:foldl(Lookup, #{}, RequiredEnv),
    #{
      "DISCORD_BOT_TOKEN" := DiscordBotToken
     } = Env,
    comictrack_bot_sup:start_link(DiscordBotToken).

stop(_State) ->
    ok.

%% internal functions
