%%%-------------------------------------------------------------------
%% @doc comictrack_bot public API
%% @end
%%%-------------------------------------------------------------------

-module(comictrack_bot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    comictrack_bot_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
