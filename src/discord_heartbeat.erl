-module(discord_heartbeat).

-export([start/1]).

start(IV) ->
    CallerPid = self(),
    timer:apply_repeatedly(IV, fun() -> CallerPid ! heartbeat end).
