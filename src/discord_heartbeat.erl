-module(discord_heartbeat).

-export([start/1]).

start(IV) ->
    CallerPid = self(),
    Fun = fun() -> CallerPid ! heartbeat end,
    {ok, {interval, Ref}} = timer:apply_interval(IV, Fun),
    {ok, Ref}.
