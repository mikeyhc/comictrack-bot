-module(discord_heartbeat).

-export([start/2, stop/1]).

start(IV, Data) ->
    CallerPid = self(),
    Fun = fun() -> CallerPid ! {heartbeat, Data, make_ref()} end,
    {ok, {interval, Ref}} = timer:apply_interval(IV, Fun),
    {ok, Ref}.

stop(Ref) ->
    timer:cancel(Ref).
