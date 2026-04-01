-module(gun_util).

-include_lib("kernel/include/logger.hrl").

-export([handle_down/4, await_down/2]).

-spec handle_down(pid(), any(), iolist(), iolist()) -> connected | disconnected.
handle_down(ConnPid, Reason, Host, Port) ->
    case temporary_reason(Reason) of
        true ->
            ?LOG_INFO("~p temporarily disconnected from ~s:~p",
                      [ConnPid, Host, Port]),
            {ok, Protocol} = gun:await_up(ConnPid),
            ?LOG_INFO("~p reconnected to ~s:~p with protocol ~p",
              [ConnPid, Host, Port, Protocol]),
            connected;
        false ->
            ?LOG_INFO("~p disconnected from ~s:~p: ~p",
                      [ConnPid, Host, Port, Reason]),
            disconnected
    end.

-spec await_down(pid(), reference()) -> ok.
await_down(ConnPid, MRef) ->
    receive
        {'DOWN', MRef, process, ConnPid, shutdown} -> ok
    after 1000 -> ?LOG_ERROR("didn't receive down message")
    end.

temporary_reason(normal) -> true;
temporary_reason({error, closesd}) -> true;
temporary_reason({error, einval}) -> true;
temporary_reason(_Reason) -> false.
