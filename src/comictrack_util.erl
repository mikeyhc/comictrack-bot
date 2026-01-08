-module(comictrack_util).

-include_lib("kernel/include/logger.hrl").

-export([sync/0]).

sync() ->
    Results = lists:map(fun comicvine_updater:fetch_volume_from_api/1,
                        user_db:get_all_volumes()),
    {Statuses, _Values} = lists:unzip(Results),
    true = lists:all(fun(V) -> V =:= ok end, Statuses),
    ?LOG_INFO("sync successful").
