-module(midtown_backend).
-behaviour(comic_repository_backend).

-export([get_volume/1, get_issues/0, lookup_volume/1, fetch_volume/1]).

get_volume(Filter) -> midtown_db:get_volume(Filter).

get_issues() -> midtown_db:get_issues().

lookup_volume(Filter) ->
    case midtown_db:get_volume(Filter) of
        {error, not_found} -> [];
        {ok, Volume} -> [Volume];
        {error, {multiple_results, Volumes}} -> Volumes
    end.

fetch_volume(#{<<"id">> := Id}) -> midtown_db:get_volume(#{id => Id});
fetch_volume(Id) when is_integer(Id) -> midtown_db:get_volume(#{id => Id}).
