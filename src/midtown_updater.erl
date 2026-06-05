-module(midtown_updater).

-export([fetch_volume_from_api/1, fetch_all_volumes_from_api/0]).

tracked_volumes() ->
    ["we don't kill spiders: season of the witch",
     "the matron IPI"].

-spec fetch_all_volumes_from_api() -> [{ok, #{}}].
fetch_all_volumes_from_api() ->
    lists:map(fun fetch_volume_from_api/1, tracked_volumes()).

fetch_volume_from_api(Name) ->
    {ok, Content} = midtown_api:volume_search(Name),
    {Volume, Issues} = midtown_parser:parse(Content),
    midtown_db:store_volume(Volume),
    lists:foreach(fun midtown_db:store_issue/1, Issues),
    {ok, Volume}.
