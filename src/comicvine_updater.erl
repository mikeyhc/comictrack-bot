-module(comicvine_updater).

-export([fetch_volume_from_api/1]).

fetch_volume_from_api(#{<<"id">> := Id}) ->
    fetch_volume_from_api(Id);
fetch_volume_from_api(Id) ->
    {ok, VolumeReply} = comicvine_api:volume(Id),
    {ok, IssueReply} = comicvine_api:issues([{"volume", integer_to_list(Id)}]),
    Volume = maps:get(<<"results">>, VolumeReply),
    Issues = maps:get(<<"results">>, IssueReply),
    comicvine_db:store_volume(Volume),
    lists:foreach(fun comicvine_db:store_issue/1, Issues),
    {ok, Volume}.
