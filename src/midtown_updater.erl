-module(midtown_updater).

-export([fetch_volume_from_api/1]).

fetch_volume_from_api(_Name) ->
    {ok, Content} = file:read_file("roots-of-madness.html"),
    {Volume, Issues} = midtown_parser:parse(Content),
    midtown_db:store_volume(Volume),
    lists:foreach(fun midtown_db:store_issue/1, Issues),
    {ok, Volume}.
