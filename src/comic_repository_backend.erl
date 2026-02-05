-module(comic_repository_backend).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{get_volume, 1},
     {get_issues, 0},
     {lookup_volume, 1},
     {fetch_volume, 1}
     ].
