-module(comicvine_backend).
-behaviour(comic_repository_backend).

-export([get_volume/1, get_issues/0, lookup_volume/1, fetch_volume/1]).

get_volume(Filter) -> comicvine_db:get_volume(Filter).

get_issues() -> comicvine_db:get_issues().

lookup_volume(Filter) ->
    ApiFilter = lists:map(fun({K, V}) ->
                                  {atom_to_list(K), standardize_args(V)}
                          end, maps:to_list(Filter)),
    {ok, Reply} = comicvine_api:volumes(ApiFilter),
    maps:get(<<"results">>, Reply).

fetch_volume(#{id := Id}) ->
    comicvine_updater:fetch_volume_from_api(#{<<"id">> => Id});
fetch_volume(Volume) ->
    comicvine_updater:fetch_volume_from_api(Volume).

standardize_args(A) when is_integer(A) -> integer_to_list(A);
standardize_args(A) when is_binary(A) -> binary_to_list(A);
standardize_args(A) -> A.
