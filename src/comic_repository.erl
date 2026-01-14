-module(comic_repository).

-include_lib("kernel/include/logger.hrl").

-export([get_volume/1, lookup_volume/1, get_new_issues/0]).

% this is less than 7 as its a lookback window (not the actual days)
-define(ONE_WEEK, 6).


-spec get_volume(Filter) -> {ok, #{}} | {error, not_found} |
                            {error, {multiple_results, [#{}]}}
    when Filter :: #{id => non_neg_integer(), name => binary()}.
get_volume(Filter) ->
    case comicvine_db:get_volume(Filter) of
        {ok, Reply} ->
            ?LOG_DEBUG("successful cache hit for ~p~n",
                       [maps:get(<<"name">>, Reply)]),
            {ok, Reply};
        {error, {multiple_results, Results}} ->
            ?LOG_DEBUG("multiple cache (~p) results for ~p",
                       [length(Results), Filter]),
            {error, {multiple_results, Results}};
        {error, not_found} ->
            ?LOG_DEBUG("cache miss for ~p", [Filter]),
            {error, not_found}
    end.


-spec lookup_volume(Filter) -> {ok, #{}} | {error, not_found} |
                               {error, {multiple_results, [#{}]}}
    when Filter :: #{id => non_neg_integer(), name => binary()}.
lookup_volume(Filter) ->
    ApiFilter = lists:map(fun({K, V}) ->
                                  {atom_to_list(K), standardize_args(V)}
                          end, maps:to_list(Filter)),
    {ok, Reply} = comicvine_api:volumes(ApiFilter),
    case maps:get(<<"results">>, Reply) of
        [] -> {error, not_found};
        [V] -> comicvine_updater:fetch_volume_from_api(V);
        Volumes -> {error, {multiple_results, Volumes}}
    end.

standardize_args(A) when is_integer(A) -> integer_to_list(A);
standardize_args(A) when is_binary(A) -> binary_to_list(A);
standardize_args(A) -> A.

-spec get_new_issues() -> [#{}].
get_new_issues() ->
    ComicVineIssues = comicvine_db:get_issues(),
    lists:filter(fun is_recent/1, ComicVineIssues).

is_recent(#{<<"store_date">> := StoreDateBin}) when StoreDateBin =/= null ->
    [StrY, StrM, StrD] = string:tokens(binary_to_list(StoreDateBin), "-"),
    StoreDate = {list_to_integer(StrY), list_to_integer(StrM),
                 list_to_integer(StrD)},
    StoreDays = calendar:date_to_gregorian_days(StoreDate),
    {NowDate, _} = calendar:local_time(),
    NowDays = calendar:date_to_gregorian_days(NowDate),
    NowDays - StoreDays =< ?ONE_WEEK;
is_recent(_Issue) -> false.
