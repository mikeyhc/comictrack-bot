-module(comic_repository).

-include_lib("kernel/include/logger.hrl").

-export([get_volume/1, get_volume/2]).

-spec get_volume(Filter) -> {ok, #{}} | {error, not_found} |
                            {error, {multiple_results, [#{}]}}
    when Filter :: #{id => non_neg_integer(), name => binary()}.
get_volume(Filter) ->
    get_volume(Filter, true).


-spec get_volume(Filter, boolean()) -> {ok, #{}} | {error, not_found} |
                                       {error, {multiple_results, [#{}]}}
    when Filter :: #{id => non_neg_integer(), name => binary()}.
get_volume(Filter, true) ->
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
            ApiFilter = lists:map(fun({K, V}) when is_binary(V)->
                                          {atom_to_list(K), binary_to_list(V)};
                                     ({K, V}) when is_integer(V) ->
                                          {atom_to_list(K), integer_to_list(V)}
                                  end, maps:to_list(Filter)),
            {ok, Reply} = comicvine_api:volumes(ApiFilter),
            case maps:get(<<"results">>, Reply) of
                [] -> {error, not_found};
                [V] -> comicvine_updater:fetch_volume_from_api(V);
                Volumes -> {error, {multiple_results, Volumes}}
            end
    end;
get_volume(Filter, false) ->
    ApiFilter = lists:map(fun({K, V}) ->
                                  {atom_to_list(K), binary_to_list(V)}
                          end, maps:to_list(Filter)),
    {ok, Reply} = comicvine_api:volumes(ApiFilter),
    case maps:get(<<"results">>, Reply) of
        [] -> {error, not_found};
        [V] -> comicvine_updater:fetch_volume_from_api(V);
        Volumes -> {error, {multiple_results, Volumes}}
    end.
