-module(comicvine_db).

% Public API
-export([install/1]).
-export([store_volume/1, get_volumes/0, get_volume/1]).
-export([store_issue/1, get_issues/0]).

-define(MATCH_CUTOFF, 0.7).

-record(comicvine_volume, {id :: non_neg_integer(),
                           name :: binary(),
                           response :: #{},
                           last_updated :: non_neg_integer()
                          }).

-record(comicvine_issue, {id :: non_neg_integer(),
                          volume_id :: non_neg_integer(),
                          response :: #{},
                          last_updated :: non_neg_integer()
                         }).


%% Public API

-spec install([node()]) -> ok.
install(Nodes) ->
    Tables = #{
        comicvine_volume => [{attributes,
                              record_info(fields, comicvine_volume)},
                             {index, [#comicvine_volume.name]},
                             {disc_copies, Nodes}],
        comicvine_issue => [{attributes,
                             record_info(fields, comicvine_issue)},
                            {index, [#comicvine_issue.volume_id]},
                            {disc_copies, Nodes}]
    },
    db_utils:install(Nodes, Tables).

-spec store_volume(#{}) -> ok.
store_volume(VolumeResponse=#{<<"name">> := Name, <<"id">> := Id}) ->
    Record = #comicvine_volume{id=Id,
                               name=string:casefold(Name),
                               response=VolumeResponse,
                               last_updated=erlang:system_time(seconds)
                              },
    mnesia:activity(transaction, fun() -> mnesia:write(Record) end).

-spec get_volume(Filter) -> [#{}]
    when Filter :: #{id => non_neg_integer(), name => binary()}.
get_volume(Filter) ->
    case {maps:get(id, Filter, undefined), maps:get(name, Filter, undefined)} of
        {undefined, undefined} -> throw(filter_required);
        {Id, undefined} -> get_volume_by_id(Id);
        {undefined, Name} -> get_volume_by_name(Name);
        {_, _} -> throw({conflicting_filters, [id, name]})
    end.

-spec get_volumes() -> [#{}].
get_volumes() ->
    lists:map(fun volume_response/1, get_all(comicvine_volume)).

-spec store_issue(#{}) -> ok.
store_issue(IssueResponse=#{<<"id">> := Id,
                            <<"volume">> := #{<<"id">> := VolumeId}}) ->
    Record = #comicvine_issue{id=Id,
                              volume_id=VolumeId,
                              response=IssueResponse,
                              last_updated=erlang:system_time(seconds)
                             },
    mnesia:activity(transaction, fun() -> mnesia:write(Record) end).

-spec get_issues() -> [#{}].
get_issues() ->
    lists:map(fun issue_response/1, get_all(comicvine_issue)).

%% helper functions

volume_response(#comicvine_volume{response=R, last_updated=LU}) ->
    R#{'_last_updated' => LU}.

issue_response(#comicvine_issue{response=R, last_updated=LU}) ->
    R#{'_last_updated' => LU}.

get_volume_by_id(Id) ->
    F = fun() ->
        case mnesia:read({comicvine_volume, Id}) of
            [] -> {error, not_found};
            [V] -> {ok, volume_response(V)};
            Res ->
                Data = lists:map(fun volume_response/1, Res),
                {error, {multiple_results, Data}}
        end
    end,
    mnesia:activity(transaction, F).

get_volume_by_name(Name) ->
    SearchName = string:casefold(Name),
    F = fun() ->
        case mnesia:index_read(comicvine_volume, SearchName,
                               #comicvine_volume.name) of
            [] -> {error, not_found};
            [V] -> {ok, volume_response(V)};
            Res ->
                Data = lists:map(fun volume_response/1, Res),
                {error, {multiple_results, Data}}
        end
    end,
    case mnesia:activity(transaction, F) of
        {error, not_found} -> get_volume_by_fuzzy_name(Name);
        Response -> Response
    end.

build_fuzzymatcher(Target) ->
    SearchName = string:casefold(Target),
    fun(Str) ->
        S = string:casefold(Str),
        case string:find(S, SearchName) of
            nomatch -> string:jaro_similarity(S, SearchName) > ?MATCH_CUTOFF;
            _Match -> true
        end
    end.

get_volume_by_fuzzy_name(Name) ->
    Matcher = build_fuzzymatcher(Name),
    case lists:filter(fun(#{<<"name">> := N}) -> Matcher(N) end,
                      get_volumes()) of
        [] -> {error, not_found};
        [V] -> {ok, V};
        Res -> {error, {multiple_results, Res}}
    end.

get_all(TabName) ->
    F = fun() -> mnesia:foldl(fun(V, Acc) -> [V|Acc] end, [], TabName) end,
    mnesia:activity(transaction, F).
