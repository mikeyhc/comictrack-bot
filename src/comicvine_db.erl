-module(comicvine_db).

-include_lib("kernel/include/logger.hrl").

% Public API
-export([install/1]).
-export([store_volume/1, get_volumes/0, get_volume/1]).

-define(MATCH_CUTOFF, 0.7).

-record(comicvine_volume, {id :: non_neg_integer(),
                           name :: binary(),
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
                             {disc_copies, Nodes}]
    },
    case mnesia:create_schema(Nodes) of
        {atomic, ok} -> ok;
        {error, {_, {already_exists, _}}} -> ok
    end,
    {_, []} = rpc:multicall(Nodes, application, start, [mnesia]),
    lists:foreach(fun({Name, Config}) ->
            case mnesia:create_table(Name, Config) of
                {atomic, ok} ->
                    ?LOG_INFO("database ~p created", [Name]);
                {aborted, {already_exists, Name}} ->
                    ?LOG_INFO("database ~p already exists", [Name])
            end
        end, maps:to_list(Tables)),
    {_, []} = rpc:multicall(Nodes, application, stop, [mnesia]),
    ok.

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
    case {maps:get(id, Filter), maps:get(name, Filter)} of
        {undefined, undefined} -> throw(filter_required);
        {Id, undefined} -> get_volume_by_id(Id);
        {undefined, Name} -> get_volume_by_name(Name);
        {_, _} -> throw({conflicting_filters, [id, name]})
    end.

-spec get_volumes() -> [#{}].
get_volumes() -> [].

%% helper functions

get_volume_by_id(Id) ->
    F = fun() ->
        case mnesia:read({comicvine_volume, Id}) of
            [] -> {error, not_found};
            [V] -> {ok, V#comicvine_volume.response};
            Res ->
                Data = lists:map(fun(V) -> V#comicvine_volume.response end,
                                 Res),
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
            [V] -> {ok, V#comicvine_volume.response};
            Res ->
                Data = lists:map(fun(V) -> V#comicvine_volume.response end,
                                 Res),
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
        case string:find(Str, SearchName) of
            nomatch -> string:jaro_similarity(Str, SearchName) > ?MATCH_CUTOFF;
            _Match -> true
        end
    end.

get_volume_by_fuzzy_name(Name) ->
    case lists:filter(build_fuzzymatcher(Name), get_volumes()) of
        [] -> {error, not_found};
        [V] -> {ok, V#comicvine_volume.response};
        Res ->
            Data = lists:map(fun(V) -> V#comicvine_volume.response end,
                             Res),
            {error, {multiple_results, Data}}
    end.
