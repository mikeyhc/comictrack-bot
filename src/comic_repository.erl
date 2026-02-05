-module(comic_repository).

-include_lib("kernel/include/logger.hrl").

% Public API
-export([get_volume/1, lookup_volume/1, get_new_issues/0]).

% this is less than 7 as its a lookback window (not the actual days)
-define(ONE_WEEK, 6).

-type filter() :: #{id => non_neg_integer(), name => binary()}.

backends() -> [{<<"cv">>, comicvine_backend},
               {<<"mt">>, midtown_backend}
              ].

%% Public API

-spec get_volume(Filter) -> {ok, #{}} | {error, not_found} |
                            {error, {multiple_results, [#{}]}}
    when Filter :: filter().
get_volume(#{id := _Id, name := _Name}) ->
    throw({conflicting_filters, [id, name]});
get_volume(#{id := FullId}) ->
    [Code, IdBin] = binary:split(FullId, <<"-">>),
    case lists:keyfind(Code, 1, backends()) of
        false -> throw({invalid_code, Code});
        {Code, Backend} ->
            Id = binary_to_integer(IdBin),
            {ok, Volume} = Backend:get_volume(#{id => Id}),
            {ok, add_code_to_volume(Code, Volume)}
    end;
get_volume(Filter) ->
    Fun = fun(Backend, Acc) -> get_volume_fold(Filter, Backend, Acc) end,
    case lists:foldl(Fun, [], backends()) of
        [] -> {error, not_found};
        [Reply] -> {ok, Reply};
        Results -> {error, {multiple_results, Results}}
    end.

get_volume_fold(Filter, {Code, Backend}, Acc) ->
    case Backend:get_volume(Filter) of
        {ok, Reply} ->
            [add_code_to_volume(Code, Reply)|Acc];
        {error, {multiple_results, Res}} ->
            Volumes = lists:map(fun(V) -> add_code_to_volume(Code, V) end, Res),
            Volumes ++ Acc;
        {error, not_found} -> Acc
    end.


-spec lookup_volume(Filter) -> {ok, #{}} | {error, not_found} |
                               {error, {multiple_results, [#{}]}}
    when Filter :: filter().
lookup_volume(#{id := _Id, name := _Name}) ->
    throw({conflicting_filters, [id, name]});
lookup_volume(#{id := FullId}) ->
    [Code, IdBin] = binary:split(FullId, <<"-">>),
    case lists:keyfind(Code, 1, backends()) of
        false -> throw({invalid_code, Code});
        {Code, Backend} ->
            Id = binary_to_integer(IdBin),
            {ok, Volume} = Backend:fetch_volume(#{id => Id}),
            {ok, add_code_to_volume(Code, Volume)}
    end;
lookup_volume(Filter) ->
    Fun = fun(Backend, Acc) -> lookup_volume_fold(Filter, Backend, Acc) end,
    case lists:foldl(Fun, [], backends()) of
        [] -> {error, not_found};
        [Reply] ->
            {Code, IdBin} = determine_volume_code(Reply),
            {_, Backend} = lists:keyfind(Code, 1, backends()),
            {ok, Volume} = Backend:fetch_volume(binary_to_integer(IdBin)),
            {ok, add_code_to_volume(Code, Volume)};
        Results -> {error, {multiple_results, Results}}
    end.

lookup_volume_fold(Filter, {Code, Backend}, Acc) ->
    case Backend:lookup_volume(Filter) of
        [] -> Acc;
        [V] -> [add_code_to_volume(Code, V)|Acc];
        Volumes ->
            lists:map(fun(V) -> add_code_to_volume(Code, V) end, Volumes) ++ Acc
    end.

-spec get_new_issues() -> [#{}].
get_new_issues() ->
    Builder = fun({Code, Backend}, Acc) ->
                      Issues0 = Backend:get_issues(),
                      Issues = lists:map(fun(I) ->
                                                 add_code_to_issue(Code, I)
                                         end, Issues0),
                      Issues ++ Acc
              end,
    ComicIssues = lists:foldl(Builder, [], backends()),
    io:format("~p~n", [length(ComicIssues)]),
    lists:filter(fun is_recent/1, ComicIssues).

%% helper methods

is_recent(#{<<"store_date">> := StoreDateBin}) when StoreDateBin =/= null ->
    [StrY, StrM, StrD] = string:tokens(binary_to_list(StoreDateBin), "-"),
    StoreDate = {list_to_integer(StrY), list_to_integer(StrM),
                 list_to_integer(StrD)},
    StoreDays = calendar:date_to_gregorian_days(StoreDate),
    {NowDate, _} = calendar:local_time(),
    NowDays = calendar:date_to_gregorian_days(NowDate),
    NowDays - StoreDays =< ?ONE_WEEK;
is_recent(#{<<"date_added">> := AddedBin}) ->
    Rfc339Time = binary_to_list(<<AddedBin/binary, "Z">>),
    SystemTime = calendar:rfc3339_to_system_time(Rfc339Time),
    {Date, _Time} = calendar:system_time_to_universal_time(SystemTime, seconds),
    AddedDays = calendar:date_to_gregorian_days(Date),
    {NowDate, _} = calendar:local_time(),
    NowDays = calendar:date_to_gregorian_days(NowDate),
    NowDays - AddedDays =< ?ONE_WEEK.

add_code(Code, Id) ->
    BinId = integer_to_binary(Id),
    <<Code/binary, "-", BinId/binary>>.

add_code_to_volume(Code, V=#{<<"id">> := Id,
                             <<"issues">> := Issues0}) ->
    Issues = lists:map(fun(I=#{<<"id">> := IssueId}) ->
                               I#{<<"id">> => add_code(Code, IssueId)}
                       end, Issues0),
    V#{<<"id">> => add_code(Code, Id), <<"issues">> => Issues};
add_code_to_volume(Code, V=#{<<"id">> := Id}) ->
    V#{<<"id">> => add_code(Code, Id)}.

add_code_to_issue(Code, I=#{<<"id">> := IssueId,
                            <<"volume">> := V}) ->
    #{<<"id">> := VolumeId} = V,
    I#{<<"id">> => add_code(Code, IssueId),
       <<"volume">> => V#{<<"id">> => add_code(Code, VolumeId)}}.

determine_volume_code(#{<<"id">> := Id}) ->
    [Code, Rest] = binary:split(Id, <<"-">>),
    {Code, Rest}.
