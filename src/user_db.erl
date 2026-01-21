-module(user_db).

% Public API
-export([install/1]).
-export([add_user_volume/2, get_user_volumes/1, get_all_volumes/0,
         get_all_users_with_volumes/0]).
-export([add_user_issue/3, remove_user_issue/3, get_user_issues/1]).

-record(user_volume, {user_and_volume_id     :: {binary(), non_neg_integer()},
                      read_issues=sets:new() :: sets:set()
                     }).

-spec install([node()]) -> ok.
install(Nodes) ->
    Tables = #{
        user_volume => [{attributes,
                         record_info(fields, user_volume)},
                        {type, set},
                        {disc_copies, Nodes}]
    },
    db_utils:install(Nodes, Tables).

%% volume functions
-spec add_user_volume(binary(), non_neg_integer()) -> ok.
add_user_volume(UserId, VolumeId) ->
    Record = #user_volume{user_and_volume_id={UserId, VolumeId}},
    mnesia:activity(transaction, fun() -> mnesia:write(Record) end).

-spec get_user_volumes(binary()) -> [non_neg_integer()].
get_user_volumes(UserId) ->
    Fun = fun() ->
                  mnesia:foldl(
                    fun(#user_volume{user_and_volume_id={U, V}}, Acc)
                          when UserId =:= U ->
                            [V|Acc];
                       (_, Acc) -> Acc
                    end,
                    [], user_volume)
          end,
    mnesia:activity(transaction, Fun).

-spec get_all_volumes() -> [non_neg_integer()].
get_all_volumes() ->
    Fun = fun() -> mnesia:foldl(fun(#user_volume{user_and_volume_id={_, V}},
                                    Acc) ->
                                        sets:add_element(V, Acc)
                                end,
                                sets:new(),
                                user_volume)
          end,
    sets:to_list(mnesia:activity(transaction, Fun)).

-spec get_all_users_with_volumes() -> #{}.
get_all_users_with_volumes() ->
    FoldFun = fun(#user_volume{user_and_volume_id={UserId, VolumeId}}, Acc) ->
                      Old = maps:get(UserId, Acc, sets:new()),
                      Acc#{UserId => sets:add_element(VolumeId, Old)}
              end,
    Fun = fun() -> mnesia:foldl(FoldFun, #{}, user_volume) end,
    maps:map(fun(_K, V) -> sets:to_list(V) end,
             mnesia:activity(transaction, Fun)).

%% issue functions
-spec add_user_issue(binary(), non_neg_integer(), non_neg_integer()) -> ok.
add_user_issue(UserId, VolumeId, IssueId) ->
    Fun = fun() ->
                  Pattern = #user_volume{user_and_volume_id={UserId, VolumeId},
                                         _ = '_'
                                        },
                  case mnesia:match_object(Pattern) of
                      [] -> {error, no_volume_found};
                      [Entry0=#user_volume{read_issues=Read0}] ->
                          Read = sets:add_element(IssueId, Read0),
                          Entry = Entry0#user_volume{read_issues=Read},
                          mnesia:write(Entry);
                      [_,_|_] -> throw({error,
                                        {multiple_entries, UserId, VolumeId}})
                  end
          end,
    mnesia:activity(transaction, Fun).

-spec remove_user_issue(binary(), non_neg_integer(), non_neg_integer()) -> ok.
remove_user_issue(UserId, VolumeId, IssueId) ->
    Fun = fun() ->
                  Pattern = #user_volume{user_and_volume_id={UserId, VolumeId},
                                         _ = '_'
                                        },
                  case mnesia:match_object(Pattern) of
                      [] -> {error, no_volume_found};
                      [Entry0=#user_volume{read_issues=Read0}] ->
                          Read = sets:del_element(IssueId, Read0),
                          Entry = Entry0#user_volume{read_issues=Read},
                          mnesia:write(Entry);
                      [_,_|_] -> throw({error,
                                        {multiple_entries, UserId, VolumeId}})
                  end
          end,
    mnesia:activity(transaction, Fun).

-spec get_user_issues(binary()) -> [#{}].
get_user_issues(UserId) ->
    FoldFn = fun(#user_volume{user_and_volume_id={U, V}, read_issues=I}, Acc)
                   when U =:= UserId ->
                     Acc#{V => I};
                (_, Acc) -> Acc
             end,
    Fn = fun() -> mnesia:foldl(FoldFn, #{}, user_volume) end,
    mnesia:activity(transaction, Fn).
