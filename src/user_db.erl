-module(user_db).

% Public API
-export([install/1]).
-export([add_user_volume/2, get_user_volumes/1, get_all_volumes/0,
         get_all_users_with_volumes/0]).
-export([add_user_issue/3, get_user_issues/1]).

-record(user_volume, {user_id                :: binary(),
                      volume_id              :: non_neg_integer(),
                      read_issues=sets:new() :: sets:set()
                     }).

-spec install([node()]) -> ok.
install(Nodes) ->
    Tables = #{
        user_volume => [{attributes,
                         record_info(fields, user_volume)},
                        {type, bag},
                        {disc_copies, Nodes}]
    },
    db_utils:install(Nodes, Tables).

%% volume functions
-spec add_user_volume(binary(), non_neg_integer()) -> ok.
add_user_volume(UserId, VolumeId) ->
    Record = #user_volume{user_id=UserId, volume_id=VolumeId},
    mnesia:activity(transaction, fun() -> mnesia:write(Record) end).

-spec get_user_volumes(binary()) -> [non_neg_integer()].
get_user_volumes(UserId) ->
    lists:map(fun(#user_volume{volume_id=V}) -> V end,
              mnesia:activity(transaction,
                              fun() -> mnesia:read({user_volume, UserId}) end)).

-spec get_all_volumes() -> [non_neg_integer()].
get_all_volumes() ->
    Fun = fun() -> mnesia:foldl(fun(#user_volume{volume_id=V}, Acc) ->
                                        sets:add_element(V, Acc)
                                end,
                                sets:new(),
                                user_volume)
          end,
    sets:to_list(mnesia:activity(transaction, Fun)).

-spec get_all_users_with_volumes() -> #{}.
get_all_users_with_volumes() ->
    FoldFun = fun(#user_volume{user_id=UserId,
                               volume_id=VolumeId},
                  Acc) ->
                      Old = maps:get(UserId, Acc, sets:new()),
                      Acc#{UserId => sets:add_element(VolumeId, Old)}
              end,
    Fun = fun() -> mnesia:foldl(FoldFun, #{}, user_volume) end,
    maps:map(fun(_K, V) -> sets:to_list(V) end,
             mnesia:activity(transaction, Fun)).

%% issue functions
-spec add_user_issue(binary(), non_neg_integer(), non_neg_integer()) -> ok.
add_user_issue(_UserId, _VolumeId, _IssueId) ->
    throw(not_implemented).

-spec get_user_issues(binary()) -> [#{}].
get_user_issues(_UserId) ->
    throw(not_implemented).
