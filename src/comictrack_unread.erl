-module(comictrack_unread).

-export([get_user_unread_issues/1]).

get_user_unread_issues(UserId) ->
    VolumeIds = user_db:get_user_volumes(UserId),
    UserIssueIds = user_db:get_user_issues(UserId),
    UnreadList = build_unread_issue_list(VolumeIds, UserIssueIds),
    lists:sort(fun comic_issue:volume_name_sort/2, UnreadList).

build_unread_issue_list(VolumeIds, IssueIds) ->
    Lookup = fun(VolumeId) ->
                     {ok, V} = comic_repository:get_volume(#{id => VolumeId}),
                     V
             end,
    Volumes = lists:sort(fun comic_volume:name_sort/2,
                         lists:map(Lookup, VolumeIds)),
    Builder = fun(#{<<"id">> := VolumeId,
                    <<"name">> := VolumeName,
                    <<"start_year">> := StartYear,
                    <<"issues">> := IssueList},
                  Acc) ->
                      ReadIssues = maps:get(VolumeId, IssueIds, sets:new()),
                      VolumeBlock = #{<<"name">> => VolumeName,
                                      <<"id">> => VolumeId,
                                      <<"start_year">> => StartYear
                                     },
                      AddBlockFn = fun(Issue) ->
                                           Issue#{<<"volume">> => VolumeBlock}
                                   end,
                      MappedIssues = lists:map(AddBlockFn, IssueList),
                      SortedIssues = lists:sort(
                                       fun comic_issue:issue_number_sort/2,
                                       MappedIssues),
                      Filter = fun(#{<<"id">> := IssueId}) ->
                                       not sets:is_element(IssueId, ReadIssues)
                               end,
                      Acc ++ lists:filter(Filter, SortedIssues)
              end,
    lists:foldl(Builder, [], Volumes).
