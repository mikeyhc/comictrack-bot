-module(comictrack_util).

-include_lib("kernel/include/logger.hrl").

-export([sync/0, send_new_updates/0]).

sync() ->
    Results = lists:map(
                fun(VolumeId) ->
                        ?LOG_INFO("fetching updates for volume id ~p",
                                  [VolumeId]),
                        comicvine_updater:fetch_volume_from_api(VolumeId)
                end,
                user_db:get_all_volumes()),
    ?LOG_INFO("fetched ~p volumes", [length(Results)]),
    {Statuses, _Values} = lists:unzip(Results),
    true = lists:all(fun(V) -> V =:= ok end, Statuses),
    ?LOG_INFO("sync successful").

send_new_updates() ->
    VolumeToIssue = lists:foldl(fun(Issue, Acc) ->
                                        #{<<"volume">> :=
                                          #{<<"id">> := VolumeId}} = Issue,
                                        Acc#{VolumeId => Issue}
                                end,
                                #{},
                                comic_repository:get_new_issues()),
    UserToVolumes = user_db:get_all_users_with_volumes(),
    FMFun = fun(Volume) ->
                    case maps:get(Volume, VolumeToIssue, false) of
                        false -> false;
                        Issue -> {true, issue_name(Issue)}
                    end
            end,
    Fun = fun({User, Volumes}) ->
                  {User, lists:filtermap(FMFun, Volumes)}
          end,
    MailList = lists:map(Fun, maps:to_list(UserToVolumes)),
    lists:foreach(fun send_issue_list/1, MailList).

issue_name(#{<<"volume">> := #{<<"name">> := VolumeName},
             <<"issue_number">> := IssueNumber,
             <<"name">> := null}) ->
    BinIssueNumber = if is_integer(IssueNumber) ->
                            integer_to_binary(IssueNumber);
                        true -> IssueNumber
                     end,
    <<VolumeName/binary, " #", BinIssueNumber/binary>>;
issue_name(#{<<"volume">> := #{<<"name">> := VolumeName},
             <<"issue_number">> := IssueNumber,
             <<"name">> := IssueName}) ->
    BinIssueNumber = if is_integer(IssueNumber) ->
                            integer_to_binary(IssueNumber);
                        true -> IssueNumber
                     end,
    <<VolumeName/binary, " #", BinIssueNumber/binary, ": ",
      IssueName/binary>>.

send_issue_list({UserId, Updates}) ->
    Msg = if length(Updates) =:= 0 -> <<"No new comics for you this week! :(">>;
             true -> build_new_issues_msg(Updates)
          end,
    ?LOG_INFO("sending ~p new issues for user @~s", [length(Updates), UserId]),
    {ok, _Resp} = discord_api:direct_message(UserId, Msg).

build_new_issues_msg(Issues) ->
    lists:foldl(fun(X, Acc) -> <<Acc/binary, "\n* ", X/binary>> end,
                <<"New issues available for you this week:">>,
                Issues).
