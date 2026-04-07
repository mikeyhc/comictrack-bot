-module(message_engine).

-include("ui_prefixes.hrl").
-include_lib("kernel/include/logger.hrl").

-export([process/1]).

-define(APPLICATION_COMMAND, 2).
-define(MESSAGE_COMPONENT, 3).
-define(MODAL_SUBMIT, 5).

-define(COMPONENT_FLAG, (1 bsl 15)).
-define(EPHEMERAL_FLAG, (1 bsl 6)).
-define(DEFAULT_FLAGS, (?EPHEMERAL_FLAG)).

-define(PONG, 1).
-define(INTERACTION_REPLY, 4).
-define(DEFERRED_UPDATE_MESSAGE, 6).
-define(UPDATE_MESSAGE, 7).

-define(MAX_SELECT_ELEMENTS, 20).
-define(MAX_RESULTS, 10).

-define(CACHE_DURATION, (15 * 60)). % 15 minutes

process(D=#{<<"id">> := InteractionId,
            <<"token">> := InteractionToken,
            <<"data">> := Data,
            <<"type">> := Type
           }) ->
    Message = maps:get(<<"message">>, D, #{}),
    User = case maps:get(<<"member">>, D, undefined) of
               undefined ->
                   case maps:get(<<"user">>, D, undefined) of
                       undefined -> ?LOG_ERROR("no user data: ~p", [D]);
                       UserEntry -> UserEntry
                   end;
               Member -> Member
           end,
    IToken = #{interaction_id => InteractionId,
               interaction_token => InteractionToken
              },
    handle_msg(Type, Data, Message, User, IToken);
process(Msg) ->
    ?LOG_WARNING("unhandled message: ~p", [Msg]),
    ok.

handle_msg(?APPLICATION_COMMAND,
           #{<<"name">> := Cmd, <<"options">> := Options},
           _Message, Member, IToken)->
    handle_command(Cmd, Options, Member, IToken);
handle_msg(?MODAL_SUBMIT,
           #{<<"custom_id">> := CustomId, <<"components">> := Components},
           _Message, Member, IToken) ->
    handle_modal_reply(CustomId, Components, Member, IToken);
handle_msg(?MESSAGE_COMPONENT,
           #{<<"component_type">> := 2, <<"custom_id">> := Id},
           Message, Member, IToken) ->
    handle_button_press(Id, Message, Member, IToken);
handle_msg(?MESSAGE_COMPONENT,
           #{<<"component_type">> := 3, <<"custom_id">> := Id,
             <<"values">> := Values},
           _Message, Member, IToken) ->
    handle_string_select(Id, Values, Member, IToken).

handle_command(<<"comictrack">>, [Option], Member, IToken) ->
    case maps:get(<<"name">>, Option) of
        <<"volume">> ->
            #{<<"options">> := [SubCommand]} = Option,
            #{<<"name">> := SubCommandName} = SubCommand,
            handle_volume_command(SubCommandName, SubCommand, Member, IToken);
        <<"unread">> ->
            #{<<"options">> := [SubCommand]} = Option,
            #{<<"name">> := SubCommandName} = SubCommand,
            handle_unread_command(SubCommandName, SubCommand, Member, IToken);
        OptionName ->
            send_interaction_reply(<<"unknown command: ", OptionName/binary>>,
                                   IToken)
    end;
handle_command(Cmd, _Options, _Member, IToken) ->
    send_interaction_reply(<<"unknown command: ", Cmd/binary>>, IToken).

handle_modal_reply(<<"volume_select_modal_add">>, [Select], Member, IToken) ->
    #{<<"component">> := #{<<"values">> := [VolumeId]}} = Select,
    {ok, Volume} = comic_repository:lookup_volume(#{id => VolumeId}),
    user_db:add_user_volume(member_to_user_id(Member), VolumeId),
    #{<<"name">> := VolumeName} = Volume,
    send_interaction_reply(<<"added \"", VolumeName/binary, "\"">>, IToken);
handle_modal_reply(<<"volume_select_modal_get">>, [Select], Member, IToken) ->
    #{<<"component">> := #{<<"values">> := [VolumeId]}} = Select,
    {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
    UserIssueIds = user_db:get_user_issues(member_to_user_id(Member)),
    #{VolumeId := ReadIssues} = UserIssueIds,
    VolumeComponent = comictrack_ui:volume_view(Volume, ReadIssues, 1),
    send_interaction_reply_components(VolumeComponent, IToken);
handle_modal_reply(<<"volume_select_modal_read_", AllBinary/binary>>, [Select],
                   Member, IToken) ->
    #{<<"component">> := #{<<"values">> := [VolumeId]}} = Select,
    ReadAll = if AllBinary =:= <<"true">> -> true; true -> false end,
    {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
    if ReadAll ->
           #{<<"name">> := ActualName,
             <<"issues">> := Issues,
             <<"id">> := VolumeId} = Volume,
           UserId = member_to_user_id(Member),
           lists:foreach(
             fun(#{<<"id">> := IssueId}) ->
                     user_db:add_user_issue(UserId, VolumeId, IssueId)
             end,
             Issues),
           send_interaction_reply(<<"Marked all issues of \"",
                                    ActualName/binary, "\" as read">>,
                                  IToken);
       true ->
           VolumeRead = generate_volume_read(Volume, Member),
           send_interaction_reply_components(VolumeRead, IToken)
    end;
handle_modal_reply(<<"volume_select_modal_remove">>, [Select], Member,
                   IToken) ->
    #{<<"component">> := #{<<"values">> := [VolumeId]}} = Select,
    {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
    user_db:remove_user_volume(member_to_user_id(Member), VolumeId),
    #{<<"name">> := VolumeName} = Volume,
    send_interaction_reply(<<"removed volume \"", VolumeName/binary, "\"">>,
                           IToken).

handle_button_press(<<?VOLUME_PAGE_PREFIX, PageBin/binary>>,
                    _Message, Member, IToken) ->
    Page = binary_to_integer(PageBin),
    VolumeIds = user_db:get_user_volumes(member_to_user_id(Member)),
    VolumeList = comictrack_ui:volume_list(ids_to_volumes(VolumeIds), Page),
    send_interaction_update(VolumeList, IToken);
handle_button_press(<<?VOLUME_VIEW_PAGE_PREFIX, Rest/binary>>,
                    _Message, Member, IToken) ->
    [VolumeId, PageBin] = string:split(Rest, <<"_">>),
    Page = binary_to_integer(PageBin),
    {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
    UserIssueIds = user_db:get_user_issues(member_to_user_id(Member)),
    #{VolumeId := ReadIssues} = UserIssueIds,
    VolumeComponent = comictrack_ui:volume_view(Volume, ReadIssues, Page),
    send_interaction_update(VolumeComponent, IToken);
handle_button_press(<<?ISSUE_PAGE_PREFIX, PageBin/binary>>,
                    _Message, Member, IToken) ->
    Page = binary_to_integer(PageBin),
    UserId = member_to_user_id(Member),
    Unread = comictrack_unread:get_user_unread_issues(UserId),
    IssueList = comictrack_ui:unread_issue_list(Unread, Page),
    send_interaction_update(IssueList, IToken);
handle_button_press(<<?ISSUE_READ_PAGE_PREFIX, Rest/binary>>,
                    _Message, Member, IToken) ->
    case Rest of
        <<"v", Rest0/binary>> ->
            [VolumeId, PageBin] = string:split(Rest0, <<"_">>),
            Page = binary_to_integer(PageBin),
            {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
            VolumeRead = generate_volume_read(Volume, Member, Page),
            send_interaction_update(VolumeRead, IToken);
        <<"u_", Rest0/binary>> ->
            [CacheId, PageBin] = string:split(Rest0, <<"_">>),
            ?LOG_DEBUG("fetching cached results: ~s~n", [CacheId]),
            UserId = member_to_user_id(Member),
            Page = binary_to_integer(PageBin),
            case request_cache:get_entry(CacheId) of
                {ok, Unread} ->
                    UserVolumeIssueIds = user_db:get_user_issues(UserId),
                    UserIssueIds = sets:union(maps:values(UserVolumeIssueIds)),
                    UnreadReply = comictrack_ui:read_select(
                                    <<"u_", CacheId/binary, "_">>,
                                    Unread,
                                    UserIssueIds,
                                    Page),
                    send_interaction_update(UnreadReply, IToken);
                {error, Error} ->
                    ?LOG_WARNING("failed to get cache results[~s]: ~p",
                                 [CacheId, Error]),
                    Reply = comictrack_ui:string_reply(
                              <<"This message has expired">>),
                    send_interaction_update(Reply, IToken)
            end
    end.

handle_string_select(<<?ISSUE_READ_PREFIX, Rest/binary>>,
                     [Value], Member, IToken) ->
    [VolumeId, IssueId] = string:split(Rest, <<"_">>),
    UserId = member_to_user_id(Member),
    case Value of
        <<"read">> -> user_db:add_user_issue(UserId, VolumeId, IssueId);
        <<"unread">> -> user_db:remove_user_issue(UserId, VolumeId, IssueId)
    end,
    send_interaction_pong(IToken).

handle_volume_command(<<"add">>, Option, Member, IToken) ->
    #{<<"options">> := Arguments} = Option,
    case Arguments of
        [#{<<"name">> := <<"name">>,
           <<"value">> := VolumeName}] ->
            handle_volume_add(VolumeName, Member, IToken);
        _ -> send_interaction_reply(<<"requires a volume to add!">>, IToken)
    end;
handle_volume_command(<<"remove">>, Option, Member, IToken) ->
    #{<<"options">> := Arguments} = Option,
    case Arguments of
        [#{<<"name">> := <<"name">>,
           <<"value">> := VolumeName}] ->
            handle_volume_remove(VolumeName, Member, IToken);
        _ -> send_interaction_reply(<<"requires a volume to add!">>, IToken)
    end;
handle_volume_command(<<"get">>, Option, Member, IToken) ->
    #{<<"options">> := Arguments} = Option,
    case Arguments of
        [#{<<"name">> := <<"name">>,
           <<"value">> := VolumeName}] ->
            handle_volume_get(VolumeName, Member, IToken);
        _ -> send_interaction_reply(<<"requires a volume to get!">>, IToken)
    end;
handle_volume_command(<<"list">>, _Option, Member, IToken) ->
    handle_volume_list(Member, IToken);
handle_volume_command(<<"read">>, Option, Member, IToken) ->
    #{<<"options">> := ArgList} = Option,
    Arguments = lists:foldl(fun(A=#{<<"name">> := N}, Acc) -> Acc#{N => A} end,
                            #{}, ArgList),
    case Arguments of
        #{<<"name">> := #{<<"value">> := VolumeName}} ->
            ?LOG_INFO("Args: ~p", [Arguments]),
            #{<<"value">> := All} = maps:get(<<"all">>, Arguments,
                                             #{<<"value">> => false}),
            handle_volume_read(VolumeName, All, Member, IToken);
        _ -> send_interaction_reply(<<"requires a volume to read!">>, IToken)
    end.

handle_unread_command(<<"list">>, _Option, Member, IToken) ->
    handle_unread_issue_list(Member, IToken);
handle_unread_command(<<"read">>, _Option, Member, IToken) ->
    handle_unread_read_list(Member, IToken).

handle_volume_add(VolumeName, Member, IToken) ->
    CleanName = clean_name(VolumeName),
    case comic_repository:lookup_volume(#{name => CleanName}) of
        {ok, Volume} ->
            #{<<"name">> := ActualName, <<"id">> := VolumeId} = Volume,
            user_db:add_user_volume(member_to_user_id(Member), VolumeId),
            send_interaction_reply(<<"added \"", ActualName/binary, "\"">>,
                                   IToken);
        {error, {multiple_results, Volumes}} ->
            Subset = lists:sublist(lists:sort(
                                     fun comic_volume:start_year_sort/2,
                                     Volumes),
                                   ?MAX_SELECT_ELEMENTS),
            volume_select_reply(<<"add">>, Subset, IToken);
        {error, not_found} ->
            send_interaction_reply(
              <<"could not find volume \"", CleanName/binary, "\"">>,
              IToken);
        {error, Err} -> ?LOG_ERROR("volume add error: ~p", [Err]),
            send_interaction_reply(<<"an unexpected error occurred">>, IToken)
    end.


handle_volume_remove(VolumeName, Member, IToken) ->
    CleanName = clean_name(VolumeName),
    UserId = member_to_user_id(Member),
    {_Oks, UserVolumes} = lists:unzip(
                            lists:map(
                              fun(V) ->
                                      comic_repository:get_volume(#{id => V})
                              end,
                              user_db:get_user_volumes(UserId))),
    case lists:filter(comic_volume:name_fuzzymatcher(CleanName), UserVolumes) of
        [] ->
            send_interaction_reply(
              <<"could not find volume \"", CleanName/binary, "\"">>,
              IToken);
        [#{<<"id">> := VolumeId, <<"name">> := ActualName}] ->
            user_db:remove_user_volume(member_to_user_id(Member), VolumeId),
            send_interaction_reply(
              <<"removed volume \"", ActualName/binary, "\"">>,
              IToken);
        Volumes ->
            case exact_match_volume(CleanName, Volumes) of
                [#{<<"id">> := VolumeId, <<"name">> := ActualName}] ->
                    user_db:remove_user_volume(member_to_user_id(Member),
                                               VolumeId),
                    send_interaction_reply(
                      <<"removed volume \"", ActualName/binary, "\"">>,
                      IToken);
                _ ->
                    Subset = lists:sublist(lists:sort(
                                             fun comic_volume:start_year_sort/2,
                                             Volumes),
                                           ?MAX_SELECT_ELEMENTS),
                    volume_select_reply(<<"remove">>, Subset, IToken)
            end
    end.

handle_volume_get(VolumeName, Member, IToken) ->
    CleanName = clean_name(VolumeName),
    UserId = member_to_user_id(Member),
    {_Oks, UserVolumes} = lists:unzip(
                            lists:map(
                              fun(V) ->
                                      comic_repository:get_volume(#{id => V})
                              end,
                              user_db:get_user_volumes(UserId))),
    case lists:filter(comic_volume:name_fuzzymatcher(CleanName), UserVolumes) of
        [] ->
            send_interaction_reply(
              <<"could not find volume \"", CleanName/binary, "\"">>,
              IToken);
        [Volume=#{<<"id">> := VolumeId}] ->
            UserIssueIds = user_db:get_user_issues(member_to_user_id(Member)),
            #{VolumeId := ReadIssues} = UserIssueIds,
            VolumeComponent = comictrack_ui:volume_view(Volume, ReadIssues, 1),
            send_interaction_reply_components(VolumeComponent, IToken);
        Volumes ->
            case exact_match_volume(CleanName, Volumes) of
                [Volume=#{<<"id">> := VolumeId}] ->
                    UserIssueIds = user_db:get_user_issues(
                                     member_to_user_id(Member)),
                    #{VolumeId := ReadIssues} = UserIssueIds,
                    VolumeComponent = comictrack_ui:volume_view(Volume, ReadIssues, 1),
                    send_interaction_reply_components(VolumeComponent, IToken);
                _ ->
                    Subset = lists:sublist(lists:sort(
                                             fun comic_volume:start_year_sort/2,
                                             Volumes),
                                           ?MAX_SELECT_ELEMENTS),
                    volume_select_reply(<<"get">>, Subset, IToken)
            end
    end.

handle_volume_read(VolumeName, ReadAll, Member, IToken) ->
    CleanName = clean_name(VolumeName),
    UserId = member_to_user_id(Member),
    {_Oks, UserVolumes} = lists:unzip(
                            lists:map(
                              fun(V) ->
                                      comic_repository:get_volume(#{id => V})
                              end,
                              user_db:get_user_volumes(UserId))),
    case lists:filter(comic_volume:name_fuzzymatcher(CleanName), UserVolumes) of
        [] ->
            send_interaction_reply(
              <<"could not find volume \"", CleanName/binary, "\"">>,
              IToken);
        [Volume] ->
            if ReadAll ->
                   #{<<"name">> := ActualName,
                     <<"issues">> := Issues,
                     <<"id">> := VolumeId} = Volume,

                   lists:foreach(
                     fun(#{<<"id">> := IssueId}) ->
                             user_db:add_user_issue(UserId, VolumeId, IssueId)
                     end,
                     Issues),
                   send_interaction_reply(<<"Marked all issues of \"",
                                            ActualName/binary, "\" as read">>,
                                          IToken);
               true ->
                   VolumeRead = generate_volume_read(Volume, Member),
                   send_interaction_reply_components(VolumeRead, IToken)
            end;
        Volumes ->
            Subset = lists:sublist(lists:sort(
                                     fun comic_volume:start_year_sort/2,
                                     Volumes),
                                   ?MAX_SELECT_ELEMENTS),
            ReadAllBool = if ReadAll -> <<"true">>; true -> <<"false">> end,
            volume_select_reply(<<"read_", ReadAllBool/binary>>, Subset, IToken)
    end.

handle_volume_list(Member, IToken) ->
    handle_volume_list(Member, 1, IToken).

handle_volume_list(Member, Page, IToken) ->
    VolumeIds = user_db:get_user_volumes(member_to_user_id(Member)),
    case VolumeIds of
        [] ->
            send_interaction_reply(<<"you aren't tracking any volumes :(">>,
                                   IToken);
        _ ->
            Volumes = ids_to_volumes(VolumeIds),
            VolumeList = comictrack_ui:volume_list(Volumes, Page),
            send_interaction_reply_components(VolumeList, IToken)
    end.

handle_unread_issue_list(Member, IToken) ->
    UserId = member_to_user_id(Member),
    VolumeIds = user_db:get_user_volumes(UserId),
    case VolumeIds of
        [] ->
            send_interaction_reply(<<"you aren't tracking any volumes :(">>,
                                   IToken);
        _ ->
            Unread = comictrack_unread:get_user_unread_issues(UserId),
            IssueList = comictrack_ui:unread_issue_list(Unread, 1),
            send_interaction_reply_components(IssueList, IToken)
    end.

handle_unread_read_list(Member, IToken) ->
    UserId = member_to_user_id(Member),
    VolumeIds = user_db:get_user_volumes(UserId),
    case VolumeIds of
        [] ->
            send_interaction_reply(<<"you aren't tracking any volumes :(">>,
                                   IToken);
        _ ->
            Unread = comictrack_unread:get_user_unread_issues(UserId),
            CacheId = request_cache:store_entry(Unread, ?CACHE_DURATION),
            Reply = comictrack_ui:read_select(<<"u_", CacheId/binary, "_">>,
                                              Unread, sets:new(), 1),
            send_interaction_reply_components(Reply, IToken)
    end.

generate_volume_read(Volume, Member) ->
    generate_volume_read(Volume, Member, 1).

generate_volume_read(V=#{<<"id">> := VolumeId,
                         <<"name">> := ActualName,
                         <<"start_year">> := StartYear,
                         <<"issues">> := Issues},
                    Member,
                    Page) ->
            UserIssueIds = user_db:get_user_issues(member_to_user_id(Member)),
            Sorted = lists:sort(fun comic_issue:issue_number_sort/2, Issues),
            ReadSelect = comictrack_ui:read_select(
                           <<"v", VolumeId/binary, "_">>,
                           lists:map(fun(I) -> decorate_with_volume(I, V) end,
                                     Sorted),
                           maps:get(VolumeId, UserIssueIds, sets:new()),
                           Page),
            StartYearBin = if StartYear =:= null -> <<"">>;
                              true -> <<" [", StartYear/binary, "] ">>
                           end,
              [#{<<"type">> => 10,
                 <<"content">> => <<"Issue list for ", ActualName/binary,
                                    StartYearBin/binary>>
                }
              ] ++ ReadSelect.


decorate_with_volume(Issue, #{<<"id">> := Id,
                              <<"name">> := Name,
                              <<"start_year">> := StartYear}) ->
    Issue#{<<"volume">> => #{<<"id">> => Id,
                             <<"name">> => Name,
                             <<"start_year">> => StartYear
                            }}.

member_to_user_id(#{<<"user">> := #{<<"id">> := UserId}}) -> UserId;
member_to_user_id(#{<<"id">> := UserId}) -> UserId.

send_interaction_reply(Msg, IToken) ->
    Body = #{
             content => Msg,
             flags => ?DEFAULT_FLAGS
            },
    send_interaction_reply_(Body, IToken).

send_interaction_reply_components(Components, IToken) ->
    Body = #{
             components => Components,
             flags => ?DEFAULT_FLAGS bor ?COMPONENT_FLAG
            },
    send_interaction_reply_(Body, IToken).


send_interaction_reply_(Msg,
                        #{interaction_id := InteractionId,
                          interaction_token := InteractionToken}) ->
    Reply = #{
              type => ?INTERACTION_REPLY,
              data => Msg
             },
    discord_api:interaction_callback(
      InteractionId,
      InteractionToken,
      Reply
     ).

send_interaction_update(Msg,
                        #{interaction_id := InteractionId,
                          interaction_token := InteractionToken
                         }) ->
    Update = #{
               type => ?UPDATE_MESSAGE,
               data => #{
                   components => Msg,
                   flags => ?DEFAULT_FLAGS bor ?COMPONENT_FLAG
               }
              },
    discord_api:interaction_callback(
      InteractionId,
      InteractionToken,
      Update
     ).

send_interaction_pong(#{interaction_id := InteractionId,
                        interaction_token := InteractionToken
                       }) ->
    Pong = #{type => ?DEFERRED_UPDATE_MESSAGE},
    discord_api:interaction_callback(
      InteractionId,
      InteractionToken,
      Pong
     ).

volume_select_reply(Id, Volumes,
                    #{interaction_id := InteractionId,
                      interaction_token := InteractionToken}) ->
    discord_api:interaction_callback(
      InteractionId,
      InteractionToken,
      comictrack_ui:volume_select_modal(Id, Volumes)
     ).

exact_match_volume(Name, Volumes) ->
    Fun = fun(#{<<"name">> := N}) -> N =:= Name end,
    lists:filter(Fun, Volumes).

ids_to_volumes(VolumeIds) ->
    Lookup = fun(VolumeId) ->
                     {ok, V} = comic_repository:get_volume(#{id => VolumeId}),
                     V
             end,
    lists:sort(fun comic_volume:name_sort/2, lists:map(Lookup, VolumeIds)).

clean_name(Name) ->
    lists:foldl(fun(Fn, Acc) -> Fn(Acc) end,
                Name,
                [fun string:trim/1,
                 fun unquote/1
                ]).

unquote(S) when is_binary(S) ->
    list_to_binary(unquote(binary_to_list(S)));
unquote(S=[$"|Rest]) ->
    Last = lists:last(S),
    if Last =:= $" -> lists:droplast(Rest);
       true -> S
    end;
unquote(S) -> S.
