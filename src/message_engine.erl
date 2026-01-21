-module(message_engine).
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
-define(MODAL_REPLY, 9).

-define(MAX_SELECT_ELEMENTS, 20).
-define(NAME_OPTION_LENGTH, 50).
-define(MAX_RESULTS, 10).

process(D=#{<<"id">> := InteractionId,
            <<"token">> := InteractionToken,
            <<"member">> := Member,
            <<"data">> := Data,
            <<"type">> := Type
           }) ->
    Message = maps:get(<<"message">>, D, #{}),
    IToken = #{interaction_id => InteractionId,
               interaction_token => InteractionToken
              },
    handle_msg(Type, Data, Message, Member, IToken);
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
        <<"unread">> -> handle_unread_command(Member, IToken);
        OptionName ->
            send_interaction_reply(<<"unknown command: ", OptionName/binary>>,
                                   IToken)
    end;
handle_command(Cmd, _Options, _Member, IToken) ->
    send_interaction_reply(<<"unknown command: ", Cmd/binary>>, IToken).

handle_modal_reply(<<"volume_select_modal_add">>, [Select], Member, IToken) ->
    #{<<"component">> := #{<<"values">> := [StrVolumeId]}} = Select,
    VolumeId = binary_to_integer(StrVolumeId),
    {ok, Volume} = comic_repository:lookup_volume(#{id => VolumeId}),
    user_db:add_user_volume(member_to_user_id(Member), VolumeId),
    #{<<"name">> := VolumeName} = Volume,
    send_interaction_reply(<<"added \"", VolumeName/binary, "\"">>, IToken);
handle_modal_reply(<<"volume_select_modal_get">>, [Select], _Member, IToken) ->
    #{<<"component">> := #{<<"values">> := [StrVolumeId]}} = Select,
    VolumeId = binary_to_integer(StrVolumeId),
    {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
    #{<<"name">> := VolumeName} = Volume,
    send_interaction_reply(<<"found ", VolumeName/binary>>, IToken);
handle_modal_reply(<<"volume_select_modal_read">>, [Select], Member, IToken) ->
    #{<<"component">> := #{<<"values">> := [StrVolumeId]}} = Select,
    VolumeId = binary_to_integer(StrVolumeId),
    {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
    VolumeRead = generate_volume_read(Volume, Member),
    send_interaction_reply_components(VolumeRead, IToken).

handle_button_press(<<"vp_", PageBin/binary>>, _Message, Member, IToken) ->
    Page = binary_to_integer(PageBin),
    VolumeIds = user_db:get_user_volumes(member_to_user_id(Member)),
    VolumeList = generate_volume_list(VolumeIds, Page),
    send_interaction_update(VolumeList, IToken);
handle_button_press(<<"ip_", PageBin/binary>>, _Message, Member, IToken) ->
    Page = binary_to_integer(PageBin),
    UserId = member_to_user_id(Member),
    VolumeIds = user_db:get_user_volumes(UserId),
    UserIssueIds = user_db:get_user_issues(UserId),
    UnreadList = build_unread_issue_list(VolumeIds, UserIssueIds),
    IssueList = generate_issue_list(UnreadList, Page),
    send_interaction_update(IssueList, IToken);
handle_button_press(<<"irp_", Rest/binary>>, _Message, Member, IToken) ->
    [VolumeIdBin, PageBin] = string:split(Rest, <<"_">>),
    VolumeId = binary_to_integer(VolumeIdBin),
    Page = binary_to_integer(PageBin),
    {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
    VolumeRead = generate_volume_read(Volume, Member, Page),
    send_interaction_update(VolumeRead, IToken).

handle_string_select(<<"ir_", Rest/binary>>, [Value], Member, IToken) ->
    [VolumeIdBin, IssueIdBin] = string:split(Rest, <<"_">>),
    UserId = member_to_user_id(Member),
    VolumeId = binary_to_integer(VolumeIdBin),
    IssueId = binary_to_integer(IssueIdBin),
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
handle_volume_command(<<"get">>, Option, _Member, IToken) ->
    #{<<"options">> := Arguments} = Option,
    case Arguments of
        [#{<<"name">> := <<"name">>,
           <<"value">> := VolumeName}] ->
            handle_volume_get(VolumeName, IToken);
        _ -> send_interaction_reply(<<"requires a volume to get!">>, IToken)
    end;
handle_volume_command(<<"list">>, _Option, Member, IToken) ->
    handle_volume_list(Member, IToken);
handle_volume_command(<<"read">>, Option, Member, IToken) ->
    #{<<"options">> := Arguments} = Option,
    case Arguments of
        [#{<<"name">> := <<"name">>,
           <<"value">> := VolumeName}] ->
            handle_volume_read(VolumeName, Member, IToken);
        _ -> send_interaction_reply(<<"requires a volume to read!">>, IToken)
    end.

handle_unread_command(Member, IToken) ->
    handle_unread_issue_list(Member, IToken).

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
            volume_select_modal(<<"add">>, Subset, IToken);
        {error, not_found} ->
            send_interaction_reply(
              <<"could not find volume \"", CleanName/binary, "\"">>,
              IToken);
        {error, Err} -> ?LOG_ERROR("volume add error: ~p", [Err]),
            send_interaction_reply(<<"an unexpected error occurred">>, IToken)
    end.

handle_volume_get(VolumeName, IToken) ->
    CleanName = clean_name(VolumeName),
    % TODO this needs to check that is actually tacked by the user
    case comic_repository:get_volume(#{name => CleanName}) of
        {ok, _Volume} ->
            send_interaction_reply(<<"found ", CleanName/binary>>, IToken);
        {error, {multiple_results, Volumes}} ->
            Subset = lists:sublist(lists:sort(
                                     fun comic_volume:start_year_sort/2,
                                     Volumes),
                                   ?MAX_SELECT_ELEMENTS),
            volume_select_modal(<<"get">>, Subset, IToken);
        {error, not_found} ->
            send_interaction_reply(
              <<"could not find volume \"", CleanName/binary, "\"">>,
              IToken);
        {error, Err} -> ?LOG_ERROR("volume add error: ~p", [Err]),
            send_interaction_reply(<<"an unexpected error occurred">>, IToken)
    end.

handle_volume_read(VolumeName, Member, IToken) ->
    CleanName = clean_name(VolumeName),
    % TODO: this needs to check that is actually tracked by the user
    case comic_repository:get_volume(#{name => CleanName}) of
        {ok, Volume} ->
            VolumeRead = generate_volume_read(Volume, Member),
            send_interaction_reply_components(VolumeRead, IToken);
        {error, {multiple_results, Volumes}} ->
            Subset = lists:sublist(lists:sort(
                                     fun comic_volume:start_year_sort/2,
                                     Volumes),
                                   ?MAX_SELECT_ELEMENTS),
            volume_select_modal(<<"read">>, Subset, IToken);
        {error, not_found} ->
            send_interaction_reply(
              <<"could not find volume \"", CleanName/binary, "\"">>,
              IToken);
        {error, Err} -> ?LOG_ERROR("volume add error: ~p", [Err]),
            send_interaction_reply(<<"an unexpected error occurred">>, IToken)
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
            VolumeList = generate_volume_list(VolumeIds, Page),
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
            UserIssueIds = user_db:get_user_issues(UserId),
            UnreadList = build_unread_issue_list(VolumeIds, UserIssueIds),
            IssueList = generate_issue_list(UnreadList, 1),
            send_interaction_reply_components(IssueList, IToken)
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
            ReadSelect = build_read_select(
                           VolumeId,
                           lists:map(fun(I) -> decorate_with_volume(I, V) end,
                                     Issues),
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

generate_volume_list(VolumeIds, Page) ->
    Lookup = fun(VolumeId) ->
                     {ok, V} = comic_repository:get_volume(#{id => VolumeId}),
                     V
             end,
    Volumes = lists:sort(fun comic_volume:name_sort/2,
                         lists:map(Lookup, VolumeIds)),
    Truncated = lists:sublist(Volumes, (Page - 1) * ?MAX_RESULTS + 1,
                              ?MAX_RESULTS),
    VolumeBinary = build_volumes_binary(Truncated),
    VolumeContent = <<"You are tracking the following volumes:\n",
                      VolumeBinary/binary>>,
    Reply0 = [
              #{type => 10,
                content => VolumeContent
               }
             ],
    if length(Volumes) > ?MAX_RESULTS ->
           Reply0 ++ page_controls(<<"vp">>, Page, count_pages(Volumes));
       true -> Reply0
    end.

generate_issue_list(Issues, Page) ->
    Truncated = lists:sublist(Issues, (Page - 1) * ?MAX_RESULTS + 1,
                              ?MAX_RESULTS),
    IssueBinary = build_issue_binary(Truncated),
    IssueContent = <<"The following issues are unread:\n",
                     IssueBinary/binary>>,
    Reply0 = [
              #{type => 10,
                content => IssueContent
               }
             ],
    if length(Issues) > ?MAX_RESULTS ->
           Reply0 ++ page_controls(<<"ip">>, Page, count_pages(Issues));
       true -> Reply0
    end.

page_controls(Prefix, Page, MaxPage) ->
    BinPage = integer_to_binary(Page),
    BinMaxPage = integer_to_binary(MaxPage),
    PageFooter = <<"(page ", BinPage/binary, " of ",
                   BinMaxPage/binary, ")">>,
    PrevPageBin = integer_to_binary(Page - 1),
    NextPageBin = integer_to_binary(Page + 1),
    PrevPageLink = <<Prefix/binary, "_", PrevPageBin/binary>>,
    NextPageLink = <<Prefix/binary, "_", NextPageBin/binary>>,
    [#{type => 10,
       content => PageFooter
      },
     #{type => 1,
       components => [#{type => 2,
                        style => 1,
                        label => <<"Previous Page">>,
                        custom_id => PrevPageLink,
                        disabled => Page =:= 1
                       },
                      #{type => 2,
                        style => 1,
                        label => <<"Next Page">>,
                        custom_id => NextPageLink,
                        disabled => Page >= MaxPage
                       }]}].

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

build_read_select(VolumeId, Issues, ReadIssues, Page) ->
    BinVolId = integer_to_binary(VolumeId),
    Builder = fun(I=#{<<"id">> := Id}) ->
                      BinId = integer_to_binary(Id),
                      Cid = <<"ir_", BinVolId/binary, "_", BinId/binary>>,
                      Read = sets:is_element(Id, ReadIssues),
                      Default = if Read -> <<"Read">>;
                                   true -> <<"Unread">>
                                end,
                      [
                       #{type => 10,
                         content => comic_issue:full_name(I)
                        },
                       #{type => 1,
                         components => [#{type => 3,
                                          custom_id => Cid,
                                          options => [#{label => <<"Read">>,
                                                        value => <<"read">>
                                                       },
                                                      #{label => <<"Unread">>,
                                                        value => <<"unread">>
                                                       }
                                                     ],
                                          placeholder => Default
                                         }]
                        }
                      ]
              end,
    Sorted = lists:sort(fun comic_issue:issue_number_sort/2, Issues),
    Truncated = lists:sublist(Sorted, (Page - 1) * ?MAX_RESULTS + 1,
                              ?MAX_RESULTS),
    Reply0 = lists:flatten(lists:map(Builder, Truncated)),
    if length(Issues) > ?MAX_RESULTS ->
           Prefix = <<"irp_", BinVolId/binary>>,
           Reply0 ++ page_controls(Prefix, Page,
                                   count_pages(Issues));
       true -> Reply0
    end.

decorate_with_volume(Issue, #{<<"id">> := Id,
                              <<"name">> := Name,
                              <<"start_year">> := StartYear}) ->
    Issue#{<<"volume">> => #{<<"id">> => Id,
                             <<"name">> => Name,
                             <<"start_year">> => StartYear
                            }}.

count_pages(Input) ->
    round(math:ceil(length(Input) / ?MAX_RESULTS)).

member_to_user_id(#{<<"user">> := #{<<"id">> := UserId}}) -> UserId.

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

volume_select_modal(Id, Volumes,
                    #{interaction_id := InteractionId,
                      interaction_token := InteractionToken}) ->
    Reply = #{
        type => ?MODAL_REPLY,
        data => #{
            custom_id => <<"volume_select_modal_", Id/binary>>,
            title => <<"Select Volume">>,
            components => [
                #{type => 18,
                  label => <<"Which volume">>,
                  component => #{
                      type => 3,
                      custom_id => <<"volume_name_select">>,
                      placeholder => <<"Choose...">>,
                      options => lists:map(fun build_volume_option/1, Volumes)
                  }
                 }
            ]
        }
    },
    discord_api:interaction_callback(
      InteractionId,
      InteractionToken,
      Reply
     ).

build_volume_option(Volume=#{<<"id">> := Id}) ->
    #{label => comic_volume:full_name(Volume,
                                      #{name_length => ?NAME_OPTION_LENGTH}),
      value => Id
     }.

build_volumes_binary(Volumes) ->
    Fun = fun(Volume, Acc) ->
                  Name = comic_volume:full_name(Volume),
                  <<Acc/binary, "* ", Name/binary, "\n">>
          end,
    lists:foldl(Fun, <<>>, Volumes).

build_issue_binary(Issues) ->
    Fun = fun(Issue, Acc) ->
                  Name = comic_issue:full_name(Issue),
                  <<Acc/binary, "* ", Name/binary, "\n">>
          end,
    lists:foldl(Fun, <<>>, Issues).

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
