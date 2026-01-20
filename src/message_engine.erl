-module(message_engine).
-include_lib("kernel/include/logger.hrl").

-export([process/1]).

-define(APPLICATION_COMMAND, 2).
-define(MESSAGE_COMPONENT, 3).
-define(MODAL_SUBMIT, 5).

-define(COMPONENT_FLAG, (1 bsl 15)).
-define(EPHEMERAL_FLAG, (1 bsl 6)).
-define(DEFAULT_FLAGS, (?EPHEMERAL_FLAG)).

-define(INTERACTION_REPLY, 4).
-define(MODAL_REPLY, 9).

-define(MAX_SELECT_ELEMENTS, 20).
-define(NAME_OPTION_LENGTH, 50).
-define(MAX_RESULTS, 20).

process(D=#{<<"id">> := InteractionId,
            <<"token">> := InteractionToken,
            <<"member">> := Member,
            <<"data">> := Data,
            <<"type">> := Type,
            <<"application_id">> := ApplicationId
           }) ->
    State0 = #{interaction_id => InteractionId,
               interaction_token => InteractionToken,
               application_id => ApplicationId
              },
    State = case maps:get(<<"message">>, D, undefined) of
                undefined -> State0;
                #{<<"id">> := MessageId} ->
                    State0#{message_id => MessageId}
            end,
    handle_msg(Type, Data, Member, State);
process(Msg) ->
    ?LOG_WARNING("unhandled message: ~p", [Msg]),
    ok.

handle_msg(?APPLICATION_COMMAND,
           #{<<"name">> := Cmd, <<"options">> := Options},
           Member, IToken)->
    handle_command(Cmd, Options, Member, IToken);
handle_msg(?MODAL_SUBMIT,
           #{<<"custom_id">> := CustomId, <<"components">> := Components},
           Member, IToken) ->
    handle_modal_reply(CustomId, Components, Member, IToken);
handle_msg(?MESSAGE_COMPONENT,
           #{<<"component_type">> := 2, <<"custom_id">> := Id},
           Member, IToken) ->
    handle_button_press(Id, Member, IToken).

handle_command(<<"comictrack">>, [Option], Member, IToken) ->
    #{<<"name">> := OptionName,
      <<"options">> := [SubCommand]
     } = Option,
    case OptionName of
        <<"volume">> ->
            #{<<"name">> := SubCommandName} = SubCommand,
            handle_volume_command(SubCommandName, SubCommand, Member, IToken);
        Option ->
            send_interaction_reply(<<"unknown command: ", Option/binary>>,
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
    send_interaction_reply(<<"found ", VolumeName/binary>>, IToken).

handle_button_press(<<"vp_", PageBin/binary>>, Member, IToken) ->
    Page = binary_to_integer(PageBin),
    handle_volume_list(Member, Page, IToken).

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
    handle_volume_list(Member, IToken).

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

handle_volume_list(Member, IToken) ->
    handle_volume_list(Member, 1, IToken).

handle_volume_list(Member, Page, IToken) ->
    VolumeIds = user_db:get_user_volumes(member_to_user_id(Member)),
    case VolumeIds of
        [] ->
            send_interaction_reply(<<"you aren't tracking any volumes :(">>,
                                   IToken);
        _ ->
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
            Reply1 = if length(Volumes) > ?MAX_RESULTS ->
                                PageCount = count_pages(Volumes),
                                BinPage = integer_to_binary(Page),
                                BinPageCount = integer_to_binary(PageCount),
                                PageFooter = <<"(page ", BinPage/binary, " of ",
                                               BinPageCount/binary, ")">>,
                                PrevPageBin = integer_to_binary(Page - 1),
                                NextPageBin = integer_to_binary(Page + 1),
                                PrevPageLink = <<"vp_", PrevPageBin/binary>>,
                                NextPageLink = <<"vp_", NextPageBin/binary>>,
                                Footer = [#{type => 10,
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
                                                            disabled => Page >= PageCount
                                                           }]}
                                         ],
                                Reply0 ++ Footer;
                        true -> Reply0
                     end,
            send_interaction_reply_components(Reply1, IToken)
    end.

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
                        #{application_id := ApplicationId,
                          interaction_token := InteractionToken,
                          message_id := MessageId
                         }) ->
    Update = #{
               content => Msg,
               flags => ?DEFAULT_FLAGS
              },
    discord_api:interaction_update(
      ApplicationId,
      InteractionToken,
      MessageId,
      Update
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
