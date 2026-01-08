-module(message_engine).
-include_lib("kernel/include/logger.hrl").

-export([process/1]).

-define(EPHEMERAL_FLAG, (1 bsl 6)).
-define(DEFAULT_FLAGS, (?EPHEMERAL_FLAG)).
-define(MAX_SELECT_ELEMENTS, 20).

process(#{<<"id">> := InteractionId,
          <<"token">> := InteractionToken,
          <<"member">> := Member,
          <<"data">> := Data
         }) ->
    handle_msg(Data, Member, {InteractionId, InteractionToken});
process(Msg) ->
    ?LOG_WARNING("unhandled message: ~p", [Msg]),
    ok.

handle_msg(#{<<"name">> := Cmd, <<"options">> := Options}, Member, IToken) ->
    handle_command(Cmd, Options, Member, IToken);
handle_msg(#{<<"custom_id">> := CustomId, <<"components">> := Components},
           Member, IToken) ->
    handle_modal_reply(CustomId, Components, Member, IToken).

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

handle_modal_reply(<<"volume_select_modal">>, [Select], Member, IToken) ->
    #{<<"component">> := #{<<"values">> := [StrVolumeId]}} = Select,
    VolumeId = binary_to_integer(StrVolumeId),
    {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
    user_db:add_user_volume(member_to_user_id(Member), VolumeId),
    #{<<"name">> := VolumeName} = Volume,
    send_interaction_reply(<<"added \"", VolumeName/binary, "\"">>, IToken).


handle_volume_command(<<"add">>, Option, Member, IToken) ->
    #{<<"options">> := Arguments} = Option,
    case Arguments of
        [#{<<"name">> := <<"name">>,
           <<"value">> := VolumeName}] ->
            handle_volume_add(VolumeName, Member, IToken);
        _ -> send_interaction_reply(<<"requires a volume to add!">>, IToken)
    end.

handle_volume_add(VolumeName, Member, IToken) ->
    CleanName = clean_name(VolumeName),
    case comic_repository:get_volume(#{name => CleanName}, false) of
        {ok, Volume} ->
            #{<<"name">> := ActualName, <<"id">> := VolumeId} = Volume,
            user_db:add_user_volume(member_to_user_id(Member), VolumeId),
            send_interaction_reply(<<"added \"", ActualName/binary, "\"">>,
                                   IToken);
        {error, {multiple_results, Volumes}} ->
            Subset = lists:sublist(lists:sort(fun start_year_sort/2,
                                              Volumes),
                                   ?MAX_SELECT_ELEMENTS),
            volume_select_modal(Subset, IToken);
        {error, not_found} ->
            send_interaction_reply(
              <<"could not find volume \"", CleanName/binary, "\"">>,
              IToken);
        {error, Err} -> ?LOG_ERROR("volume add error: ~p", [Err]),
            send_interaction_reply(<<"an unexpected error occurred">>, IToken)
    end.

send_interaction_reply(Msg, IToken) ->
    send_interaction_reply(Msg, ?DEFAULT_FLAGS, IToken).

member_to_user_id(#{<<"user">> := #{<<"id">> := UserId}}) -> UserId.

send_interaction_reply(Msg, Flags, {InteractionId, InteractionToken}) ->
    Reply = #{
              type => 4,
              data => #{
                        content => Msg,
                        flags => Flags
                       }
             },
    discord_api:interaction_callback(
      InteractionId,
      InteractionToken,
      Reply
     ).

volume_select_modal(Volumes, {InteractionId, InteractionToken}) ->
    Reply = #{
        type => 9,
        data => #{
            custom_id => <<"volume_select_modal">>,
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

build_volume_option(#{<<"name">> := Name,
                      <<"id">> := Id,
                      <<"start_year">> := StartYear}) ->
    SafeName = if size(Name) > 90 ->
                      Head = binary:part(Name, 0, 87),
                      <<Head/binary, "...">>;
                  true -> Name
               end,
    FullName = <<SafeName/binary, " (", StartYear/binary, ")">>,
    #{label => FullName,
      value => Id
     }.

start_year_sort(#{<<"start_year">> := A}, #{<<"start_year">> := B}) ->
    A > B.

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
