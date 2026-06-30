-module(message_engine).
-behaviour(discord_gateway).

-include("ui_prefixes.hrl").
-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).
-export([process/2, process_application_command/3, process_modal_submit/3,
         process_message_component/3]).

-define(MAX_SELECT_ELEMENTS, 20).
-define(MAX_RESULTS, 10).

-define(CACHE_DURATION, (15 * 60)). % 15 minutes

start_link(BotToken) ->
    discord_gateway:start_link(?MODULE, BotToken).

process(Msg, _Context) -> throw({not_implemented, Msg}).

process_application_command(#{<<"name">> := Cmd, <<"options">> := Options},
                            _Message, Context)->
    handle_command(Cmd, Options, Context).

process_modal_submit(
           #{<<"custom_id">> := CustomId, <<"components">> := Components},
           _Message, Context) ->
    handle_modal_reply(CustomId, Components, Context).

process_message_component(
           #{<<"component_type">> := 2, <<"custom_id">> := Id},
           Message, Context) ->
    handle_button_press(Id, Message, Context);
process_message_component(
           #{<<"component_type">> := 3, <<"custom_id">> := Id,
             <<"values">> := Values},
           _Message, Context) ->
    handle_string_select(Id, Values, Context).

handle_command(<<"comictrack">>, [Option], Context) ->
    case maps:get(<<"name">>, Option) of
        <<"volume">> ->
            #{<<"options">> := [SubCommand]} = Option,
            #{<<"name">> := SubCommandName} = SubCommand,
            handle_volume_command(SubCommandName, SubCommand, Context);
        <<"unread">> ->
            #{<<"options">> := [SubCommand]} = Option,
            #{<<"name">> := SubCommandName} = SubCommand,
            handle_unread_command(SubCommandName, SubCommand, Context);
        OptionName ->
            discord_interaction:reply(
              <<"unknown command: ", OptionName/binary>>,
              Context)
    end;
handle_command(Cmd, _Options, Context) ->
    discord_interaction:reply(<<"unknown command: ", Cmd/binary>>, Context).

handle_modal_reply(<<"volume_select_modal_add">>, [Select], Context) ->
    #{<<"component">> := #{<<"values">> := [VolumeId]}} = Select,
    {ok, Volume} = comic_repository:lookup_volume(#{id => VolumeId}),
    user_db:add_user_volume(discord_context:user_id(Context), VolumeId),
    #{<<"name">> := VolumeName} = Volume,
    discord_interaction:reply(<<"added \"", VolumeName/binary, "\"">>, Context);
handle_modal_reply(<<"volume_select_modal_get">>, [Select], Context) ->
    #{<<"component">> := #{<<"values">> := [VolumeId]}} = Select,
    {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
    UserIssueIds = user_db:get_user_issues(discord_context:user_id(Context)),
    #{VolumeId := ReadIssues} = UserIssueIds,
    VolumeComponent = comictrack_ui:volume_view(Volume, ReadIssues, 1),
    discord_interaction:component_reply(VolumeComponent, Context);
handle_modal_reply(<<"volume_select_modal_read_", AllBinary/binary>>, [Select],
                   Context) ->
    #{<<"component">> := #{<<"values">> := [VolumeId]}} = Select,
    ReadAll = if AllBinary =:= <<"true">> -> true; true -> false end,
    {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
    if ReadAll ->
           #{<<"name">> := ActualName,
             <<"issues">> := Issues,
             <<"id">> := VolumeId} = Volume,
           UserId = discord_context:user_id(Context),
           lists:foreach(
             fun(#{<<"id">> := IssueId}) ->
                     user_db:add_user_issue(UserId, VolumeId, IssueId)
             end,
             Issues),
           discord_interaction:reply(<<"Marked all issues of \"",
                                       ActualName/binary, "\" as read">>,
                                     Context);
       true ->
           VolumeRead = generate_volume_read(Volume, Context),
           discord_interaction:component_reply(VolumeRead, Context)
    end;
handle_modal_reply(<<"volume_select_modal_remove">>, [Select], Context) ->
    #{<<"component">> := #{<<"values">> := [VolumeId]}} = Select,
    {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
    user_db:remove_user_volume(discord_context:user_id(Context), VolumeId),
    #{<<"name">> := VolumeName} = Volume,
    discord_interaction:reply(<<"removed volume \"", VolumeName/binary, "\"">>,
                              Context).

handle_button_press(<<?VOLUME_PAGE_PREFIX, PageBin/binary>>,
                    _Message, Context) ->
    Page = binary_to_integer(PageBin),
    VolumeIds = user_db:get_user_volumes(discord_context:user_id(Context)),
    VolumeList = comictrack_ui:volume_list(ids_to_volumes(VolumeIds), Page),
    discord_interaction:update(VolumeList, Context);
handle_button_press(<<?VOLUME_VIEW_PAGE_PREFIX, Rest/binary>>,
                    _Message, Context) ->
    [VolumeId, PageBin] = string:split(Rest, <<"_">>),
    Page = binary_to_integer(PageBin),
    {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
    UserIssueIds = user_db:get_user_issues(discord_context:user_id(Context)),
    #{VolumeId := ReadIssues} = UserIssueIds,
    VolumeComponent = comictrack_ui:volume_view(Volume, ReadIssues, Page),
    discord_interaction:update(VolumeComponent, Context);
handle_button_press(<<?ISSUE_PAGE_PREFIX, PageBin/binary>>,
                    _Message, Context) ->
    Page = binary_to_integer(PageBin),
    UserId = discord_context:user_id(Context),
    Unread = comictrack_unread:get_user_unread_issues(UserId),
    IssueList = comictrack_ui:unread_issue_list(Unread, Page),
    discord_interaction:update(IssueList, Context);
handle_button_press(<<?ISSUE_READ_PAGE_PREFIX, Rest/binary>>,
                    _Message, Context) ->
    case Rest of
        <<"v", Rest0/binary>> ->
            [VolumeId, PageBin] = string:split(Rest0, <<"_">>),
            Page = binary_to_integer(PageBin),
            {ok, Volume} = comic_repository:get_volume(#{id => VolumeId}),
            VolumeRead = generate_volume_read(Volume, Context, Page),
            discord_interaction:update(VolumeRead, Context);
        <<"u_", Rest0/binary>> ->
            [CacheId, PageBin] = string:split(Rest0, <<"_">>),
            ?LOG_DEBUG("fetching cached results: ~s~n", [CacheId]),
            UserId = discord_context:user_id(Context),
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
                    discord_interaction:update(UnreadReply, Context);
                {error, Error} ->
                    ?LOG_WARNING("failed to get cache results[~s]: ~p",
                                 [CacheId, Error]),
                    Reply = comictrack_ui:string_reply(
                              <<"This message has expired">>),
                    discord_interaction:update(Reply, Context)
            end
    end.

handle_string_select(<<?ISSUE_READ_PREFIX, Rest/binary>>,
                     [Value], Context) ->
    [VolumeId, IssueId] = string:split(Rest, <<"_">>),
    UserId = discord_context:user_id(Context),
    case Value of
        <<"read">> -> user_db:add_user_issue(UserId, VolumeId, IssueId);
        <<"unread">> -> user_db:remove_user_issue(UserId, VolumeId, IssueId)
    end,
    discord_interaction:pong(Context).

handle_volume_command(<<"add">>, Option, Context) ->
    #{<<"options">> := Arguments} = Option,
    case Arguments of
        [#{<<"name">> := <<"name">>,
           <<"value">> := VolumeName}] ->
            handle_volume_add(VolumeName, Context);
        _ -> discord_interaction:reply(<<"requires a volume to add!">>, Context)
    end;
handle_volume_command(<<"remove">>, Option, Context) ->
    #{<<"options">> := Arguments} = Option,
    case Arguments of
        [#{<<"name">> := <<"name">>,
           <<"value">> := VolumeName}] ->
            handle_volume_remove(VolumeName, Context);
        _ -> discord_interaction:reply(<<"requires a volume to add!">>, Context)
    end;
handle_volume_command(<<"get">>, Option, Context) ->
    #{<<"options">> := Arguments} = Option,
    case Arguments of
        [#{<<"name">> := <<"name">>,
           <<"value">> := VolumeName}] ->
            handle_volume_get(VolumeName, Context);
        _ -> discord_interaction:reply(<<"requires a volume to get!">>, Context)
    end;
handle_volume_command(<<"list">>, _Option, Context) ->
    handle_volume_list(Context);
handle_volume_command(<<"read">>, Option, Context) ->
    #{<<"options">> := ArgList} = Option,
    Arguments = lists:foldl(fun(A=#{<<"name">> := N}, Acc) -> Acc#{N => A} end,
                            #{}, ArgList),
    case Arguments of
        #{<<"name">> := #{<<"value">> := VolumeName}} ->
            ?LOG_INFO("Args: ~p", [Arguments]),
            #{<<"value">> := All} = maps:get(<<"all">>, Arguments,
                                             #{<<"value">> => false}),
            handle_volume_read(VolumeName, All, Context);
        _ -> discord_interaction:reply(<<"requires a volume to read!">>,
                                       Context)
    end.

handle_unread_command(<<"list">>, _Option, Context) ->
    handle_unread_issue_list(Context);
handle_unread_command(<<"read">>, _Option, Context) ->
    handle_unread_read_list(Context).

handle_volume_add(VolumeName, Context) ->
    CleanName = clean_name(VolumeName),
    case comic_repository:lookup_volume(#{name => CleanName}) of
        {ok, Volume} ->
            #{<<"name">> := ActualName, <<"id">> := VolumeId} = Volume,
            user_db:add_user_volume(discord_context:user_id(Context), VolumeId),
            discord_interaction:reply(<<"added \"", ActualName/binary, "\"">>,
                                      Context);
        {error, {multiple_results, Volumes}} ->
            Subset = lists:sublist(lists:sort(
                                     fun comic_volume:start_year_sort/2,
                                     Volumes),
                                   ?MAX_SELECT_ELEMENTS),
            volume_select_reply(<<"add">>, Subset, Context);
        {error, not_found} ->
            discord_interaction:reply(
              <<"could not find volume \"", CleanName/binary, "\"">>,
              Context)
    end.


handle_volume_remove(VolumeName, Context) ->
    CleanName = clean_name(VolumeName),
    UserId = discord_context:user_id(Context),
    {_Oks, UserVolumes} = lists:unzip(
                            lists:map(
                              fun(V) ->
                                      comic_repository:get_volume(#{id => V})
                              end,
                              user_db:get_user_volumes(UserId))),
    case lists:filter(comic_volume:name_fuzzymatcher(CleanName), UserVolumes) of
        [] ->
            discord_interaction:reply(
              <<"could not find volume \"", CleanName/binary, "\"">>,
              Context);
        [#{<<"id">> := VolumeId, <<"name">> := ActualName}] ->
            UserId = discord_context:user_id(Context),
            user_db:remove_user_volume(UserId, VolumeId),
            discord_interaction:reply(
              <<"removed volume \"", ActualName/binary, "\"">>,
              Context);
        Volumes ->
            case exact_match_volume(CleanName, Volumes) of
                [#{<<"id">> := VolumeId, <<"name">> := ActualName}] ->
                    UserId = discord_context:user_id(Context),
                    user_db:remove_user_volume(UserId, VolumeId),
                    discord_interaction:reply(
                      <<"removed volume \"", ActualName/binary, "\"">>,
                      Context);
                _ ->
                    Subset = lists:sublist(lists:sort(
                                             fun comic_volume:start_year_sort/2,
                                             Volumes),
                                           ?MAX_SELECT_ELEMENTS),
                    volume_select_reply(<<"remove">>, Subset, Context)
            end
    end.

handle_volume_get(VolumeName, Context) ->
    CleanName = clean_name(VolumeName),
    UserId = discord_context:user_id(Context),
    {_Oks, UserVolumes} = lists:unzip(
                            lists:map(
                              fun(V) ->
                                      comic_repository:get_volume(#{id => V})
                              end,
                              user_db:get_user_volumes(UserId))),
    case lists:filter(comic_volume:name_fuzzymatcher(CleanName), UserVolumes) of
        [] ->
            discord_interaction:reply(
              <<"could not find volume \"", CleanName/binary, "\"">>,
              Context);
        [Volume=#{<<"id">> := VolumeId}] ->
            UserIssueIds = user_db:get_user_issues(UserId),
            #{VolumeId := ReadIssues} = UserIssueIds,
            VolumeComponent = comictrack_ui:volume_view(Volume, ReadIssues, 1),
            discord_interaction:component_reply(VolumeComponent, Context);
        Volumes ->
            case exact_match_volume(CleanName, Volumes) of
                [Volume=#{<<"id">> := VolumeId}] ->
                    UserIssueIds = user_db:get_user_issues(UserId),
                    #{VolumeId := ReadIssues} = UserIssueIds,
                    VolumeComponent = comictrack_ui:volume_view(Volume, ReadIssues, 1),
                    discord_interaction:component_reply(VolumeComponent, Context);
                _ ->
                    Subset = lists:sublist(lists:sort(
                                             fun comic_volume:start_year_sort/2,
                                             Volumes),
                                           ?MAX_SELECT_ELEMENTS),
                    volume_select_reply(<<"get">>, Subset, Context)
            end
    end.

handle_volume_read(VolumeName, ReadAll, Context) ->
    CleanName = clean_name(VolumeName),
    UserId = discord_context:user_id(Context),
    {_Oks, UserVolumes} = lists:unzip(
                            lists:map(
                              fun(V) ->
                                      comic_repository:get_volume(#{id => V})
                              end,
                              user_db:get_user_volumes(UserId))),
    case lists:filter(comic_volume:name_fuzzymatcher(CleanName), UserVolumes) of
        [] ->
            discord_interaction:reply(
              <<"could not find volume \"", CleanName/binary, "\"">>,
              Context);
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
                   discord_interaction:reply(
                     <<"Marked all issues of \"",
                       ActualName/binary, "\" as read">>,
                     Context);
               true ->
                   VolumeRead = generate_volume_read(Volume, Context),
                   discord_interaction:component_reply(VolumeRead, Context)
            end;
        Volumes ->
            Subset = lists:sublist(lists:sort(
                                     fun comic_volume:start_year_sort/2,
                                     Volumes),
                                   ?MAX_SELECT_ELEMENTS),
            ReadAllBool = if ReadAll -> <<"true">>; true -> <<"false">> end,
            volume_select_reply(<<"read_", ReadAllBool/binary>>, Subset,
                                Context)
    end.

handle_volume_list(Context) ->
    handle_volume_list(Context, 1).

handle_volume_list(Context, Page) ->
    VolumeIds = user_db:get_user_volumes(discord_context:user_id(Context)),
    case VolumeIds of
        [] ->
            discord_interaction:reply(<<"you aren't tracking any volumes :(">>,
                                      Context);
        _ ->
            Volumes = ids_to_volumes(VolumeIds),
            VolumeList = comictrack_ui:volume_list(Volumes, Page),
            discord_interaction:component_reply(VolumeList, Context)
    end.

handle_unread_issue_list(Context) ->
    UserId = discord_context:user_id(Context),
    VolumeIds = user_db:get_user_volumes(UserId),
    case VolumeIds of
        [] ->
            discord_interaction:reply(<<"you aren't tracking any volumes :(">>,
                                      Context);
        _ ->
            Unread = comictrack_unread:get_user_unread_issues(UserId),
            IssueList = comictrack_ui:unread_issue_list(Unread, 1),
            discord_interaction:component_reply(IssueList, Context)
    end.

handle_unread_read_list(Context) ->
    UserId = discord_context:user_id(Context),
    VolumeIds = user_db:get_user_volumes(UserId),
    case VolumeIds of
        [] ->
            discord_interaction:reply(<<"you aren't tracking any volumes :(">>,
                                      Context);
        _ ->
            Unread = comictrack_unread:get_user_unread_issues(UserId),
            CacheId = request_cache:store_entry(Unread, ?CACHE_DURATION),
            Reply = comictrack_ui:read_select(<<"u_", CacheId/binary, "_">>,
                                              Unread, sets:new(), 1),
            discord_interaction:component_reply(Reply, Context)
    end.

generate_volume_read(Volume, Context) ->
    generate_volume_read(Volume, Context, 1).

generate_volume_read(V=#{<<"id">> := VolumeId,
                         <<"name">> := ActualName,
                         <<"start_year">> := StartYear,
                         <<"issues">> := Issues},
                    Context,
                    Page) ->
    UserIssueIds = user_db:get_user_issues(discord_context:user_id(Context)),
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

volume_select_reply(Id, Volumes, Context) ->
    Modal = comictrack_ui:volume_select_components(Volumes),
    discord_interaction:modal_reply(<<"volume_select_modal_", Id/binary>>,
                                    <<"Select Volume">>,
                                    Modal,
                                    Context).

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
