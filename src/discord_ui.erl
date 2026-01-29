-module(discord_ui).

-export([volume_select_modal/2, page_controls/3, read_select/4]).

-export_type([discord_modal/0, discord_component/0]).

% reply codes
-define(MODAL_REPLY, 9).

% component types
-define(ACTION_ROW, 1).
-define(BUTTON, 2).
-define(STRING_SELECT, 3).
-define(TEXT_DISPLAY, 10).
-define(LABEL, 18).

% button styles
-define(BUTTON_PRIMARY, 1).

% limits
-define(NAME_OPTION_LENGTH, 50).
-define(MAX_RESULTS, 10).


-type discord_component() :: #{}.
-type discord_modal() :: #{}.

-spec volume_select_modal(binary(), [comic_volume:volume()]) ->
    discord_modal().
volume_select_modal(Id, Volumes) ->
    VolumeOptions = lists:map(fun build_volume_option/1, Volumes),
    #{type => ?MODAL_REPLY,
      data => #{custom_id => <<"volume_select_modal_", Id/binary>>,
                title => <<"Select Volume">>,
                components => [label(<<"Which volume">>,
                                     string_select(<<"volume_name_select">>,
                                                   <<"Choose...">>,
                                                   VolumeOptions))]}}.

-spec page_controls(binary(), non_neg_integer(), non_neg_integer()) ->
    [discord_component()].
page_controls(Prefix, Page, MaxPage) ->
    BinPage = integer_to_binary(Page),
    BinMaxPage = integer_to_binary(MaxPage),
    PageFooter = <<"(page ", BinPage/binary, " of ",
                   BinMaxPage/binary, ")">>,
    PrevPageBin = integer_to_binary(Page - 1),
    NextPageBin = integer_to_binary(Page + 1),
    PrevPageLink = <<Prefix/binary, "_", PrevPageBin/binary>>,
    NextPageLink = <<Prefix/binary, "_", NextPageBin/binary>>,
    [text_display(PageFooter),
               action_row([button(?BUTTON_PRIMARY,
                                  <<"Previous Page">>,
                                  PrevPageLink,
                                  #{disabled => Page =:= 1}),
                           button(?BUTTON_PRIMARY,
                                  <<"Next Page">>,
                                  NextPageLink,
                                  #{disabled => Page >= MaxPage})
                          ])].

read_select(PagePrefix, Issues, ReadIssues, Page) ->
    Builder = fun(I=#{<<"id">> := Id,
                      <<"volume">> := #{<<"id">> := VolumeId}}) ->
                      BinId = integer_to_binary(Id),
                      BinVolId = integer_to_binary(VolumeId),
                      Cid = <<"ir_", BinVolId/binary, "_", BinId/binary>>,
                      Read = sets:is_element(Id, ReadIssues),
                      Default = if Read -> <<"Read">>;
                                   true -> <<"Unread">>
                                end,
                      [text_display(comic_issue:full_name(I)),
                       action_row([string_select(Cid,
                                                 Default,
                                                 read_options())
                                  ])]
              end,
    Truncated = lists:sublist(Issues, (Page - 1) * ?MAX_RESULTS + 1,
                              ?MAX_RESULTS),
    Reply0 = lists:flatten(lists:map(Builder, Truncated)),
    if length(Issues) > ?MAX_RESULTS ->
           Prefix = <<"irp_", PagePrefix/binary>>,
           Reply0 ++ discord_ui:page_controls(Prefix, Page,
                                              count_pages(Issues));
       true -> Reply0
    end.

% component functions
action_row(Components) ->
    #{type => ?ACTION_ROW,
      components => Components
     }.

button(Style, Label, CustomId, Options) ->
    #{type => ?BUTTON,
      style => Style,
      label => Label,
      custom_id => CustomId,
      disabled => maps:get(disabled, Options, false)
     }.

string_select(Id, Placeholder, Options) ->
    #{type => ?STRING_SELECT,
      custom_id => Id,
      placeholder => Placeholder,
      options => Options
     }.

text_display(Content) ->
    #{type => ?TEXT_DISPLAY,
      content => Content
     }.

label(Text, Component) ->
    #{type => ?LABEL,
      label => Text,
      component => Component
     }.

% helper methods
build_volume_option(Volume=#{<<"id">> := Id}) ->
    #{label => comic_volume:full_name(Volume,
                                      #{name_length => ?NAME_OPTION_LENGTH}),
      value => Id
     }.

count_pages(Input) ->
    round(math:ceil(length(Input) / ?MAX_RESULTS)).

read_options() ->
    [#{label => <<"Read">>,
       value => <<"read">>
      },
     #{label => <<"Unread">>,
       value => <<"unread">>
      }
    ].
