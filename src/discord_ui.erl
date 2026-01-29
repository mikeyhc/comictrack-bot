-module(discord_ui).

-export([modal_reply/3]).
-export([action_row/1, button/4, string_select/3, text_display/1, label/2]).

-export_type([discord_modal/0, discord_component/0,
              discord_string_select_entry/0, discord_button_options/0]).

% reply codes
-define(MODAL_REPLY, 9).

% component types
-define(ACTION_ROW, 1).
-define(BUTTON, 2).
-define(STRING_SELECT, 3).
-define(TEXT_DISPLAY, 10).
-define(LABEL, 18).

-type discord_component() :: #{}.
-type discord_modal() :: #{}.
-type discord_string_select_entry() :: #{label := binary(), value := binary()}.
-type discord_button_options() :: #{disabled := binary()}.

% reply functions
-spec modal_reply(binary(), binary(), [discord_component()]) -> discord_modal().
modal_reply(Id, Title, Components) ->
    #{type => ?MODAL_REPLY,
      data => #{custom_id => Id,
                title => Title,
                components => Components}}.

% component functions
-spec action_row([discord_component()]) -> discord_component().
action_row(Components) ->
    #{type => ?ACTION_ROW,
      components => Components
     }.

-spec button(non_neg_integer(), binary(), binary(),
             discord_button_options()) -> discord_component().
button(Style, Label, CustomId, Options) ->
    #{type => ?BUTTON,
      style => Style,
      label => Label,
      custom_id => CustomId,
      disabled => maps:get(disabled, Options, false)
     }.

-spec string_select(binary(), binary(), [discord_string_select_entry()]) ->
    discord_component().
string_select(Id, Placeholder, Options) ->
    #{type => ?STRING_SELECT,
      custom_id => Id,
      placeholder => Placeholder,
      options => Options
     }.

-spec text_display(binary()) -> discord_component().
text_display(Content) ->
    #{type => ?TEXT_DISPLAY,
      content => Content
     }.

-spec label(binary(), discord_component()) -> discord_component().
label(Text, Component) ->
    #{type => ?LABEL,
      label => Text,
      component => Component
     }.
