-module(discord_commands).

-include_lib("kernel/include/logger.hrl").

-export([install/2]).

-define(SUBCOMMAND_TYPE, 1).
-define(SUBCOMMAND_GROUP_TYPE, 2).
-define(STRING_INPUT_TYPE, 3).
-define(INTEGER_INPUT_TYPE, 4).

install(ApplicationId, BotToken) ->
    application:ensure_all_started(gun),
    discord_api:start_link(BotToken),
    register_commands(ApplicationId).

register_commands(ApplicationId) ->
    Results = lists:map(fun(Cmd) ->
                                discord_api:register_command(ApplicationId, Cmd)
                        end, command_list()),
    lists:foreach(fun({error, {Err, Code, Response}}) ->
                          ?LOG_ERROR("~p[~p]: ~s", [Err, Code, Response]),
                          throw(Err);
                     ({error, Err}) -> throw(Err);
                     (_) -> ok
                  end, Results),
    ?LOG_INFO("successfully added all discord commmands").

command_list() ->
    [comictrack_command()].

comictrack_command() ->
    command(<<"comictrack">>,
            <<"Commands related to comictrack">>,
            [volume_subcommand_group(),
             unread_subcommand()
            ]).

%% volume commands

volume_subcommand_group() ->
    subcommand_group(<<"volume">>,
                     <<"Volume releated commands">>,
                     [volume_add_subcommand(),
                      volume_get_subcommand(),
                      volume_list_subcommand(),
                      volume_read_subcommand()
                     ]).

volume_add_subcommand() ->
    subcommand(<<"add">>,
               <<"Add a volume to track">>,
               [string_input(<<"name">>,
                             <<"The volume to add">>,
                             #{required => true})
               ]).

volume_get_subcommand() ->
    subcommand(<<"get">>,
               <<"View the details of a volume">>,
               [string_input(<<"name">>,
                             <<"The volume to view">>,
                             #{required => true})
               ]).

volume_read_subcommand() ->
    subcommand(<<"read">>,
               <<"Mark issues as read">>,
               [string_input(<<"name">>,
                             <<"The volume to update">>,
                             #{required => true})
               ]).

volume_list_subcommand() ->
    subcommand(<<"list">>,
               <<"View all tracked volumes">>).

%% unread commands

unread_subcommand() ->
    subcommand(<<"unread">>, <<"View all unread issues">>).

%% helper methods

command(Name, Description, Options) ->
    #{name => Name,
      description => Description,
      options => Options
     }.

subcommand_group(Name, Description, Options) ->
    #{name => Name,
      description => Description,
      type => ?SUBCOMMAND_GROUP_TYPE,
      options => Options
     }.

subcommand(Name, Description) ->
    #{name => Name,
      description => Description,
      type => ?SUBCOMMAND_TYPE
     }.

subcommand(Name, Description, Options) ->
    #{name => Name,
      description => Description,
      type => ?SUBCOMMAND_TYPE,
      options => Options
     }.

string_input(Name, Description, Options) ->
    build_input(?STRING_INPUT_TYPE, Name, Description, Options).

build_input(Type, Name, Description, Options) ->
    Required = maps:get(required, Options, false),
    #{name => Name,
      description => Description,
      type => Type,
      required => Required
     }.
