-module(discord_commands).

-include_lib("kernel/include/logger.hrl").

-export([install/2]).

install(ApplicationId, BotToken) ->
    application:ensure_all_started(gun),
    discord_api:start_link(BotToken),
    register_commands(ApplicationId).

register_commands(ApplicationId) ->
    Results = lists:map(fun(Cmd) ->
                                discord_api:register_command(ApplicationId, Cmd)
                        end, command_list()),
    lists:foreach(fun({error, Err}) -> throw(Err);
                     (_) -> ok
                  end, Results),
    ?LOG_INFO("successfully added all discord commmands").

command_list() ->
    [comictrack_command()].

comictrack_command() ->
    #{name => <<"comictrack">>,
      description => <<"Commands related to comictrack">>,
      options => [
        volume_subcommand()
      ]
     }.

volume_subcommand() ->
    #{name => <<"volume">>,
      description => <<"Volume releated commands">>,
      type => 2,
      options => [
        #{name => <<"add">>,
          description => <<"Add a volume to track">>,
          type => 1,
          options => [
            #{name => <<"name">>,
              description => <<"The volume to add">>,
              type => 3,
              required => true
            }
          ]
         }
      ]
     }.
