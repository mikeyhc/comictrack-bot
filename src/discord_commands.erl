-module(discord_commands).

-export([register_commands/1]).

register_commands(ApplicationId) ->
    Results = lists:map(fun(Cmd) ->
                                discord_api:register_command(ApplicationId, Cmd)
                        end, command_list()),
    lists:foreach(fun({error, Err}) -> throw(Err);
                     (_) -> ok
                  end, Results).

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
          type => 1
         }
      ]
     }.
