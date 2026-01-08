%%%-------------------------------------------------------------------
%% @doc comictrack_bot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(comictrack_bot_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(DiscordBotToken) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [DiscordBotToken]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([DiscordBotToken]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 3,
        period => 600
    },
    ChildSpecs = [
        #{id => discord_sup,
          start => {discord_sup, start_link, [DiscordBotToken]},
          type => supervisor
         }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
