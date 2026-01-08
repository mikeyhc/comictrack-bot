%%%-------------------------------------------------------------------
%% @doc discord process supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(discord_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(BotToken) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [BotToken]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([BotToken]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 3,
        period => 60
    },
    ChildSpecs = [
        #{id => discord_api,
          start => {discord_api, start_link, [BotToken]}
         }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
