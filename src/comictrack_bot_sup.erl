%%%-------------------------------------------------------------------
%% @doc comictrack_bot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(comictrack_bot_sup).

-behaviour(supervisor).

-export([start_link/3]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(DiscordBotToken, ComicvineApiKey, MidtownHost) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE,
                          [DiscordBotToken, ComicvineApiKey, MidtownHost]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([DiscordBotToken, ComicvineApiKey, MidtownHost]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 3,
        period => 600
    },
    ChildSpecs = [
        #{id => discord_sup,
          start => {discord_sup, start_link, [DiscordBotToken]},
          type => supervisor
         },
        #{id => comicvine_sup,
          start => {comicvine_sup, start_link, [ComicvineApiKey]},
          type => supervisor
         },
        #{id => midtown_sup,
          start => {midtown_sup, start_link, [MidtownHost]},
          type => supervisor
         }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
