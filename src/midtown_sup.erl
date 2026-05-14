%%%-------------------------------------------------------------------
%% @doc midtown process supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(midtown_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(MidtownHost) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [MidtownHost]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([MidtownHost]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 60
    },
    ChildSpecs = [
        #{id => id_generator,
          start => {id_generator, start_link, []}
         },
        #{id => midtown_api,
          start => {midtown_api, start_link, [MidtownHost]}
         }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
