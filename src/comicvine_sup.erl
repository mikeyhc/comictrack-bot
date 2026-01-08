%%%-------------------------------------------------------------------
%% @doc comicvine process supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(comicvine_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(ApiKey) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [ApiKey]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([ApiKey]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 60
    },
    ChildSpecs = [
        #{id => comicvine_api,
          start => {comicvine_api, start_link, [ApiKey]}
         }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
