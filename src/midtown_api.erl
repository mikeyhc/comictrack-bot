-module(midtown_api).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1, volume_search/1]).

-define(SERVER_NAME, ?MODULE).

-spec start_link(string()) -> {ok, pid()}.
start_link(MidtownHost) ->
    Configuration = #{
        host => MidtownHost,
        port => 443
    },
    http_client:start_link(?SERVER_NAME, Configuration).

-spec volume_search(string()) -> {ok, binary()} |
                                 {error, {non_neg_integer(), binary()}}.
volume_search(Name) ->
    Options = #{query_params => [{"q", Name},
                                 {"pp", "100"},
                                 {"pj", "1"},
                                 {"os", "1"}]},
    {ok, {Code, Reply}} = http_client:get(?SERVER_NAME, "/search", Options),
    if Code >= 200 andalso Code < 300 -> {ok, Reply};
       true ->
           ?LOG_ERROR("error fetching from midtown: {~p, ~p}", [Code, Reply]),
           {error, {Code, Reply}}
    end.
