-module(comicvine_api).

-include("types.hrl").

% Public API
-export([start_link/1]).
-export([issues/1, volume/1, volumes/1]).

-define(SERVER_NAME, ?MODULE).
-define(COMICVINE_HOST, "comicvine.gamespot.com").
-define(COMICVINE_PORT, 443).
-define(DEFAULT_FORMAT, "json").
% this needs to be a well known agent
-define(USER_AGENT, "curl/8.7.1").

-define(ISSUE_TYPE, 4000).
-define(VOLUME_TYPE, 4050).

% Public API
-spec start_link(string() | binary()) -> {ok, pid()}.
start_link(Token) ->
    Configuration = #{
        host => ?COMICVINE_HOST,
        port => ?COMICVINE_PORT,
        user_agent => ?USER_AGENT,
        query_params => [
            {"api_key", Token},
            {"format", ?DEFAULT_FORMAT}
        ]
    },
    http_client:start_link(?SERVER_NAME, Configuration).


-spec volume(non_neg_integer()) -> {ok, #{}} | {error, not_found}.
volume(VolumeId) ->
    {ok, {200, Body}} = http_client:get(
                          ?SERVER_NAME,
                          build_url("/volume/~p-~p/",[?VOLUME_TYPE, VolumeId])),
    Reply = jsone:decode(Body),
    case maps:get(<<"error">>, Reply) of
        <<"OK">> -> {ok, Reply};
        <<"Object Not Found">> -> {error, not_found}
    end.

-spec volumes([{string(), string()}]) -> {ok, #{}}.
volumes(Filters) ->
    {ok, {200, Body}} = http_client:get(
                          ?SERVER_NAME,
                          build_url("/volumes/", []),
                          #{query_params => [
                            build_filter(Filters),
                            {"sort", "date_added:desc"}
                           ]}),
    Reply = jsone:decode(Body),
    #{<<"error">> := <<"OK">>} = Reply,
    {ok, Reply}.

-spec issues([{string(), string()}]) -> {ok, #{}}.
issues(Filters) ->
    {ok, {200, Body}} = http_client:get(
                          ?SERVER_NAME,
                          build_url("/issues/", []),
                          #{query_params => [build_filter(Filters)]}),
    Reply = jsone:decode(Body),
    #{<<"error">> := <<"OK">>} = Reply,
    {ok, Reply}.

% helper functions

build_filter(Filters) ->
    MapFn = fun({K, V}) -> K ++ ":" ++ V end,
    {"filter", lists:flatten(lists:join(",", lists:map(MapFn, Filters)))}.

build_url(String, Args) ->
    lists:flatten(io_lib:format("/api" ++ String, Args)).
