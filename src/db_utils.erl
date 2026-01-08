-module(db_utils).

-include_lib("kernel/include/logger.hrl").

-export([install/2]).

install(Nodes, Tables) ->
    case mnesia:create_schema(Nodes) of
        ok -> ok;
        {atomic, ok} -> ok;
        {error, {_, {already_exists, _}}} -> ok
    end,
    {_, []} = rpc:multicall(Nodes, application, start, [mnesia]),
    lists:foreach(fun({Name, Config}) ->
            case mnesia:create_table(Name, Config) of
                {atomic, ok} ->
                    ?LOG_INFO("database ~p created", [Name]);
                {aborted, {already_exists, Name}} ->
                    ?LOG_INFO("database ~p already exists", [Name])
            end
        end, maps:to_list(Tables)),
    {_, []} = rpc:multicall(Nodes, application, stop, [mnesia]),
    ok.
