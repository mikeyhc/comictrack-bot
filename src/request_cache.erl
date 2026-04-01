%% NOTE: we don't actually expire yet, but it was easier to add it to the API
%% now then retrofit it later

-module(request_cache).

-export([install/1]).
-export([store_entry/2, get_entry/1]).

-define(ID_SIZE, 8).

-record(request_cache_entry, {id     :: binary(),
                              data   :: any(),
                              expiry :: calendar:datetime()
                             }).

-spec install([node()]) -> ok.
install(Nodes) ->
    Tables = #{
        request_cache_entry => [{attributes,
                                 record_info(fields, request_cache_entry)},
                                {type, set},
                                {ram_copies, Nodes}]
    },
    db_utils:install(Nodes, Tables).

-spec store_entry(Data :: any(), ExpirySeconds :: integer()) -> binary().
store_entry(Data, ExpirySeconds) ->
    Id = binary:encode_hex(rand:bytes(?ID_SIZE)),
    Expiry =  system_now() + ExpirySeconds,
    Record = #request_cache_entry{id=Id, data=Data, expiry=Expiry},
    mnesia:activity(transaction, fun() -> mnesia:write(Record) end),
    Id.

-spec get_entry(Id :: binary()) -> {ok, any()} | {error, not_found | expired}.
get_entry(Id) ->
    Entries = mnesia:activity(transaction,
                              fun() ->
                                      mnesia:read(request_cache_entry, Id)
                              end),
    case Entries of
        [] -> {error, not_found};
        [Entry] ->
            Expiry = Entry#request_cache_entry.expiry,
            Now = system_now(),
            if Expiry - Now < 0 -> {error, expired};
               true -> {ok, Entry#request_cache_entry.data}
            end;
        [_|_] -> throw({error, invalid_state})
    end.

system_now() ->
            calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
