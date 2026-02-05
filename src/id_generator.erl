-module(id_generator).

-behaviour(gen_server).

%  Public API
-export([install/1, start_link/0, get_id/2, get_volume_id/1, get_issue_id/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER_NAME, ?MODULE).

-define(FIRST_ID, 1).

-record(idgen_entry, {key :: {any(), any()},
                      id  :: non_neg_integer()
                     }).

-record(state, {lookup   :: maps:map(),
                next_idx :: non_neg_integer()
               }).

% Public API

-spec install([node()]) -> ok.
install(Nodes) ->
    Tables = #{
        idgen_entry => [{attributes, record_info(fields, idgen_entry)},
                        {disc_copies, Nodes}]
    },
    db_utils:install(Nodes, Tables).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

-spec get_id(any(), any()) -> {ok, non_neg_integer()}.
get_id(Namespace, Name) ->
    gen_server:call(?SERVER_NAME, {get_id, Namespace, Name}).

-spec get_volume_id(binary()) -> {ok, non_neg_integer()}.
get_volume_id(VolumeName) ->
    get_id(volume, VolumeName).

-spec get_issue_id(non_neg_integer(), non_neg_integer()) ->
    {ok, non_neg_integer()}.
get_issue_id(VolumeId, IssueNumber) ->
    get_id({issue, VolumeId}, IssueNumber).

% gen_server callbacks

init([]) ->
    gen_server:cast(self(), load),
    {ok, #state{}}.

handle_call({get_id, Namespace, Name}, _From, State0) ->
    Key = build_key(Namespace, Name),
    Id = get_id_from_state(Key, State0),
    State = update_id(Key, Id, State0),
    {reply, {ok, Id}, State}.

handle_cast(load, _State) ->
    Fold = fun(#idgen_entry{key=Key, id=Id}, {Max, Lookup0}) ->
                   Lookup = Lookup0#{Key => Id},
                   if Id > Max -> {Id, Lookup};
                      true -> {Max, Lookup}
                   end
           end,
    Fun = fun() -> mnesia:foldl(Fold, {0, #{}}, idgen_entry) end,
    {Idx, Lookup} = mnesia:activity(transaction, Fun),
    {noreply, #state{next_idx=Idx, lookup=Lookup}}.

% helper functions

get_id_from_state(Key, #state{lookup=Lookup, next_idx=NextIdx}) ->
    case maps:get(Key, Lookup, false) of
        false -> NextIdx + 1;
        Idx -> Idx
    end.

build_key(Namespace, Name) -> {Namespace, Name}.

update_id(Key, Id, State=#state{next_idx=NextIdx, lookup=Lookup}) ->
    Record = #idgen_entry{key=Key, id=Id},
    Fun = fun() -> mnesia:write(Record) end,
    ok = mnesia:activity(transaction, Fun),
    State#state{next_idx=if Id > NextIdx -> Id; true -> NextIdx end,
                lookup=Lookup#{Key => Id}}.
