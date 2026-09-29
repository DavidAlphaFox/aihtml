%%%-------------------------------------------------------------------
%%% @doc The demo's data layer, in Mnesia.
%%%
%%% Two tables:
%%%
%%%   aihtml_example_counter  {Key, Value}: the page counter and the todo
%%%                           id sequence
%%%   aihtml_example_todo     {Id, Text, Done}, ordered by id
%%%
%%% Every write is a transaction, so concurrent requests on any node of the
%%% cluster see consistent data. The web layer stays stateless: the action
%%% and push code only calls the functions below.
%%%
%%% Configuration (application env of aihtml_example):
%%%
%%%   db_storage  disc_copies (default, survives restarts) or ram_copies
%%%   db_join     a node already running the demo (atom or string): this
%%%               node joins its Mnesia cluster and copies the tables.
%%%               Leave it unset or empty on the first node.
%%%
%%% Mnesia's directory defaults to _build/mnesia/<node name> unless the
%%% `dir' of the mnesia application is set.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_example_store).

-export([init/0, bump/0, reset/0, counter/0, todos/0, todo/1, add_todo/1,
         toggle_todo/1, delete_todo/1]).

-export_type([todo/0]).

-type todo() :: #{id := pos_integer(), text := binary(), done := boolean()}.

-record(aihtml_example_counter, {key :: atom(), value :: integer()}).
-record(aihtml_example_todo, {id :: pos_integer(), text :: binary(), done :: boolean()}).

-define(COUNTER, aihtml_example_counter).
-define(TODO, aihtml_example_todo).
-define(TABLES, [?COUNTER, ?TODO]).
-define(WAIT, 30000).

-define(SEED, [<<"Write pages as Erlang calls">>,
               <<"Switch the four theme axes">>,
               <<"Swap fragments from the server">>]).

%%%===================================================================
%%% Setup
%%%===================================================================

%% @doc Start Mnesia and make sure this node has the tables: create them
%% on a standalone node, or join the cluster of `db_join' and copy them.
-spec init() -> ok.
init() ->
    Storage = application:get_env(aihtml_example, db_storage, disc_copies),
    ok = set_dir(),
    case join_node() of
        none -> init_standalone(Storage);
        Node -> init_joined(Node, Storage)
    end,
    ok = mnesia:wait_for_tables(?TABLES, ?WAIT).

%% db_join may be an atom, or a string from a release's sys.config.src
%% ("" when unset).
join_node() ->
    case application:get_env(aihtml_example, db_join) of
        undefined -> none;
        {ok, undefined} -> none;
        {ok, ""} -> none;
        {ok, <<>>} -> none;
        {ok, Node} when is_atom(Node) -> Node;
        {ok, Node} when is_list(Node) -> list_to_atom(Node);
        {ok, Node} when is_binary(Node) -> binary_to_atom(Node)
    end.

init_standalone(Storage) ->
    %% A disc schema has to exist before Mnesia starts; create_schema
    %% fails harmlessly with already_exists on later starts.
    _ = Storage =:= disc_copies andalso mnesia:create_schema([node()]),
    ok = start_mnesia(),
    ensure_schema_type(Storage),
    Created = [T || T <- ?TABLES, create_table(T, Storage)],
    lists:member(?TODO, Created) andalso seed(),
    ok.

init_joined(Node, Storage) ->
    ok = start_mnesia(),
    case mnesia:change_config(extra_db_nodes, [Node]) of
        {ok, [_ | _]} -> ok;
        Other -> error({cannot_join_mnesia_cluster, Node, Other})
    end,
    ensure_schema_type(Storage),
    _ = [copy_table(T, Storage) || T <- ?TABLES],
    ok.

start_mnesia() ->
    {ok, _} = application:ensure_all_started(mnesia),
    ok.

%% disc_copies tables need the schema itself on disc on this node.
ensure_schema_type(disc_copies) ->
    case mnesia:change_table_copy_type(schema, node(), disc_copies) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, schema, _, disc_copies}} -> ok
    end;
ensure_schema_type(ram_copies) ->
    ok.

create_table(Name, Storage) ->
    Def = [{Storage, [node()]} | definition(Name)],
    case mnesia:create_table(Name, Def) of
        {atomic, ok} -> true;
        {aborted, {already_exists, Name}} -> false
    end.

copy_table(Name, Storage) ->
    case mnesia:add_table_copy(Name, node(), Storage) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, Name, _}} -> ok
    end.

definition(?COUNTER) ->
    [{type, set}, {attributes, record_info(fields, aihtml_example_counter)}];
definition(?TODO) ->
    [{type, ordered_set}, {attributes, record_info(fields, aihtml_example_todo)}].

seed() ->
    _ = [add_todo(T) || T <- ?SEED],
    ok.

%% Mnesia does not create missing parent directories.
set_dir() ->
    Dir = case application:get_env(mnesia, dir) of
              {ok, D} -> D;
              undefined ->
                  D = filename:join(["_build", "mnesia", atom_to_list(node())]),
                  ok = application:set_env(mnesia, dir, D),
                  D
          end,
    filelib:ensure_path(Dir).

%%%===================================================================
%%% Counter
%%%===================================================================

-spec bump() -> integer().
bump() -> tx(fun() -> incr(counter, 1) end).

-spec reset() -> ok.
reset() -> tx(fun() -> mnesia:write(#aihtml_example_counter{key = counter, value = 0}) end).

-spec counter() -> integer().
counter() ->
    case mnesia:dirty_read(?COUNTER, counter) of
        [#aihtml_example_counter{value = N}] -> N;
        [] -> 0
    end.

%%%===================================================================
%%% Todos
%%%===================================================================

-spec todos() -> [todo()].
todos() ->
    %% ordered_set: a full select comes back sorted by id
    [to_map(T) || T <- mnesia:dirty_select(?TODO, [{'_', [], ['$_']}])].

-spec todo(pos_integer()) -> todo() | undefined.
todo(Id) ->
    case mnesia:dirty_read(?TODO, Id) of
        [T] -> to_map(T);
        [] -> undefined
    end.

-spec add_todo(binary()) -> todo().
add_todo(Text) ->
    tx(fun() ->
           Id = incr(todo_seq, 1),
           T = #aihtml_example_todo{id = Id, text = Text, done = false},
           ok = mnesia:write(T),
           to_map(T)
       end).

-spec toggle_todo(pos_integer()) -> todo() | undefined.
toggle_todo(Id) ->
    tx(fun() ->
           case mnesia:read(?TODO, Id, write) of
               [#aihtml_example_todo{done = D} = T0] ->
                   T = T0#aihtml_example_todo{done = not D},
                   ok = mnesia:write(T),
                   to_map(T);
               [] ->
                   undefined
           end
       end).

-spec delete_todo(pos_integer()) -> ok.
delete_todo(Id) ->
    tx(fun() -> mnesia:delete({?TODO, Id}) end).

%%%===================================================================
%%% Internal
%%%===================================================================

%% Read-modify-write under a write lock, inside a transaction.
incr(Key, By) ->
    N = case mnesia:read(?COUNTER, Key, write) of
            [#aihtml_example_counter{value = V}] -> V + By;
            [] -> By
        end,
    ok = mnesia:write(#aihtml_example_counter{key = Key, value = N}),
    N.

tx(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> error({mnesia_aborted, Reason})
    end.

to_map(#aihtml_example_todo{id = Id, text = Text, done = Done}) ->
    #{id => Id, text => Text, done => Done}.
