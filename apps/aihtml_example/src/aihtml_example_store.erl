%% @doc The demo's data layer: a public ETS table standing in for a
%% database. With several nodes, use Mnesia or a real database instead.
-module(aihtml_example_store).

-export([init/0, bump/0, reset/0, counter/0, todos/0, todo/1, add_todo/1,
         toggle_todo/1, delete_todo/1]).

-export_type([todo/0]).

-type todo() :: #{id := pos_integer(), text := binary(), done := boolean()}.

-define(T, aihtml_example_store).

-spec init() -> ok.
init() ->
    ?T = ets:new(?T, [named_table, public, ordered_set]),
    true = ets:insert(?T, [{counter, 0}, {seq, 0}]),
    _ = [add_todo(T) || T <- [<<"Write pages as Erlang calls">>,
                              <<"Switch the four theme axes">>,
                              <<"Swap fragments with jQuery">>]],
    ok.

-spec bump() -> integer().
bump() -> ets:update_counter(?T, counter, 1).

-spec reset() -> ok.
reset() ->
    true = ets:insert(?T, {counter, 0}),
    ok.

-spec counter() -> integer().
counter() -> ets:lookup_element(?T, counter, 2).

-spec todos() -> [todo()].
todos() ->
    [to_map(T) || T <- ets:select(?T, [{{{todo, '_'}, '_', '_'}, [], ['$_']}])].

-spec todo(pos_integer()) -> todo() | undefined.
todo(Id) ->
    case ets:lookup(?T, {todo, Id}) of
        [T] -> to_map(T);
        [] -> undefined
    end.

-spec add_todo(binary()) -> todo().
add_todo(Text) ->
    Id = ets:update_counter(?T, seq, 1),
    true = ets:insert(?T, {{todo, Id}, Text, false}),
    #{id => Id, text => Text, done => false}.

-spec toggle_todo(pos_integer()) -> todo() | undefined.
toggle_todo(Id) ->
    case todo(Id) of
        undefined -> undefined;
        #{done := D} = T ->
            true = ets:update_element(?T, {todo, Id}, {3, not D}),
            T#{done := not D}
    end.

-spec delete_todo(pos_integer()) -> ok.
delete_todo(Id) ->
    true = ets:delete(?T, {todo, Id}),
    ok.

to_map({{todo, Id}, Text, Done}) -> #{id => Id, text => Text, done => Done}.
