%% Tests for aihtml_tree. The module is also the fake action module of
%% the lazy tree round trip.
-module(aihtml_tree_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_tree.hrl").

-export([action/4]).

-define(M, aihtml_tree).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

%%%===================================================================
%%% tree
%%%===================================================================

files() ->
    [#{label => <<"Docs">>, value => docs, icon => <<"D">>,
       items => [{work, <<"Work">>, [<<"q1">>, <<"q2">>]},
                 #{label => <<"Personal">>, disabled => true}]},
     {dl, <<"Downloads">>},
     #{label => <<"Remote">>, value => remote, lazy => true}].

tree_structure_test() ->
    H = r(?M:ah_tree(files(), undefined, [<<"w-64">>], [{id, t}, {aria_label, <<"Files">>}])),
    ?assert(has(<<"<div class=\"ah-tree w-64\" id=\"t\" role=\"tree\" data-ah=\"tree\" "
                  "data-ah-value=\"\" data-toggle-mode=\"click\" data-animation=\"slide\" "
                  "aria-label=\"Files\">">>, H)),
    ?assert(has(<<"<ul class=\"ah-tree-list\" role=\"presentation\">">>, H)),
    %% top level: first enabled node takes the tab stop
    ?assert(has(<<"<li class=\"ah-tree-item\" id=\"t-0\" role=\"treeitem\" aria-level=\"1\" "
                  "data-tree-id=\"0\" data-value=\"docs\" aria-expanded=\"false\" tabindex=\"0\">">>, H)),
    ?assert(has(<<"<span class=\"ah-tree-icon\" aria-hidden=\"true\">D</span>">>, H)),
    ?assert(has(<<"<ul class=\"ah-tree-list\" role=\"group\" id=\"t-0-g\" style=\"display:none;\">">>, H)),
    ?assert(has(<<"id=\"t-0-0-1\" role=\"treeitem\" aria-level=\"3\" data-tree-id=\"0-0-1\" "
                  "data-value=\"q2\" tabindex=\"-1\"">>, H)),
    ?assert(has(<<"<div class=\"ah-tree-row ah-tree-row-disabled\">">>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"<li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-1\"">>, H)),
    ?assert(has(<<"<span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">"/utf8>>, H)),
    %% a lazy node: expandable, empty group, names the tree
    ?assert(has(<<"data-value=\"remote\" aria-expanded=\"false\" tabindex=\"-1\" data-lazy=\"true\" "
                  "data-tree=\"t\" data-level=\"1\"">>, H)),
    ?assert(has(<<"id=\"t-2-g\" style=\"display:none;\"></ul>">>, H)),
    ?assertNot(has_quiet(<<"data-load">>, H)).

tree_selection_test() ->
    H = r(?M:ah_tree(files(), <<"q2">>, [], [{id, t}, {name, doc}])),
    ?assert(has(<<"data-ah-value=\"q2\"">>, H)),
    %% the ancestors of the selected node are open
    ?assert(has(<<"data-value=\"docs\" aria-expanded=\"true\" tabindex=\"-1\"">>, H)),
    ?assert(has(<<"data-value=\"work\" aria-expanded=\"true\"">>, H)),
    ?assertNot(has_quiet(<<"id=\"t-0-g\" style">>, H)),
    ?assert(has(<<"data-value=\"q2\" aria-selected=\"true\" tabindex=\"0\"">>, H)),
    ?assert(has(<<"<div class=\"ah-tree-row ah-tree-row-selected\">">>, H)),
    ?assertEqual(1, count(<<"tabindex=\"0\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"doc\" value=\"q2\" data-ah-input>">>, H)),
    %% an unknown value selects nothing
    H2 = r(?M:ah_tree(files(), nope, [], [{id, t}])),
    ?assert(has(<<"data-ah-value=\"\"">>, H2)),
    ?assertNot(has_quiet(<<"aria-selected">>, H2)).

tree_options_test() ->
    H = r(?M:ah_tree([a, 1, "s"], undefined, [disabled],
                     [{toggle_mode, dblclick}, {animation, none}, {load, {?MODULE, children, #{}}}])),
    ?assert(has(<<"class=\"ah-tree ah-tree-disabled\"">>, H)),
    ?assert(has(<<"aria-disabled=\"true\" data-ah=\"tree\"">>, H)),
    ?assert(has(<<"data-toggle-mode=\"dblclick\" data-animation=\"none\" data-load=\"">>, H)),
    {match, [Token]} = re:run(H, <<"data-load=\"([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    ?assertEqual({ok, {?MODULE, children, #{}}}, aihtml_action:verify(Token)),
    ?assert(has(<<"data-value=\"1\"">>, H)),
    ?assert(has(<<"data-value=\"s\"">>, H)),
    %% expanded without children is a leaf; expanded with children is open
    H2 = r(?M:ah_tree([#{label => <<"x">>, expanded => true},
                       #{label => <<"y">>, expanded => true, items => [z]}], undefined, [], [{id, e}])),
    ?assert(has(<<"data-value=\"x\" tabindex=\"0\"">>, H2)),
    ?assert(has(<<"data-value=\"y\" aria-expanded=\"true\"">>, H2)),
    ?assert(has(<<"<span class=\"ah-tree-toggle ah-tree-toggle-open\"">>, H2)).

tree_escaping_test() ->
    H = r(?M:ah_tree([{<<"a\"b">>, <<"<i>x</i>">>}], <<"a\"b">>, [], [{id, t}])),
    ?assert(has(<<"data-value=\"a&quot;b\"">>, H)),
    ?assert(has(<<"&lt;i&gt;x&lt;/i&gt;">>, H)).

generated_id_test() ->
    H = r(#ah_tree{items = [a]}),
    {match, [Id]} = re:run(H, <<"^<div class=\"ah-tree\" id=\"(ah-t[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(<<"id=\"", Id/binary, "-0\"">>, H)),
    ?assertNotEqual(r(#ah_tree{}), r(#ah_tree{})).

%%%===================================================================
%%% Lazy nodes: render, fire, answer with set_children
%%%===================================================================

action(children, #{source := Source}, #{data := #{<<"value">> := V}} = Ev, Ctx) ->
    ?M:set_children(Ctx, Ev, maps:get(V, Source, [])).

lazy_round_trip_test() ->
    Ref = {?MODULE, children, #{source => #{<<"remote">> => [<<"r1">>, #{label => <<"R2">>,
                                                                        lazy => true}]}}},
    H = r(?M:ah_tree(files(), undefined, [], [{id, <<"tr">>}, {load, Ref}])),
    {match, [Token]} = re:run(H, <<"data-load=\"([^\"]+)\"">>, [{capture, all_but_first, binary}]),
    {ok, Ref} = aihtml_action:verify(Token),
    %% the browser sends the node's id and data-* attributes
    Event = #{<<"type">> => <<"ah:load">>, <<"id">> => <<"tr-2">>, <<"value">> => null,
              <<"data">> => #{<<"value">> => <<"remote">>, <<"tree">> => <<"tr">>,
                              <<"treeId">> => <<"2">>, <<"level">> => <<"1">>,
                              <<"lazy">> => <<"true">>}},
    {ok, [Html, Call]} = aihtml_action:execute(Ref, Event, #{send => fun(_) -> error(unexpected_flush) end}),
    #{op := html, swap := morph_inner, id := <<"tr-2-g">>, html := Kids} = Html,
    ?assert(has(<<"<li class=\"ah-tree-item ah-tree-item-leaf\" id=\"tr-2-0\" role=\"treeitem\" "
                  "aria-level=\"2\" data-tree-id=\"2-0\" data-value=\"r1\" tabindex=\"-1\">">>, Kids)),
    %% a nested lazy node names the tree and its own level
    ?assert(has(<<"id=\"tr-2-1\" role=\"treeitem\" aria-level=\"2\" data-tree-id=\"2-1\" "
                  "data-value=\"R2\" aria-expanded=\"false\" tabindex=\"-1\" data-lazy=\"true\" "
                  "data-tree=\"tr\" data-level=\"2\"">>, Kids)),
    ?assertEqual(#{op => call, id => <<"tr">>, method => <<"childrenLoaded">>,
                   args => [<<"tr-2">>]}, Call),
    %% the same markup as a first render of those children would have
    Full = r(?M:ah_tree([#{label => <<"Remote">>, value => remote,
                           items => [<<"r1">>, #{label => <<"R2">>, lazy => true}]}],
                        undefined, [], [{id, <<"tr">>}])),
    ?assert(has(binary:replace(binary:replace(Kids, <<"tr-2">>, <<"tr-0">>, [global]),
                               <<"data-tree-id=\"2-">>, <<"data-tree-id=\"0-">>, [global]),
                Full)),
    _ = iolist_to_binary(json:encode([Html, Call])).

set_children_empty_test() ->
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:set_children(Ctx, #{id => <<"t-0-3">>,
                                           data => #{<<"tree">> => <<"t">>, <<"level">> => <<"2">>,
                                                     <<"treeId">> => <<"0-3">>}}, [])
            end),
    ?assertMatch([#{op := html, id := <<"t-0-3-g">>, html := <<>>},
                  #{op := call, method := <<"childrenLoaded">>, args := [<<"t-0-3">>]}], Ops).


has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := tree}] = ?M:catalog(),
    ?assertEqual([{set_children, 3}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         [_ | _] = aihtml_catalog:classes(E, Fl),
         [?assert(is_binary(D)) || #{doc := D} <- Ms]
     end || #{name := N} <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         ?assert(byte_size(maps:get(doc, E)) > 0),
         [?assert(byte_size(maps:get(K, maps:get(option_docs, E))) > 0)
          || K <- maps:get(options, E, []) ++ maps:get(flags, E, [])]
     end || E <- ?M:catalog()].

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    Load = {?MODULE, children, #{}},
    ?assertEqual(r(?M:ah_tree(files(), q1, [disabled, <<"w-64">>],
                              [{id, t}, {name, n}, {toggle_mode, dblclick}, {animation, none},
                               {load, Load}, {title, <<"t">>}])),
                 r(#ah_tree{items = files(), value = q1, disabled = true, css = [<<"w-64">>],
                            id = t, name = n, toggle_mode = dblclick, animation = none,
                            load = Load, attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    T = ?M:ah_tree([a], a, [disabled, <<"x">>], [{name, n}, {toggle_mode, dblclick}, {role, x}]),
    ?assertMatch(#ah_tree{items = [a], value = a, disabled = true, name = n,
                          toggle_mode = dblclick, animation = slide, css = [<<"x">>],
                          attrs = [{role, x}]}, T),
    ?assertError({aihtml, {record_only_field, ah_tree, postback}},
                 ?M:ah_tree([], undefined, [], [{postback, pick}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:]+):([^\"]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, picked, #{id => 7}}},
                 Token(#ah_tree{items = [a], postback = {picked, #{id => 7}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, toggle_mode, triple}}, r(#ah_tree{toggle_mode = triple})),
    ?assertError({aihtml, {bad_option, animation, fade}}, r(#ah_tree{animation = fade})),
    ?assertError({aihtml, {bad_option, load, nope}}, r(#ah_tree{load = nope})),
    ?assertError({aihtml, {bad_option, lazy, yes}}, r(#ah_tree{items = [#{label => a, lazy => yes}]})),
    ?assertError({aihtml, {bad_tree_item, {1, 2, 3}}}, r(#ah_tree{items = [{1, 2, 3}]})),
    ?assertError({aihtml, {tree_item_needs_value, _}},
                 r(#ah_tree{items = [#{label => {safe, <<"<b>x</b>">>}}]})),
    ?assertError({aihtml, {bad_flag, tree, disabled, yes}}, r(#ah_tree{disabled = yes})),
    ?assertError({aihtml, {unknown_modifier, tree, big, _}}, ?M:ah_tree([], undefined, [big], [])).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(default(Tag))))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].

default(ah_tree) -> #ah_tree{}.
