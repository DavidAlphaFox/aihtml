%% Tests for aihtml_tile_layout.
-module(aihtml_tile_layout_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_tile_layout.hrl").

-define(M, aihtml_tile_layout).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%% data-ah-value decoded (the attribute is HTML-escaped).
value_of(H) ->
    {match, [V]} = re:run(H, <<"data-ah-value=\"([^\"]*)\"">>, [{capture, all_but_first, binary}]),
    json:decode(unescape(V)).

unescape(B) ->
    lists:foldl(fun({F, T}, Acc) -> binary:replace(Acc, F, T, [global]) end, B,
                [{<<"&quot;">>, <<"\"">>}, {<<"&lt;">>, <<"<">>}, {<<"&gt;">>, <<">">>},
                 {<<"&#39;">>, <<"'">>}, {<<"&amp;">>, <<"&">>}]).

%%%===================================================================
%%% tile_layout
%%%===================================================================

layout() ->
    {columns, [#{id => left, size => <<"25%">>, min => 100,
                 tabs => [{a, <<"A">>, <<"aa">>}, {b, <<"B">>, <<"bb">>}]},
               {rows, [#{id => ed, content => <<"editor">>, label => <<"Editor">>},
                       #{tabs => [#{id => t, label => <<"Term">>, content => <<"$">>, close => false}],
                         position => bottom, active => t, resize => false}]}]}.

tile_layout_test() ->
    H = r(?M:tile_layout(layout(), undefined, [<<"h-96">>], [{id, tl}, {name, arr}])),
    ?assert(has(<<"<div class=\"ah-tl h-96\" id=\"tl\" data-ah=\"tile-layout\" data-ah-value=\"">>, H)),
    ?assert(has(<<"data-splitbar-size=\"4\"><input type=\"hidden\" name=\"arr\" value=\"">>, H)),
    ?assert(has(<<"<div class=\"ah-tl-group ah-tl-vertical\" data-id=\"n\" data-type=\"layout-group\" "
                  "data-orientation=\"vertical\" style=\"grid-template-columns:25% 4px 1fr\">">>, H)),
    ?assert(has(<<"<div class=\"ah-tl-tab-group\" data-id=\"left\" data-type=\"tab-group\" "
                  "data-size=\"25%\" data-min=\"100\"><div class=\"ah-tl-tab-strip\" role=\"tablist\" "
                  "aria-orientation=\"horizontal\">">>, H)),
    ?assert(has(<<"<div class=\"ah-tl-tab ah-tl-tab-selected\" role=\"tab\" id=\"tl-tab-a\" "
                  "data-tab-id=\"a\" data-modifiers=\"drag,close\" aria-controls=\"tl-panel-a\" "
                  "aria-selected=\"true\" tabindex=\"0\"><span class=\"ah-tl-tab-label\">A</span>"
                  "<span class=\"ah-tl-tab-close\" aria-hidden=\"true\">&times;</span></div>">>, H)),
    ?assert(has(<<"<div class=\"ah-tl-tab-content ah-tl-tab-content-active\" id=\"tl-panel-a\" "
                  "data-id=\"a\" role=\"tabpanel\" aria-labelledby=\"tl-tab-a\">aa</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-tl-tab-content\" id=\"tl-panel-b\" data-id=\"b\" role=\"tabpanel\" "
                  "aria-labelledby=\"tl-tab-b\">bb</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-tl-splitbar ah-tl-splitbar-v\" role=\"separator\" "
                  "aria-orientation=\"vertical\" aria-label=\"Resize\" tabindex=\"0\"></div>">>, H)),
    ?assert(has(<<"<div class=\"ah-tl-group ah-tl-horizontal\" data-id=\"n1\" data-type=\"layout-group\" "
                  "data-orientation=\"horizontal\" style=\"grid-template-rows:1fr 4px 1fr\">">>, H)),
    ?assert(has(<<"<div class=\"ah-tl-item\" data-id=\"ed\" data-type=\"layout-item\" "
                  "data-label=\"Editor\">editor</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-tl-tab-group ah-tl-tab-group-bottom\" data-id=\"n1.1\" "
                  "data-type=\"tab-group\" data-resize=\"false\">">>, H)),
    ?assert(has(<<"data-tab-id=\"t\" data-modifiers=\"drag\"">>, H)),
    ?assertEqual(#{<<"closed">> => [],
                   <<"root">> =>
                       #{<<"type">> => <<"columns">>, <<"id">> => <<"n">>,
                         <<"items">> =>
                             [#{<<"type">> => <<"tabs">>, <<"id">> => <<"left">>, <<"size">> => <<"25%">>,
                                <<"tabs">> => [<<"a">>, <<"b">>], <<"active">> => <<"a">>},
                              #{<<"type">> => <<"rows">>, <<"id">> => <<"n1">>,
                                <<"items">> =>
                                    [#{<<"type">> => <<"item">>, <<"id">> => <<"ed">>},
                                     #{<<"type">> => <<"tabs">>, <<"id">> => <<"n1.1">>,
                                       <<"tabs">> => [<<"t">>], <<"active">> => <<"t">>}]}]}},
                 value_of(H)).

tile_layout_options_test() ->
    H = r(?M:tile_layout(#{tabs => [{x, <<"X">>, <<>>}], position => left}, undefined, [],
                         [{splitbar_size, 6}, {height, 300}, {disabled, true}])),
    ?assert(has(<<"class=\"ah-tl ah-tl-disabled\"">>, H)),
    ?assert(has(<<"style=\"height:300px;\"">>, H)),
    ?assert(has(<<"data-splitbar-size=\"6\" aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"ah-tl-tab-group ah-tl-tab-group-left">>, H)),
    ?assert(has(<<"aria-orientation=\"vertical\"">>, H)),
    %% percentages over 100% all become 1fr, as in sigil
    G = r(?M:tile_layout({rows, [#{id => a, content => <<>>, size => <<"70%">>},
                                 #{id => b, content => <<>>, size => <<"60%">>},
                                 #{id => c, content => <<>>, size => 200}]}, undefined, [],
                         [{splitbar_size, 6}])),
    ?assert(has(<<"grid-template-rows:1fr 6px 1fr 6px 1fr">>, G)),
    P = r(?M:tile_layout({rows, [#{id => a, content => <<>>, size => 200},
                                 #{id => b, content => <<>>}]}, undefined, [], [])),
    ?assert(has(<<"grid-template-rows:200px 4px 1fr">>, P)),
    ?assertError({aihtml, {duplicate_tile_id, <<"a">>}},
                 r(?M:tile_layout({columns, [{tabs, [{a, <<"A">>, <<>>}]},
                                             #{id => a, content => <<>>}]}, undefined, [], []))),
    ?assertError({aihtml, {bad_tile_layout_node, _}},
                 r(?M:tile_layout({columns, [#{content => <<"no id">>}]}, undefined, [], []))),
    ?assertError({aihtml, {bad_tile_layout_tab, _}},
                 r(?M:tile_layout({tabs, [x]}, undefined, [], []))).

%% A stored arrangement is rendered with the contents of the layout.
tile_layout_value_test() ->
    Stored = <<"{\"closed\":[\"a\"],\"root\":{\"type\":\"rows\",\"id\":\"x\",\"items\":["
               "{\"type\":\"tabs\",\"id\":\"left\",\"tabs\":[\"b\",\"ed\",\"gone\"],\"active\":\"ed\","
               "\"size\":\"30.5fr\"},"
               "{\"type\":\"item\",\"id\":\"zz\"}]}}">>,
    H = r(?M:tile_layout(layout(), Stored, [], [{id, tl}])),
    %% the rows group lost its second child, so the tab group is the root;
    %% the tile "ed" became a tab; "t" (not stored, not closed) joined the
    %% first tab group; "a" stays closed
    ?assertEqual(#{<<"closed">> => [<<"a">>],
                   <<"root">> => #{<<"type">> => <<"tabs">>, <<"id">> => <<"left">>,
                                   <<"size">> => <<"30.5fr">>,
                                   <<"tabs">> => [<<"b">>, <<"ed">>, <<"t">>],
                                   <<"active">> => <<"ed">>}},
                 value_of(H)),
    ?assert(has(<<"data-id=\"left\" data-type=\"tab-group\" data-size=\"30.5fr\" data-min=\"100\"">>, H)),
    ?assert(has(<<"id=\"tl-panel-ed\" data-id=\"ed\" role=\"tabpanel\" aria-labelledby=\"tl-tab-ed\">"
                  "editor</div>">>, H)),
    ?assert(has(<<"<span class=\"ah-tl-tab-label\">Editor</span>">>, H)),
    ?assertNot(has_quiet(<<"tl-panel-a\"">>, H)),
    ?assert(has(<<"data-tab-id=\"t\" data-modifiers=\"drag\"">>, H)),
    %% a decoded map works as well; sizes that are not plain tracks are dropped
    M = #{<<"root">> => #{<<"type">> => <<"columns">>, <<"id">> => <<"n">>,
                          <<"items">> => [#{<<"type">> => <<"item">>, <<"id">> => <<"ed">>,
                                            <<"size">> => <<"1fr;color:red">>},
                                          #{<<"type">> => <<"tabs">>, <<"id">> => <<"left">>,
                                            <<"tabs">> => [<<"a">>, <<"b">>, <<"t">>],
                                            <<"size">> => <<"120px">>}]}},
    H2 = r(?M:tile_layout(layout(), M, [], [])),
    ?assert(has(<<"grid-template-columns:1fr 4px 120px">>, H2)),
    ?assertNot(has_quiet(<<"color:red">>, H2)),
    %% unparsable or empty values render the layout as written
    Plain = value_of(r(?M:tile_layout(layout(), undefined, [], []))),
    ?assertEqual(Plain, value_of(r(?M:tile_layout(layout(), <<"not json">>, [], [])))),
    ?assertEqual(Plain, value_of(r(?M:tile_layout(layout(), <<"{\"root\":{\"type\":\"item\",\"id\":\"q\"}}">>,
                                                  [], [])))).

%% A tile the stored value does not know is appended to the root.
tile_layout_new_tile_test() ->
    Stored = <<"{\"closed\":[],\"root\":{\"type\":\"columns\",\"id\":\"n\",\"items\":["
               "{\"type\":\"tabs\",\"id\":\"left\",\"tabs\":[\"a\",\"b\",\"t\"],\"active\":\"a\"},"
               "{\"type\":\"item\",\"id\":\"old\"}]}}">>,
    V = value_of(r(?M:tile_layout(layout(), Stored, [], []))),
    #{<<"root">> := #{<<"type">> := <<"columns">>, <<"items">> := Items}} = V,
    ?assertEqual([<<"left">>, <<"ed">>], [maps:get(<<"id">>, I) || I <- Items]).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := tile_layout}] = ?M:catalog(),
    E = aihtml_catalog:entry(?M, tile_layout),
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E,
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assertMatch([_ | _], Ms).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:tile_layout(layout(), undefined, [<<"h-64">>],
                                  [{id, b}, {splitbar_size, 8}, {height, 200}, {disabled, true}])),
                 r(#ah_tile_layout{layout = layout(), css = [<<"h-64">>], id = b, splitbar_size = 8,
                                   height = 200, disabled = true})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z:]+:[^\":]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Tok | Rev] = lists:reverse(binary:split(T, <<":">>, [global])),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {iolist_to_binary(lists:join(<<":">>, lists:reverse(Rev))), Ref}
            end,
    ?assertEqual({<<"change">>, {other, save, #{}}},
                 Token(#ah_tile_layout{layout = layout(), postback = save, delegate = other})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, splitbar_size, 0}}, r(#ah_tile_layout{splitbar_size = 0})),
    ?assertError({aihtml, {bad_option, position, middle}},
                 r(#ah_tile_layout{layout = #{tabs => [], position => middle}})),
    ?assertError({aihtml, {bad_option, min, -1}},
                 r(#ah_tile_layout{layout = #{id => a, content => <<>>, min => -1}})),
    ?assertError({aihtml, {bad_option, close, 1}},
                 r(#ah_tile_layout{layout = {tabs, [#{id => a, label => <<"A">>, close => 1}]}})),
    ?assertError({aihtml, {unknown_modifier, tile_layout, big, _}}, ?M:tile_layout(x, x, [big], [])).

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

default(ah_tile_layout) -> #ah_tile_layout{}.
