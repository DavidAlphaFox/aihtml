%% Tests for aihtml_ribbon.
-module(aihtml_ribbon_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_ribbon.hrl").

-define(M, aihtml_ribbon).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

%%%===================================================================
%%% ribbon
%%%===================================================================

tabs() ->
    [#{key => home, label => <<"Home">>,
       groups => [{<<"Clipboard">>,
                   [#{key => paste, label => <<"Paste">>, icon => <<"P">>, size => large},
                    {stack, [{cut, <<"X">>, <<"Cut">>}]},
                    separator,
                    #{key => bold, label => <<"Bold">>, toggle => true, pressed => true},
                    #{key => more, label => <<"More">>, title => <<"More">>,
                      items => [{a, <<"A">>}, divider, #{key => b, label => <<"B">>, disabled => true}]},
                    {html, {safe, <<"<i>raw</i>">>}}]}]},
     {edit, <<"Edit">>, <<"edit panel">>, [{disabled, true}]},
     {view, <<"View">>, <<"view panel">>, [{icon, <<"V">>}]}].

ribbon_test() ->
    H = r(?M:ah_ribbon(tabs(), undefined, [<<"mb-2">>], [{id, rb}, {name, tab}])),
    ?assert(has(<<"<div class=\"ah-ribbon ah-ribbon-mode-default ah-ribbon-position-top mb-2\" "
                  "id=\"rb\" data-ah=\"ribbon\" data-ah-value=\"home\" "
                  "data-selection-mode=\"click\"><input type=\"hidden\" name=\"tab\" value=\"home\">">>, H)),
    ?assert(has(<<"<div class=\"ah-ribbon-tabs-inner\" role=\"tablist\" aria-label=\"Ribbon tabs\" "
                  "aria-orientation=\"horizontal\">">>, H)),
    ?assert(has(<<"<button class=\"ah-ribbon-tab ah-ribbon-tab-selected\" type=\"button\" role=\"tab\" "
                  "id=\"rb-tab-0\" data-index=\"0\" data-key=\"home\" aria-selected=\"true\" "
                  "aria-controls=\"rb-panel-0\" tabindex=\"0\"><span class=\"ah-ribbon-tab-text\">Home"
                  "</span></button>">>, H)),
    ?assert(has(<<"<button class=\"ah-ribbon-tab ah-ribbon-tab-disabled\" type=\"button\" role=\"tab\" "
                  "id=\"rb-tab-1\" data-index=\"1\" data-key=\"edit\" aria-selected=\"false\" "
                  "aria-controls=\"rb-panel-1\" aria-disabled=\"true\" tabindex=\"-1\" disabled>">>, H)),
    ?assert(has(<<"<span class=\"ah-ribbon-tab-icon\" aria-hidden=\"true\">V</span>">>, H)),
    ?assert(has(<<"<div class=\"ah-ribbon-selection-token\" aria-hidden=\"true\"></div>">>, H)),
    ?assert(has(<<"<button class=\"ah-ribbon-scroll-btn ah-ribbon-scroll-left\" type=\"button\" "
                  "data-scroll-direction=\"left\" aria-label=\"Scroll left\" tabindex=\"-1\">">>, H)),
    ?assertNot(has_quiet(<<"ah-ribbon-collapse-btn">>, H)),
    ?assert(has(<<"<div class=\"ah-ribbon-tab-content ah-ribbon-tab-content-active\" id=\"rb-panel-0\" "
                  "role=\"tabpanel\" aria-labelledby=\"rb-tab-0\" data-index=\"0\" data-key=\"home\">">>, H)),
    ?assert(has(<<"<div class=\"ah-ribbon-tab-content\" id=\"rb-panel-2\" role=\"tabpanel\" "
                  "aria-labelledby=\"rb-tab-2\" data-index=\"2\" data-key=\"view\">view panel</div>">>, H)),
    %% groups
    ?assert(has(<<"<div class=\"ah-ribbon-group\" role=\"group\" aria-labelledby=\"rb-panel-0-g0\">"
                  "<div class=\"ah-ribbon-group-content\">">>, H)),
    ?assert(has(<<"<div class=\"ah-ribbon-group-label\" id=\"rb-panel-0-g0\">Clipboard</div>">>, H)),
    ?assert(has(<<"<button class=\"ah-ribbon-button-large\" type=\"button\" data-command=\"paste\">"
                  "<span class=\"ah-ribbon-button-large-icon\" aria-hidden=\"true\">P</span>"
                  "<span class=\"ah-ribbon-button-large-text\">Paste</span></button>">>, H)),
    ?assert(has(<<"<div class=\"ah-ribbon-stack\"><button class=\"ah-ribbon-button\" type=\"button\" "
                  "data-command=\"cut\"><span class=\"ah-ribbon-button-icon\" aria-hidden=\"true\">X</span>"
                  "<span class=\"ah-ribbon-button-text\">Cut</span></button></div>">>, H)),
    ?assert(has(<<"<div class=\"ah-ribbon-separator\" role=\"separator\" "
                  "aria-orientation=\"vertical\"></div>">>, H)),
    ?assert(has(<<"<button class=\"ah-ribbon-button ah-ribbon-button-pressed\" type=\"button\" "
                  "data-command=\"bold\" data-toggle aria-pressed=\"true\">">>, H)),
    ?assert(has(<<"<div class=\"ah-ribbon-dropdown\"><button class=\"ah-ribbon-button "
                  "ah-ribbon-dropdown-toggle\" type=\"button\" data-menu=\"more\" title=\"More\" "
                  "aria-haspopup=\"menu\" aria-expanded=\"false\">">>, H)),
    ?assert(has(<<"<div class=\"ah-dropdown-btn-popup ah-ribbon-menu\" role=\"menu\" hidden>"
                  "<button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" "
                  "tabindex=\"-1\" data-command=\"a\"><span>A</span></button>"
                  "<div class=\"ah-dropdown-btn-divider\" role=\"separator\"></div>">>, H)),
    ?assert(has(<<"data-command=\"b\" disabled><span>B</span>">>, H)),
    ?assert(has(<<"<i>raw</i>">>, H)).

ribbon_options_test() ->
    H = r(?M:ah_ribbon(tabs(), view, [left, collapsed, danger, fade, collapsible],
                       [{selection_mode, hover}, {width, 300}, {height, <<"10rem">>},
                        {disabled, true}])),
    ?assert(has(<<"class=\"ah-ribbon ah-ribbon-animation-fade ah-ribbon-danger "
                  "ah-ribbon-mode-collapsed ah-ribbon-position-left ah-ribbon-collapsible "
                  "ah-ribbon-disabled\"">>, H)),
    ?assert(has(<<"style=\"width:300px;height:10rem;\"">>, H)),
    ?assert(has(<<"data-ah-value=\"view\" data-selection-mode=\"hover\" aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"aria-orientation=\"vertical\"">>, H)),
    ?assert(has(<<"ah-ribbon-scroll-up">>, H)),
    ?assert(has(<<"ah-ribbon-scroll-down">>, H)),
    ?assert(has(<<"<button class=\"ah-ribbon-collapse-btn\" type=\"button\" "
                  "aria-label=\"Collapse the ribbon\" aria-expanded=\"false\"">>, H)),
    %% the whole ribbon disabled: every tab is
    ?assertEqual(3, count(<<"aria-disabled=\"true\" tabindex=\"-1\" disabled>">>, H)),
    %% a disabled or unknown value falls back to the first enabled tab
    ?assertEqual(<<"home">>, attr_value(r(?M:ah_ribbon(tabs(), edit, [], [])))),
    ?assertEqual(<<"home">>, attr_value(r(?M:ah_ribbon(tabs(), nope, [], [])))),
    ?assertEqual(<<>>, attr_value(r(?M:ah_ribbon([], undefined, [], [])))).

attr_value(H) ->
    {match, [V]} = re:run(H, <<"data-ah-value=\"([^\"]*)\"">>, [{capture, all_but_first, binary}]),
    V.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := ribbon}] = ?M:catalog(),
    E = aihtml_catalog:entry(?M, ribbon),
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E,
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assertMatch([_ | _], Ms).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_ribbon(tabs(), view, [bottom, popup, primary, slide, collapsible, <<"x">>],
                                [{id, a}, {name, n}, {selection_mode, hover}, {title, <<"t">>}])),
                 r(#ah_ribbon{items = tabs(), value = view, position = bottom, mode = popup,
                              color = primary, animation = slide, collapsible = true,
                              css = [<<"x">>], id = a, name = n, selection_mode = hover,
                              attrs = [{title, <<"t">>}]})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z:]+:[^\":]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Tok | Rev] = lists:reverse(binary:split(T, <<":">>, [global])),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {iolist_to_binary(lists:join(<<":">>, lists:reverse(Rev))), Ref}
            end,
    ?assertEqual({<<"ah:command">>, {?MODULE, run, #{id => 1}}},
                 Token(#ah_ribbon{items = tabs(), postback = {run, #{id => 1}}})),
    ?assertError({aihtml, {record_only_field, ah_ribbon, postback}},
                 ?M:ah_ribbon([], undefined, [], [{postback, x}])).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, ribbon, position, middle, _}},
                 r(#ah_ribbon{position = middle})),
    ?assertError({aihtml, {bad_modifier, ribbon, mode, hidden, _}}, r(#ah_ribbon{mode = hidden})),
    ?assertError({aihtml, {bad_flag, ribbon, collapsible, yes}}, r(#ah_ribbon{collapsible = yes})),
    ?assertError({aihtml, {bad_option, selection_mode, dblclick}},
                 r(#ah_ribbon{selection_mode = dblclick})),
    ?assertError({aihtml, {bad_ribbon_tab, x}}, r(#ah_ribbon{items = [x]})),
    ?assertError({aihtml, {bad_ribbon_group, x}},
                 r(#ah_ribbon{items = [{k, <<"K">>, {groups, [x]}}]})),
    ?assertError({aihtml, {bad_ribbon_command, x}},
                 r(#ah_ribbon{items = [{k, <<"K">>, {groups, [{<<"G">>, [x]}]}}]})),
    ?assertError({aihtml, {bad_option, size, huge}},
                 r(#ah_ribbon{items = [{k, <<"K">>, {groups, [{<<"G">>, [#{key => c, label => <<"C">>,
                                                                           size => huge}]}]}}]})),
    ?assertError({aihtml, {bad_ribbon_menu_item, x}},
                 r(#ah_ribbon{items = [{k, <<"K">>, {groups, [{<<"G">>, [#{key => c, label => <<"C">>,
                                                                           items => [x]}]}]}}]})).

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

default(ah_ribbon) -> #ah_ribbon{}.
