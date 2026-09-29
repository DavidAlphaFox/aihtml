-module(aihtml_layout_basic_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_layout_basic.hrl").

-export([action/4]).

-define(M, aihtml_layout_basic).

r(H) -> aihtml_html:render_binary(H).

has(Bin, Part) -> binary:match(Bin, Part) =/= nomatch.

count(Bin, Part) -> length(binary:matches(Bin, Part)).

%%% catalog / examples

catalog_matches_exports_test() ->
    Exports = ?M:module_info(exports),
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([card, panel, expander, tabs, tab_bar, breadcrumbs, pagination, steps,
                  skeleton, loader, empty], Names),
    [?assert(lists:keymember(N, 1, Exports)) || N <- Names],
    [?assertMatch(#{category := layout, root := <<"ah-", _/binary>>, signature := _}, E)
     || E <- ?M:catalog()].

catalog_documents_every_option_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         ?assertEqual({N, []}, {N, [K || K <- maps:get(options, E, []) ++ maps:get(flags, E, []),
                                         not maps:is_key(K, Docs)]}),
         ?assert(is_list(maps:get(methods, E)))
     end || #{name := N} = E <- ?M:catalog()].

unknown_modifier_test() ->
    ?assertError({aihtml, {unknown_modifier, card, bogus, _}}, ?M:card(<<"x">>, [bogus], [])),
    ?assertError({aihtml, {conflicting_modifiers, tabs, position, _}},
                 ?M:tabs([], undefined, [left, right], [])).

%%% card

card_test() ->
    H = r(?M:card(<<"Body & more">>, [hover, <<"w-64">>],
                  [{title, <<"T">>}, {footer, <<"F">>}, {id, c1}])),
    ?assertEqual(<<"<div class=\"ah-card ah-card-hover w-64\" id=\"c1\">"
                   "<div class=\"ah-card-header\"><h3 class=\"ah-card-title\">T</h3></div>"
                   "<div class=\"ah-card-body\">Body &amp; more</div>"
                   "<div class=\"ah-card-footer\">F</div></div>">>, H).

card_minimal_test() ->
    ?assertEqual(<<"<div class=\"ah-card\"><div class=\"ah-card-body\">x</div></div>">>,
                 r(?M:card(<<"x">>, [], []))).

%%% panel

panel_test() ->
    H = r(?M:panel(<<"c">>, [bordered], [{id, p}, {title, <<"T">>}, {collapsible, true},
                                          {collapsed, true}, {height, 100}])),
    ?assert(has(H, <<"class=\"ah-panel ah-panel-bordered ah-panel-has-header ah-panel-collapsed\"">>)),
    ?assert(has(H, <<"data-ah=\"panel\"">>)),
    ?assert(has(H, <<"aria-expanded=\"false\" aria-controls=\"p-body\"">>)),
    ?assert(has(H, <<"id=\"p-body\" style=\"height:100px;display:none;\"">>)),
    ?assert(has(H, <<"<div class=\"ah-panel-content\">c</div>">>)).

panel_plain_test() ->
    H = r(?M:panel(<<"c">>, [], [{id, p}])),
    ?assertNot(has(H, <<"ah-panel-header">>)),
    ?assertNot(has(H, <<"style=">>)).

%%% expander

expander_test() ->
    H = r(?M:expander(<<"body">>, [bottom, no_gutters], [{id, e}, {header, <<"Head">>},
                                                         {expanded, false}, {name, open}])),
    ?assert(has(H, <<"class=\"ah-expander ah-expander-bottom ah-expander-no-gutters\"">>)),
    ?assert(has(H, <<"data-ah=\"expander\" data-ah-value=\"false\"">>)),
    ?assert(has(H, <<"role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"e-content\"">>)),
    ?assert(has(H, <<"id=\"e-content\" role=\"region\" aria-labelledby=\"e-header\" style=\"display:none\"">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"open\" value=\"false\">">>)).

expander_disabled_dual_icon_test() ->
    H = r(?M:expander(<<"b">>, [disabled], [{id, e}, {expand_icon, <<"+">>},
                                            {collapse_icon, <<"-">>}])),
    ?assert(has(H, <<"tabindex=\"-1\"">>)),
    ?assert(has(H, <<"aria-disabled=\"true\"">>)),
    ?assert(has(H, <<"ah-expander-arrow ah-expander-arrow-expanded ah-expander-arrow-dual">>)),
    ?assert(has(H, <<"ah-expander-header ah-expander-header-expanded">>)).

expander_structured_header_test() ->
    H = r(?M:expander(<<"b">>, [], [{header, #{title => <<"A">>, extra => <<"3">>}},
                                    {toggle_mode, none}, {accordion, g}])),
    ?assert(has(H, <<"ah-expander-header-text-structured">>)),
    ?assert(has(H, <<"<span class=\"ah-expander-header-extra\">3</span>">>)),
    ?assert(has(H, <<"ah-expander-header-no-toggle">>)),
    ?assert(has(H, <<"data-toggle-mode=\"none\"">>)),
    ?assert(has(H, <<"data-accordion=\"g\"">>)).

%%% tabs

tabs_test() ->
    H = r(?M:tabs([{a, <<"A">>, <<"pa">>}, {<<"b">>, <<"B">>, <<"pb">>},
                   {c, <<"C">>, <<"pc">>, #{disabled => true}}],
                  b, [left], [{id, t}, {name, tab}])),
    ?assert(has(H, <<"class=\"ah-tabs ah-tabs-left\" id=\"t\" data-ah=\"tabs\" data-ah-value=\"b\"">>)),
    ?assert(has(H, <<"role=\"tablist\" aria-orientation=\"vertical\"">>)),
    ?assert(has(H, <<"class=\"ah-tabs-item ah-tabs-item-selected\" id=\"t-tab-1\" role=\"tab\" "
                     "data-key=\"b\" tabindex=\"0\" aria-selected=\"true\" aria-controls=\"t-panel-1\"">>)),
    ?assert(has(H, <<"ah-tabs-item ah-tabs-item-disabled">>)),
    ?assertEqual(2, count(H, <<"style=\"display:none\"">>)),
    ?assert(has(H, <<"class=\"ah-tabs-panel ah-tabs-panel-active\" id=\"t-panel-1\"">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"tab\" value=\"b\">">>)),
    ?assertNot(has(H, <<" name=\"tab\" data">>)).

tabs_default_active_skips_disabled_test() ->
    H = r(?M:tabs([{a, <<"A">>, <<>>, #{disabled => true}}, {b, <<"B">>, <<>>}], undefined, [],
                  [{scrollable, true}])),
    ?assert(has(H, <<"data-ah-value=\"b\"">>)),
    ?assert(has(H, <<"ah-tabs ah-tabs-top ah-tabs-scrollable">>)),
    ?assertEqual(2, count(H, <<"ah-tabs-scroll-btn">>)).

%%% tab_bar

tab_bar_test() ->
    H = r(?M:tab_bar([{a, <<"a.erl">>}, {b, <<"b.erl">>, #{dirty => true}}], b, [], [])),
    ?assert(has(H, <<"class=\"ah-tab-bar\" role=\"tablist\" data-ah=\"tab-bar\" data-ah-value=\"b\"">>)),
    ?assert(has(H, <<"data-id=\"b\" data-active=\"true\" data-dirty=\"true\"">>)),
    ?assertEqual(1, count(H, <<"ah-tab-bar__dot">>)),
    ?assertEqual(2, count(H, <<"aria-label=\"close ">>)),
    ?assertNot(has(r(?M:tab_bar([{a, <<"A">>}], a, [], [{closable, false}])), <<"__close">>)).

%%% breadcrumbs

breadcrumbs_test() ->
    H = r(?M:breadcrumbs([{<<"Home">>, <<"/">>}, <<"Here">>], [], [])),
    ?assertEqual(<<"<nav class=\"ah-breadcrumbs\" aria-label=\"breadcrumb\" data-has-separator=\"true\">"
                   "<ol class=\"ah-breadcrumbs__list\">"
                   "<li class=\"ah-breadcrumbs__item\" data-index=\"0\">"
                   "<a class=\"ah-breadcrumbs__link\" href=\"/\">Home</a></li>"
                   "<li class=\"ah-breadcrumbs__separator\" role=\"presentation\" aria-hidden=\"true\">/</li>"
                   "<li class=\"ah-breadcrumbs__item\" data-index=\"1\" aria-current=\"page\">"
                   "<span class=\"ah-breadcrumbs__text\">Here</span></li></ol></nav>">>, H).

breadcrumbs_collapse_dots_test() ->
    H = r(?M:breadcrumbs([{<<"1">>, <<"#">>}, {<<"2">>, <<"#">>}, {<<"3">>, <<"#">>},
                          {<<"4">>, <<"#">>}, {<<"5">>, <<"#">>}], [],
                         [{max_items, 3}, {separator, none}, {active_last, true}])),
    ?assert(has(H, <<"data-has-separator=\"false\"">>)),
    ?assert(has(H, <<"ah-breadcrumbs__ellipsis">>)),
    ?assertEqual(3, count(H, <<"ah-breadcrumbs__link">>)),
    ?assertNot(has(H, <<"aria-current">>)).

%%% pagination

visible_pages_test() ->
    ?assertEqual([1, 2, 3, 4, 5], ?M:visible_pages(3, 5, 7)),
    ?assertEqual([1, 2, 3, 4, 5, gap, 20], ?M:visible_pages(4, 20, 7)),
    ?assertEqual([1, gap, 4, 5, 6, gap, 20], ?M:visible_pages(5, 20, 7)),
    ?assertEqual([1, gap, 16, 17, 18, 19, 20], ?M:visible_pages(17, 20, 7)),
    ?assertEqual([1, gap, 8, 9, 10, 11, 12, gap, 30], ?M:visible_pages(10, 30, 9)),
    [?assert(length(?M:visible_pages(C, 50, 7)) =< 7) || C <- lists:seq(1, 50)],
    [?assert(lists:member(C, ?M:visible_pages(C, 50, 7))) || C <- lists:seq(1, 50)].

pagination_test() ->
    H = r(?M:pagination(95, 3, [], [{name, page}])),
    ?assert(has(H, <<"data-ah=\"pagination\" data-ah-value=\"3\" data-total=\"95\" "
                     "data-page-size=\"10\" data-max-visible=\"7\"">>)),
    ?assert(has(H, <<"aria-current=\"page\">3</li>">>)),
    ?assert(has(H, <<"<option value=\"10\" selected>10 / page</option>">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"page\" value=\"3\">">>)),
    ?assertEqual(0, count(H, <<"ah-pagination-nav-disabled">>)).

pagination_clamps_and_options_test() ->
    H = r(?M:pagination(40, 99, [], [{page_size, 20}, {show_first_last, true}, {show_total, true},
                                     {show_jumper, true}, {show_size_selector, false},
                                     {labels, #{total => <<"{0} rows">>}}])),
    ?assert(has(H, <<"data-ah-value=\"2\"">>)),
    ?assert(has(H, <<"40 rows">>)),
    ?assert(has(H, <<"data-type=\"last\"">>)),
    ?assertEqual(2, count(H, <<"ah-pagination-nav-disabled">>)),
    ?assert(has(H, <<"ah-pagination-jumper-input">>)),
    ?assertNot(has(H, <<"<select">>)).

pagination_simple_and_links_test() ->
    S = r(?M:pagination(30, 2, [simple], [])),
    ?assert(has(S, <<"Page 2 / 3">>)),
    ?assertNot(has(S, <<"ah-pagination-item">>)),
    L = r(?M:pagination(30, 1, [], [{href, <<"?p={page}&s={size}">>}, {show_size_selector, false}])),
    ?assert(has(L, <<"<a class=\"ah-pagination-item\" href=\"?p=2&amp;s=10\"">>)),
    ?assert(has(L, <<"<a class=\"ah-pagination-nav\" href=\"?p=2&amp;s=10\" data-type=\"next\"">>)),
    ?assert(has(L, <<"ah-pagination-nav ah-pagination-nav-disabled\" data-type=\"prev\"">>)).

%%% steps

steps_test() ->
    H = r(?M:steps([<<"A">>, {<<"B">>, <<"desc">>}, #{title => <<"C">>, status => error}], 1,
                   [vertical], [])),
    ?assert(has(H, <<"class=\"ah-steps ah-steps-vertical\" data-ah=\"steps\" data-ah-value=\"1\"">>)),
    ?assert(has(H, <<"ah-steps-item ah-steps-item-completed ah-steps-item-clickable">>)),
    ?assert(has(H, <<"ah-steps-item ah-steps-item-active ah-steps-item-clickable ah-steps-item-selected">>)),
    ?assert(has(H, <<"ah-steps-item ah-steps-item-error">>)),
    ?assert(has(H, <<"ah-steps-connector ah-steps-connector-done">>)),
    ?assert(has(H, <<"aria-current=\"step\"">>)),
    ?assertNot(has(H, <<"ah-steps-panels">>)),
    ?assertNot(has(H, <<"ah-steps-nav">>)).

steps_panels_nav_test() ->
    H = r(?M:steps([#{title => <<"A">>, content => <<"a">>}, #{title => <<"B">>, content => <<"b">>}],
                   0, [], [{clickable, false}])),
    ?assert(has(H, <<"<div class=\"ah-steps-panel ah-steps-panel-active\" data-index=\"0\">a</div>">>)),
    ?assertEqual(2, count(H, <<" disabled>">>)),
    ?assert(has(H, <<"data-clickable=\"false\"">>)),
    ?assertNot(has(H, <<"role=\"button\"">>)).

%%% skeleton, loader, empty

skeleton_test() ->
    T = r(?M:skeleton([], [{lines, 2}])),
    ?assertEqual(<<"<div class=\"ah-skeleton\" data-variant=\"text\" data-animated=\"true\" "
                   "role=\"status\" aria-busy=\"true\" aria-live=\"polite\" aria-label=\"Loading\">"
                   "<span class=\"ah-skeleton__line\" style=\"width:100%;\"></span>"
                   "<span class=\"ah-skeleton__line\" style=\"width:62%;\"></span></div>">>, T),
    C = r(?M:skeleton([circle, static], [{width, 32}])),
    ?assert(has(C, <<"data-variant=\"circle\" data-animated=\"false\"">>)),
    ?assert(has(C, <<"style=\"width:32px;height:32px;\"">>)),
    R = r(?M:skeleton([rect, done], [{radius, 4}])),
    ?assert(has(R, <<"class=\"ah-skeleton ah-skeleton--done\"">>)),
    ?assert(has(R, <<"border-radius:4px;">>)).

loader_test() ->
    H = r(?M:loader([hidden, top], [{text, <<"Wait">>}, {modal, true}])),
    ?assertEqual(<<"<div class=\"ah-loader ah-loader-text-top ah-loader-hidden\" role=\"status\" "
                   "aria-live=\"polite\" aria-busy=\"false\" aria-label=\"Wait\" data-ah=\"loader\" "
                   "data-modal=\"true\"><div class=\"ah-loader-icon\" aria-hidden=\"true\"></div>"
                   "<div class=\"ah-loader-text\">Wait</div></div>">>, H),
    ?assert(has(r(?M:loader([], [])), <<"ah-loader ah-loader-text-bottom\"">>)).

empty_test() ->
    H = r(?M:empty([], [compact], [{title, <<"None">>}, {description, <<"D">>}])),
    ?assertEqual(<<"<div class=\"ah-empty ah-empty-compact\"><div class=\"ah-empty__title\">None</div>"
                   "<div class=\"ah-empty__description\">D</div></div>">>, H),
    ?assert(has(r(?M:empty(<<"act">>, [], [])), <<"<div class=\"ah-empty__content\">act</div>">>)).

%%% CSS: every sigil class the module writes exists in the stylesheets

classes_are_styled_test() ->
    %% the sources when run from the project root, else the installed copy
    Root = "apps/aihtml/priv/css",
    Dir = case filelib:is_dir(Root) of
              true -> Root;
              false -> filename:join(code:priv_dir(aihtml), "css")
          end,
    Css = iolist_to_binary([element(2, file:read_file(F))
                            || F <- filelib:wildcard(filename:join([Dir, "**", "*.css"]))]),
    Html = iolist_to_binary([r(H) || H <- samples()]),
    {ok, Re} = re:compile(<<"class=\"([^\"]*)\"">>),
    {match, Ms} = re:run(Html, Re, [global, {capture, [1], binary}]),
    Classes = lists:usort([C || [Cs] <- Ms, C <- binary:split(Cs, <<" ">>, [global, trim_all]),
                                binary:match(C, <<"ah-">>) =:= {0, 3}]),
    %% state markers with no rules of their own (sigil writes -top too)
    Markers = [<<"ah-expander-top">>, <<"ah-pagination-links">>],
    Missing = [C || C <- Classes -- Markers, not has(Css, <<".", C/binary>>)],
    ?assertEqual([], Missing).

%% One render of each component in its main variants and states.
samples() ->
    Tabs = [{a, <<"A">>, <<"a">>}, {b, <<"B">>, <<"b">>, #{disabled => true}}],
    [?M:card(<<"b">>, [hover, flush], [{title, <<"t">>}, {subtitle, <<"s">>}, {extra, <<"x">>},
                                       {media, <<"m">>}, {footer, <<"f">>}]),
     ?M:panel(<<"c">>, [bordered], [{title, <<"t">>}, {actions, <<"a">>}, {collapsible, true},
                                    {collapsed, true}]),
     ?M:expander(<<"c">>, [bottom, square, no_gutters, disabled],
                 [{header, #{title => <<"t">>, subheader => <<"s">>, extra => <<"x">>}},
                  {actions, <<"a">>}, {arrow_position, left}, {toggle_mode, none},
                  {expand_icon, <<"+">>}, {collapse_icon, <<"-">>}]),
     ?M:expander(<<"c">>, [], []),
     [?M:tabs(Tabs, a, [P, disabled], [{scrollable, true}]) || P <- [top, bottom, left, right]],
     ?M:tab_bar([{a, <<"A">>, #{dirty => true, icon => <<"i">>}}, {b, <<"B">>}], a, [], []),
     ?M:breadcrumbs([#{label => <<"H">>, href => <<"/">>, icon => <<"i">>}, {<<"A">>, <<"#">>},
                     {<<"B">>, <<"#">>}, {<<"C">>, <<"#">>}, <<"D">>], [], [{max_items, 3}]),
     ?M:pagination(500, 12, [disabled], [{show_first_last, true}, {show_total, true},
                                         {show_jumper, true}]),
     ?M:pagination(95, 3, [simple], []),
     ?M:pagination(95, 3, [], [{href, <<"?p={page}">>}]),
     ?M:steps([<<"A">>, #{title => <<"B">>, status => error, description => <<"d">>,
                          content => <<"c">>}, #{title => <<"C">>, disabled => true}, <<"D">>],
              1, [vertical, disabled], []),
     [?M:skeleton([V, static], []) || V <- [text, circle, rect]],
     ?M:skeleton([done], []),
     [?M:loader([P, hidden, inline, center, disabled], []) || P <- [top, bottom, left, right]],
     ?M:empty(<<"a">>, [compact], [{icon, <<"i">>}, {title, <<"t">>}, {description, <<"d">>}])].

%%% element records (designs/05-records.md)

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

-define(TABS, [{a, <<"A">>, <<"pa">>}, {b, <<"B">>, <<"pb">>, #{disabled => true}}]).

record_equals_builder_test() ->
    ?assertEqual(r(?M:card(<<"b">>, [hover, <<"w-64">>],
                           [{title, <<"T">>}, {footer, <<"F">>}, {id, c1}, {data_x, 1}])),
                 r(#ah_card{body = <<"b">>, hover = true, css = [<<"w-64">>], title = <<"T">>,
                            footer = <<"F">>, id = c1, attrs = [{data_x, 1}]})),
    ?assertEqual(r(?M:expander(<<"b">>, [bottom, disabled],
                               [{id, e}, {header, <<"H">>}, {expanded, false},
                                {toggle_mode, dblclick}, {name, open}])),
                 r(#ah_expander{body = <<"b">>, position = bottom, disabled = true, id = e,
                                header = <<"H">>, expanded = false, toggle_mode = dblclick,
                                name = open})),
    ?assertEqual(r(?M:tabs(?TABS, b, [left], [{id, t}, {scrollable, true}])),
                 r(#ah_tabs{items = ?TABS, value = b, position = left, id = t,
                            scrollable = true})),
    ?assertEqual(r(?M:pagination(500, 12, [simple], [{page_size, 20}, {show_total, true},
                                                     {labels, #{prev => <<"<">>}}])),
                 r(#ah_pagination{total = 500, value = 12, simple = true, page_size = 20,
                                  show_total = true, labels = #{prev => <<"<">>}})),
    ?assertEqual(r(?M:steps([<<"A">>, <<"B">>], 1, [vertical], [{clickable, false}])),
                 r(#ah_steps{items = [<<"A">>, <<"B">>], value = 1, orientation = vertical,
                             clickable = false})),
    ?assertEqual(r(?M:skeleton([circle, static], [{width, 32}])),
                 r(#ah_skeleton{variant = circle, static = true, width = 32})),
    ?assertEqual(r(?M:loader([hidden, top], [{text, <<"Wait">>}, {modal, true}])),
                 r(#ah_loader{hidden = true, text_position = top, text = <<"Wait">>,
                              modal = true})).

builder_fills_fields_test() ->
    P = ?M:panel(<<"c">>, [bordered, <<"p-2">>],
                 [{id, p}, {title, <<"T">>}, {collapsible, true}, {height, 100},
                  {data_x, 1}]),
    ?assertMatch(#ah_panel{body = <<"c">>, bordered = true, css = [<<"p-2">>], id = p,
                           title = <<"T">>, collapsible = true, collapsed = false,
                           height = 100, attrs = [{data_x, 1}]}, P),
    ?assertMatch(#ah_breadcrumbs{items = [<<"A">>], separator = none, max_items = 3,
                                 label = <<"breadcrumb">>},
                 ?M:breadcrumbs([<<"A">>], [], [{separator, none}, {max_items, 3}])),
    ?assertMatch(#ah_tab_bar{items = [], value = x, closable = false},
                 ?M:tab_bar([], x, [], [{closable, false}])),
    ?assertError({aihtml, {record_only_field, ah_card, postback}},
                 ?M:card(<<"x">>, [], [{postback, save}])).

generated_ids_test() ->
    %% no id: one is generated and the inner ids derive from it
    E = r(#ah_expander{body = <<"b">>}),
    {match, [Id]} = re:run(E, <<"^<div class=\"ah-expander ah-expander-top\" id=\"(ah-expander-[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(E, <<"id=\"", Id/binary, "-header\"">>)),
    ?assert(has(E, <<"aria-controls=\"", Id/binary, "-content\"">>)),
    ?assertEqual(1, count(E, <<"id=\"", Id/binary, "\"">>)),
    ?assertMatch({match, _}, re:run(r(#ah_panel{}), <<"id=\"ah-panel-[0-9]+-body\"">>)),
    %% an id among the attrs is used too
    T = r(#ah_tabs{items = ?TABS, attrs = [{<<"id">>, <<"t">>}]}),
    ?assert(has(T, <<"class=\"ah-tabs ah-tabs-top\" id=\"t\" data-ah=\"tabs\"">>)),
    ?assert(has(T, <<"id=\"t-panel-0\"">>)).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, toggled, #{}}},
                 Token(#ah_expander{id = e, postback = toggled})),
    ?assertEqual({<<"change">>, {?MODULE, tab, #{id => 1}}},
                 Token(#ah_tabs{items = ?TABS, id = t, postback = {tab, #{id => 1}}})),
    ?assertEqual({<<"change">>, {?MODULE, focus, #{}}},
                 Token(#ah_tab_bar{items = [{a, <<"a">>}], postback = focus})),
    ?assertEqual({<<"change">>, {other_mod, page, 1}},
                 Token(#ah_pagination{total = 50, postback = {page, 1}, delegate = other_mod})),
    ?assertEqual({<<"change">>, {?MODULE, step, #{}}},
                 Token(#ah_steps{items = [<<"A">>], postback = step})),
    [?assertError({aihtml, {no_postback_event, Tag}}, r(setelement(6, R, x)))
     || R <- [#ah_card{}, #ah_panel{id = p}, #ah_breadcrumbs{}, #ah_skeleton{}, #ah_loader{},
              #ah_empty{}],
        Tag <- [element(1, R)]].

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, tabs, position, middle, _}},
                 r(#ah_tabs{id = t, position = middle})),
    ?assertError({aihtml, {bad_modifier, loader, text_position, center, _}},
                 r(#ah_loader{text_position = center})),
    ?assertError({aihtml, {bad_flag, card, hover, yes}}, r(#ah_card{hover = yes})),
    ?assertError({aihtml, {modifier_in_css, empty, compact}}, r(#ah_empty{css = [compact]})),
    ?assertError({aihtml, {bad_option, toggle_mode, hold}},
                 r(#ah_expander{id = e, toggle_mode = hold})),
    ?assertError({aihtml, {bad_option, expanded, "no"}},
                 r(#ah_expander{id = e, expanded = "no"})),
    ?assertError({aihtml, {bad_option, selection_mode, drag}},
                 r(#ah_tabs{id = t, selection_mode = drag})),
    ?assertError({aihtml, {bad_option, show_jumper, 1}},
                 r(#ah_pagination{show_jumper = 1})),
    %% values are checked when rendering, not when building
    Bad = ?M:expander(<<"b">>, [], [{id, e}, {arrow_position, up}]),
    ?assertError({aihtml, {bad_option, arrow_position, up}}, r(Bad)),
    %% a group without a default may stay undefined
    ?assert(has(r(#ah_skeleton{}), <<"class=\"ah-skeleton\" data-variant=\"text\"">>)).

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

default(ah_card) -> #ah_card{};
default(ah_panel) -> #ah_panel{};
default(ah_expander) -> #ah_expander{};
default(ah_tabs) -> #ah_tabs{};
default(ah_tab_bar) -> #ah_tab_bar{};
default(ah_breadcrumbs) -> #ah_breadcrumbs{};
default(ah_pagination) -> #ah_pagination{};
default(ah_steps) -> #ah_steps{};
default(ah_skeleton) -> #ah_skeleton{};
default(ah_loader) -> #ah_loader{};
default(ah_empty) -> #ah_empty{}.
