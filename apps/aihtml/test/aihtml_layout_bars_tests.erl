%% Tests for aihtml_layout_bars. The module is also the fake action module
%% of the command palette's server-side search.
-module(aihtml_layout_bars_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_layout_bars.hrl").

-define(M, aihtml_layout_bars).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

%%%===================================================================
%%% activity_bar
%%%===================================================================

activity_items() ->
    [{files, <<"F">>, <<"Explorer">>},
     {search, <<"S">>, <<"Search">>},
     divider,
     {git, <<"G">>, <<"Source control">>, [{disabled, true}]}].

activity_bar_test() ->
    H = r(?M:activity_bar(activity_items(), search, [<<"h-64">>], [{id, ab}, {name, view}])),
    ?assert(has(<<"<div class=\"ah-activity-bar h-64\" role=\"tablist\" "
                  "aria-orientation=\"vertical\" data-placement=\"left\" "
                  "data-ah=\"activity-bar\" data-ah-value=\"search\" id=\"ab\">">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"view\" value=\"search\">">>, H)),
    ?assert(has(<<"<button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" "
                  "data-id=\"search\" data-active=\"true\" data-disabled=\"false\" "
                  "aria-selected=\"true\" aria-label=\"Search\" title=\"Search\" "
                  "tabindex=\"0\"><span class=\"ah-activity-bar__icon\">S</span></button>">>, H)),
    ?assert(has(<<"data-id=\"files\" data-active=\"false\"">>, H)),
    ?assert(has(<<"<div class=\"ah-activity-bar__divider\" role=\"presentation\" "
                  "data-index=\"2\"></div>">>, H)),
    ?assert(has(<<"data-id=\"git\" data-active=\"false\" data-disabled=\"true\"">>, H)),
    ?assert(has(<<" disabled>">>, H)),
    ?assertEqual(1, count(<<"tabindex=\"0\"">>, H)).

activity_bar_right_and_no_value_test() ->
    H = r(?M:activity_bar(activity_items(), undefined, [right], [])),
    ?assert(has(<<"class=\"ah-activity-bar\"">>, H)),
    ?assert(has(<<"data-placement=\"right\"">>, H)),
    ?assert(has(<<"data-ah-value=\"\"">>, H)),
    %% nothing active: the first enabled item takes the tab stop
    ?assert(has(<<"data-id=\"files\" data-active=\"false\" data-disabled=\"false\" "
                  "aria-selected=\"false\" aria-label=\"Explorer\" title=\"Explorer\" "
                  "tabindex=\"0\"">>, H)),
    ?assertEqual(1, count(<<"tabindex=\"0\"">>, H)),
    ?assertError({aihtml, {bad_activity_bar_item, _}}, r(?M:activity_bar([x], x, [], []))).

%%%===================================================================
%%% navigationbar
%%%===================================================================

nav_items() ->
    [{<<"One">>, <<"First">>},
     {#{title => <<"Two">>, subheader => <<"sub">>, extra => <<"x">>}, <<"Second">>,
      [{actions, <<"Act">>}]},
     #{header => <<"Three">>, content => <<"Third">>, disabled => true}].

navigationbar_test() ->
    H = r(?M:navigationbar(nav_items(), 0, [square], [{id, nb}, {name, open}])),
    ?assert(has(<<"<div class=\"ah-navigationbar ah-navigationbar-square ah-navigationbar-vertical "
                  "ah-navigationbar-animate-slide\" id=\"nb\" data-ah=\"navigationbar\" "
                  "data-ah-value=\"0\" data-expand-mode=\"single_fit_height\" "
                  "data-animation=\"slide\" data-toggle-mode=\"click\" "
                  "data-expand-duration=\"250\" data-collapse-duration=\"250\">">>, H)),
    %% hidden input first, so that :last-child is the last item
    ?assert(has(<<"data-collapse-duration=\"250\"><input type=\"hidden\" name=\"open\" "
                  "value=\"0\"><div class=\"ah-navigationbar-item\">">>, H)),
    ?assert(has(<<"<div class=\"ah-navigationbar-header ah-navigationbar-header-expanded\" "
                  "id=\"nb-item-0-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"true\" "
                  "aria-controls=\"nb-item-0-content\"><span class=\"ah-navigationbar-header-text\">One"
                  "</span><span class=\"ah-navigationbar-arrow ah-navigationbar-arrow-up\" "
                  "aria-hidden=\"true\">▼</span></div>"/utf8>>, H)),
    ?assert(has(<<"<div class=\"ah-navigationbar-body\" id=\"nb-item-0-content\" role=\"region\" "
                  "aria-labelledby=\"nb-item-0-header\"><div class=\"ah-navigationbar-content\">"
                  "First</div></div>">>, H)),
    ?assert(has(<<"id=\"nb-item-1-content\" role=\"region\" aria-labelledby=\"nb-item-1-header\" "
                  "style=\"display:none;\"">>, H)),
    ?assert(has(<<"<span class=\"ah-navigationbar-header-text ah-navigationbar-header-text-structured\">"
                  "<span class=\"ah-navigationbar-header-title\">Two</span>"
                  "<span class=\"ah-navigationbar-header-subheader\">sub</span>"
                  "<span class=\"ah-navigationbar-header-extra\">x</span></span>">>, H)),
    ?assert(has(<<"<div class=\"ah-navigationbar-actions\">Act</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-navigationbar-header ah-navigationbar-disabled\" "
                  "id=\"nb-item-2-header\" role=\"button\" tabindex=\"-1\" aria-expanded=\"false\" "
                  "aria-controls=\"nb-item-2-content\" aria-disabled=\"true\">">>, H)).

navigationbar_options_test() ->
    H = r(?M:navigationbar(nav_items(), <<"0,1">>, [disable_gutters, no_arrow],
                           [{expand_mode, multiple}, {animation, none}, {toggle_mode, none},
                            {height, 300}, {width, <<"20rem">>}, {disabled, true}])),
    ?assert(has(<<"class=\"ah-navigationbar ah-navigationbar-no-gutters ah-navigationbar-vertical "
                  "ah-navigationbar-expand-multiple ah-navigationbar-disabled\"">>, H)),
    ?assert(has(<<"style=\"width:20rem;height:300px;\"">>, H)),
    ?assert(has(<<"data-ah-value=\"0,1\"">>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    ?assertNot(has_quiet(<<"ah-navigationbar-arrow">>, H)),
    ?assert(has(<<"ah-navigationbar-header-no-toggle">>, H)),
    %% only single_fit_height with a height fits the bodies
    ?assertNot(has_quiet(<<"data-fit">>, H)),
    ?assertEqual(3, count(<<"tabindex=\"-1\"">>, H)),
    F = r(?M:navigationbar(nav_items(), [1], [], [{height, 300}])),
    ?assert(has(<<" data-fit">>, F)),
    D = r(?M:navigationbar(nav_items(), undefined, [],
                           [{arrow_position, left}, {expand_icon, <<"+">>},
                            {collapse_icon, <<"-">>}])),
    ?assert(has(<<"<span class=\"ah-navigationbar-arrow ah-navigationbar-arrow-left "
                  "ah-navigationbar-arrow-dual\" aria-hidden=\"true\">"
                  "<span class=\"ah-navigationbar-icon ah-navigationbar-icon-expand\">+</span>"
                  "<span class=\"ah-navigationbar-icon ah-navigationbar-icon-collapse\">-</span>"
                  "</span>">>, D)),
    ?assert(has(<<"data-ah-value=\"\"">>, D)).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%%%===================================================================
%%% command
%%%===================================================================

cmd_items() ->
    [#{heading => <<"Files">>,
       items => [#{value => new, label => <<"New file">>, shortcut => <<"⌘N"/utf8>>,
                   icon => <<"+">>},
                 #{value => open, label => <<"Open">>, description => <<"Open a file">>,
                   disabled => true}]},
     <<"Loose">>,
     {theme, <<"Toggle theme">>}].

command_test() ->
    H = r(?M:command(cmd_items(), [<<"w-96">>], [{id, cmd}, {placeholder, <<"Search">>}])),
    ?assert(has(<<"<div class=\"ah-command w-96\" id=\"cmd\" data-ah=\"command\" data-ah-query=\"\" "
                  "data-close-on-select=\"true\">">>, H)),
    ?assert(has(<<"<input class=\"ah-command__input\" type=\"text\" id=\"cmd-input\" "
                  "placeholder=\"Search\" value=\"\" autocomplete=\"off\" spellcheck=\"false\" "
                  "aria-label=\"command input\" role=\"combobox\" aria-expanded=\"true\" "
                  "aria-autocomplete=\"list\" aria-controls=\"cmd-list\" data-command=\"cmd\" "
                  "data-empty=\"No results found.\">">>, H)),
    ?assert(has(<<"<div class=\"ah-command__list\" id=\"cmd-list\" role=\"listbox\">">>, H)),
    ?assert(has(<<"<div class=\"ah-command__group\" role=\"group\"><div class=\"ah-command__group-heading\" "
                  "role=\"presentation\">Files</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-command__item\" id=\"cmd-item-0\" role=\"option\" data-value=\"new\" "
                  "data-index=\"0\" data-active=\"true\" data-disabled=\"false\" aria-selected=\"true\">"
                  "<span class=\"ah-command__item-icon\" aria-hidden=\"true\">+</span>"
                  "<div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">New file</span>"
                  "</div><kbd class=\"ah-kbd ah-command__item-shortcut\">⌘N</kbd></div>"/utf8>>, H)),
    ?assert(has(<<"data-value=\"open\" data-index=\"1\" data-active=\"false\" data-disabled=\"true\" "
                  "aria-selected=\"false\" aria-disabled=\"true\">">>, H)),
    ?assert(has(<<"<span class=\"ah-command__item-desc\">Open a file</span>">>, H)),
    %% bare commands in a row share one unnamed group
    ?assert(has(<<"<div class=\"ah-command__group\" role=\"group\"><div class=\"ah-command__item\" "
                  "id=\"cmd-item-2\" role=\"option\" data-value=\"Loose\"">>, H)),
    ?assert(has(<<"id=\"cmd-item-3\" role=\"option\" data-value=\"theme\"">>, H)),
    ?assertEqual(2, count(<<"class=\"ah-command__group\"">>, H)),
    ?assert(has(<<"<div class=\"ah-command__empty\" hidden>No results found.</div>">>, H)).

command_palette_test() ->
    H = r(?M:command([<<"a">>], [palette], [{id, p}, {hotkey, <<"k">>}, {close_on_select, false},
                                             {empty_text, <<"Nothing">>}])),
    ?assert(has(<<"<div class=\"ah-command-overlay\" hidden><div class=\"ah-command ah-command-panel\" "
                  "id=\"p\" role=\"dialog\" aria-modal=\"true\" aria-label=\"Command palette\" "
                  "data-ah=\"command\" data-ah-query=\"\" data-hotkey=\"k\" "
                  "data-close-on-select=\"false\">">>, H)),
    E = r(?M:command([], [auto_focus], [{query, <<"zz">>}])),
    ?assert(has(<<"data-ah-query=\"zz\" data-auto-focus">>, E)),
    ?assert(has(<<"value=\"zz\"">>, E)),
    ?assert(has(<<"<div class=\"ah-command__empty\">No results found.</div>">>, E)),
    ?assertError({aihtml, {bad_command_item, _}}, r(?M:command([{1, 2, 3}], [], []))).

command_search_test() ->
    H = r(?M:command([], [], [{id, c}, {search, {?MODULE, search, #{}}}])),
    ?assert(has(<<"data-ah-remote">>, H)),
    {match, [Tok]} = re:run(H, <<"data-ah-on=\"input:([^\":]+):250\"">>,
                            [{capture, all_but_first, binary}]),
    ?assertEqual({ok, {?MODULE, search, #{}}}, aihtml_action:unsign(Tok)),
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:set_command_items(Ctx, #{data => #{<<"command">> => <<"c">>,
                                                          <<"empty">> => <<"None">>}},
                                         [{a, <<"Alpha">>}])
            end),
    Bin = iolist_to_binary(io_lib:format("~p", [Ops])),
    ?assert(has(<<"c-list">>, Bin)),
    ?assert(has(<<"morph_inner">>, Bin) orelse has(<<"morph-inner">>, Bin)),
    ?assert(has(<<"itemsLoaded">>, Bin)),
    ?assert(has(<<"c-item-0">>, Bin)),
    ?assert(has(<<"None">>, Bin)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := activity_bar}, #{name := navigationbar}, #{name := command}] = ?M:catalog(),
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         ?assertMatch([_ | _], Ms)
     end || N <- [activity_bar, navigationbar, command]],
    ?assertEqual([{set_command_items, 3}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:activity_bar(activity_items(), files, [right, <<"x">>],
                                   [{id, a}, {name, n}, {title, <<"t">>}])),
                 r(#ah_activity_bar{items = activity_items(), value = files, placement = right,
                                    css = [<<"x">>], id = a, name = n,
                                    attrs = [{title, <<"t">>}]})),
    ?assertEqual(r(?M:navigationbar(nav_items(), [0, 2], [square, no_arrow],
                                    [{id, b}, {expand_mode, toggle}, {animation, fade},
                                     {expand_duration, 100}, {disabled, true}])),
                 r(#ah_navigationbar{items = nav_items(), value = [0, 2], square = true,
                                     no_arrow = true, id = b, expand_mode = toggle,
                                     animation = fade, expand_duration = 100,
                                     disabled = true})),
    ?assertEqual(r(?M:command(cmd_items(), [palette], [{id, c}, {hotkey, <<"k">>},
                                                       {search, {?MODULE, s, #{}}}])),
                 r(#ah_command{items = cmd_items(), palette = true, id = c, hotkey = <<"k">>,
                               search = {?MODULE, s, #{}}})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z:]+:[^\":]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Tok | Rev] = lists:reverse(binary:split(T, <<":">>, [global])),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {iolist_to_binary(lists:join(<<":">>, lists:reverse(Rev))), Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, pick, #{id => 1}}},
                 Token(#ah_activity_bar{postback = {pick, #{id => 1}}})),
    ?assertEqual({<<"change">>, {?MODULE, open, #{}}},
                 Token(#ah_navigationbar{postback = open})),
    ?assertEqual({<<"ah:select">>, {other, run, #{}}},
                 Token(#ah_command{postback = run, delegate = other})),
    ?assertError({aihtml, {record_only_field, ah_command, postback}},
                 ?M:command([], [], [{postback, x}])).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, activity_bar, placement, top, _}},
                 r(#ah_activity_bar{placement = top})),
    ?assertError({aihtml, {bad_option, expand_mode, all}},
                 r(#ah_navigationbar{expand_mode = all})),
    ?assertError({aihtml, {bad_option, animation, spin}}, r(#ah_navigationbar{animation = spin})),
    ?assertError({aihtml, {bad_option, toggle_mode, hover}},
                 r(#ah_navigationbar{toggle_mode = hover})),
    ?assertError({aihtml, {bad_option, arrow_position, top}},
                 r(#ah_navigationbar{arrow_position = top})),
    ?assertError({aihtml, {bad_option, expand_duration, -1}},
                 r(#ah_navigationbar{expand_duration = -1})),
    ?assertError({aihtml, {bad_value, <<"a">>}}, r(#ah_navigationbar{value = <<"a">>})),
    ?assertError({aihtml, {bad_navigationbar_item, x}}, r(#ah_navigationbar{items = [x]})),
    ?assertError({aihtml, {bad_flag, navigationbar, square, yes}},
                 r(#ah_navigationbar{square = yes})),
    ?assertError({aihtml, {bad_option, search, nope}}, r(#ah_command{search = nope})),
    ?assertError({aihtml, {bad_option, close_on_select, 1}},
                 r(#ah_command{close_on_select = 1})),
    ?assertError({aihtml, {unknown_modifier, command, big, _}}, ?M:command([], [big], [])).

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

default(ah_activity_bar) -> #ah_activity_bar{};
default(ah_navigationbar) -> #ah_navigationbar{};
default(ah_command) -> #ah_command{}.
