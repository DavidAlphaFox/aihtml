-module(aihtml_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml.hrl").

r(Html) -> aihtml:render_binary(Html).

%%%===================================================================
%%% Elements, classes, attributes
%%%===================================================================

nesting_test() ->
    ?assertEqual(<<"<div class=\"a b\" id=\"x\"><p>hi</p><span>1</span></div>">>,
                 r('div'([p(<<"hi">>), span(1)], [a, <<"b">>], [{id, x}]))).

text_is_escaped_test() ->
    ?assertEqual(<<"<p>&lt;script&gt;&amp;&quot;&#39;</p>">>, r(p(<<"<script>&\"'">>))).

charlist_is_text_test() ->
    ?assertEqual(<<"<p>héllo</p>"/utf8>>, r(p("héllo"))).

integer_child_is_a_number_test() ->
    ?assertEqual(<<"<dd>8</dd><dd>100</dd>">>, r([dd(8), dd(100)])).

safe_is_verbatim_test() ->
    ?assertEqual(<<"<p><b>x</b></p>">>, r(p(safe(<<"<b>x</b>">>)))).

attribute_values_are_escaped_test() ->
    ?assertEqual(<<"<a href=\"/q?a=1&amp;b=&quot;2&quot;\">x</a>">>,
                 r(a(<<"x">>, [], [{href, <<"/q?a=1&b=\"2\"">>}]))).

boolean_and_dropped_attributes_test() ->
    ?assertEqual(<<"<input disabled>">>,
                 r(aihtml:void(input, [], [{disabled, true}, {checked, false},
                                           {value, undefined}]))).

underscore_and_data_attributes_test() ->
    ?assertEqual(<<"<span aria-label=\"l\" data-a=\"1\" data-b-c=\"x\"></span>">>,
                 r(span([], [], [{aria_label, l}, {data, #{a => 1, b_c => x}}]))).

later_attribute_wins_class_accumulates_test() ->
    ?assertEqual(<<"<div class=\"a b c\" id=\"2\"></div>">>,
                 r('div'([], [a], [{id, 1}, {class, <<"b">>}, [{id, 2}, {class, c}]]))).

map_attrs_test() ->
    ?assertEqual(<<"<p id=\"x\" title=\"t\"></p>">>, r(p([], [], #{title => t, id => x}))).

css_whitespace_is_normalised_test() ->
    ?assertEqual(<<"<p class=\"a b c\"></p>">>, r(p([], [<<"  a\n b ">>, ["c"]], []))).

bad_attribute_name_test() ->
    ?assertError({aihtml, {bad_attribute_name, <<"on\"x">>}},
                 span([], [], [{<<"on\"x">>, 1}])).

void_with_children_test() ->
    ?assertError({aihtml, {void_element_with_children, <<"input">>}},
                 aihtml:el(input, [<<"x">>], [], [])).

%%%===================================================================
%%% Prefabs
%%%===================================================================

button_test() ->
    ?assertEqual(<<"<button class=\"ah-btn ah-btn-lg ah-btn-danger mt-4\" type=\"submit\""
                   " value=\"save\">Save</button>">>,
                 r(button(<<"Save">>, save, [danger, lg, <<"mt-4">>], [{type, submit}]))).

button_default_variant_test() ->
    ?assertEqual(<<"<button class=\"ah-btn ah-btn-primary\" type=\"button\">Go</button>">>,
                 r(button(<<"Go">>, undefined, [], []))).

unknown_modifier_test() ->
    ?assertError({aihtml, {unknown_modifier, button, primay, _}},
                 button(<<"x">>, x, [primay], [])).

conflicting_modifiers_test() ->
    ?assertError({aihtml, {conflicting_modifiers, button, variant, [primary, danger]}},
                 button(<<"x">>, x, [primary, danger], [])).

checkbox_attrs_go_to_input_test() ->
    ?assertEqual(<<"<label class=\"ah-check mt-2\" data-ah=\"check\">"
                   "<input class=\"ah-check-input\" type=\"checkbox\" value=\"yes\""
                   " name=\"remember\" checked>"
                   "<span class=\"ah-check-label\">Remember</span></label>">>,
                 r(checkbox(<<"Remember">>, yes, [<<"mt-2">>], [{name, remember},
                                                               {checked, true}]))).

switch_has_role_and_track_test() ->
    H = r(switch(<<"On">>, 1, [], [])),
    ?assertMatch({_, _}, binary:match(H, <<"role=\"switch\"">>)),
    ?assertMatch({_, _}, binary:match(H, <<"ah-switch-track">>)).

select_marks_selected_test() ->
    ?assertEqual(<<"<select class=\"ah-select\" name=\"r\">"
                   "<option value=\"a\">A</option>"
                   "<option value=\"b\" selected>B</option>"
                   "<option value=\"c\">c</option></select>">>,
                 r(select([{a, <<"A">>}, {b, <<"B">>}, c], b, [], [{name, r}]))).

input_invalid_flag_test() ->
    ?assertEqual(<<"<input class=\"ah-input ah-input-invalid\" type=\"text\" value=\"v\""
                   " aria-invalid=\"true\">">>,
                 r(input(<<"v">>, [invalid], []))).

field_error_replaces_help_test() ->
    H = r(field(<<"L">>, input(<<>>, [], []), [], [{for, x}, {help, <<"h">>},
                                                  {error, <<"e">>}, {id, f}])),
    ?assertMatch({_, _}, binary:match(H, <<"<div class=\"ah-field ah-field-invalid\" id=\"f\">">>)),
    ?assertMatch({_, _}, binary:match(H, <<"<label class=\"ah-field-label\" for=\"x\">">>)),
    ?assertMatch({_, _}, binary:match(H, <<"role=\"alert\">e</p>">>)),
    ?assertEqual(nomatch, binary:match(H, <<">h</p>">>)).

card_title_option_test() ->
    ?assertEqual(<<"<div class=\"ah-card\" id=\"c\"><div class=\"ah-card-header\">"
                   "<h3 class=\"ah-card-title\">T</h3></div>"
                   "<div class=\"ah-card-body\">x</div></div>">>,
                 r(card(<<"x">>, [], [{title, <<"T">>}, {id, c}]))).

alert_dismissible_test() ->
    H = r(alert(<<"m">>, [error, dismissible], [])),
    ?assertMatch({_, _}, binary:match(H, <<"class=\"ah-alert ah-alert-error ah-alert-dismissible\"">>)),
    ?assertMatch({_, _}, binary:match(H, <<"data-ah-dismiss">>)).

tabs_test() ->
    H = r(tabs([{a, <<"A">>, <<"pa">>}, {b, <<"B">>, <<"pb">>}], b, [], [{id, t}])),
    ?assertMatch({_, _}, binary:match(H, <<"id=\"t-tab-b\" aria-controls=\"t-panel-b\""
                                           " aria-selected=\"true\"">>)),
    ?assertMatch({_, _}, binary:match(H, <<"data-ah-panel=\"a\" hidden>pa">>)),
    ?assertMatch({_, _}, binary:match(H, <<"data-ah-panel=\"b\">pb">>)).

tabs_default_active_is_first_test() ->
    H = r(tabs([{a, <<"A">>, <<"pa">>}, {b, <<"B">>, <<"pb">>}], undefined, [], [{id, t}])),
    ?assertMatch({_, _}, binary:match(H, <<"id=\"t-tab-a\" aria-controls=\"t-panel-a\""
                                           " aria-selected=\"true\"">>)).

every_catalog_prefab_has_a_function_test() ->
    Exports = aihtml:module_info(exports),
    [?assert(lists:keymember(Name, 1, Exports)) || #{name := Name} <- aihtml_catalog:prefabs()].

%%%===================================================================
%%% Fetch, theme, page
%%%===================================================================

fetch_attrs_test() ->
    ?assertEqual(<<"<button class=\"ah-btn ah-btn-primary\" type=\"button\" value=\"m\""
                   " data-ah-fetch=\"post\" data-ah-url=\"/more\" data-ah-target=\"#list\""
                   " data-ah-swap=\"append\" data-ah-confirm=\"Sure?\">More</button>">>,
                 r(button(<<"More">>, m, [], [fetch(post, <<"/more">>, <<"#list">>,
                                                   #{swap => append,
                                                     confirm => <<"Sure?">>})]))).

fetch_bad_method_test() ->
    ?assertError({aihtml, {bad_fetch_method, head}}, fetch(head, <<"/">>, this)).

theme_attrs_test() ->
    ?assertEqual([{<<"data-theme">>, <<"dark">>}, {<<"data-palette">>, <<"indigo">>},
                  {<<"data-typography">>, <<"sans">>}, {<<"data-skin">>, <<"brutal">>}],
                 aihtml_theme:attrs(#{appearance => dark, skin => <<"brutal">>})).

theme_bad_value_test() ->
    ?assertError({aihtml, {bad_theme_value, palette, pink, _}},
                 aihtml_theme:attrs(#{palette => pink})).

theme_values_are_in_the_css_test() ->
    {ok, Css} = file:read_file(filename:join(code:priv_dir(aihtml), "css/aihtml.css")),
    [?assertMatch({_, _}, binary:match(Css, <<"[", Attr/binary, "=\"",
                                               (atom_to_binary(V, utf8))/binary, "\"]">>))
     || {_, Attr, Values, Default} <- aihtml_theme:axes(), V <- Values, V =/= Default].

page_test() ->
    H = iolist_to_binary(aihtml:page(p(<<"x">>), #{title => <<"T">>,
                                                   theme => #{palette => rose}})),
    ?assertMatch(<<"<!DOCTYPE html>\n<html lang=\"en\" data-theme=\"light\""
                   " data-palette=\"rose\"", _/binary>>, H),
    ?assertMatch({_, _}, binary:match(H, <<"<title>T</title>">>)),
    ?assertMatch({_, _}, binary:match(H, <<"<link rel=\"stylesheet\" href=\"/aihtml/aihtml.css\">">>)),
    ?assertMatch({_, _}, binary:match(H, <<"<body class=\"ah-body\"><p>x</p>"
                                           "<script src=\"/aihtml/vendor/jquery.min.js\"></script>"
                                           "<script src=\"/aihtml/aihtml.js\"></script></body>">>)).
