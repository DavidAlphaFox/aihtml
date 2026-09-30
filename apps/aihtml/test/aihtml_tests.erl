-module(aihtml_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml.hrl").

%% renders #ah_el{module = ?MODULE} in plain_element_record_test
-export([render/1]).

r(Html) -> aihtml:render_binary(Html).

%%%===================================================================
%%% Elements, classes, attributes
%%%===================================================================

nesting_test() ->
    ?assertEqual(<<"<div class=\"a b\" id=\"x\"><p>hi</p><span>1</span></div>">>,
                 r(ah_div([ah_p(<<"hi">>), ah_span(1)], [a, <<"b">>], [{id, x}]))).

text_is_escaped_test() ->
    ?assertEqual(<<"<p>&lt;script&gt;&amp;&quot;&#39;</p>">>, r(ah_p(<<"<script>&\"'">>))).

charlist_is_text_test() ->
    ?assertEqual(<<"<p>héllo</p>"/utf8>>, r(ah_p("héllo"))).

integer_child_is_a_number_test() ->
    ?assertEqual(<<"<dd>8</dd><dd>100</dd>">>, r([ah_dd(8), ah_dd(100)])).

safe_is_verbatim_test() ->
    ?assertEqual(<<"<p><b>x</b></p>">>, r(ah_p(safe(<<"<b>x</b>">>)))).

attribute_values_are_escaped_test() ->
    ?assertEqual(<<"<a href=\"/q?a=1&amp;b=&quot;2&quot;\">x</a>">>,
                 r(ah_a(<<"x">>, [], [{href, <<"/q?a=1&b=\"2\"">>}]))).

boolean_and_dropped_attributes_test() ->
    ?assertEqual(<<"<input disabled>">>,
                 r(aihtml:ah_void(input, [], [{disabled, true}, {checked, false},
                                              {value, undefined}]))).

underscore_and_data_attributes_test() ->
    ?assertEqual(<<"<span aria-label=\"l\" data-a=\"1\" data-b-c=\"x\"></span>">>,
                 r(ah_span([], [], [{aria_label, l}, {data, #{a => 1, b_c => x}}]))).

later_attribute_wins_class_accumulates_test() ->
    ?assertEqual(<<"<div class=\"a b c\" id=\"2\"></div>">>,
                 r(ah_div([], [a], [{id, 1}, {class, <<"b">>}, [{id, 2}, {class, c}]]))).

map_attrs_test() ->
    ?assertEqual(<<"<p id=\"x\" title=\"t\"></p>">>, r(ah_p([], [], #{title => t, id => x}))).

css_whitespace_is_normalised_test() ->
    ?assertEqual(<<"<p class=\"a b c\"></p>">>, r(ah_p([], [<<"  a\n b ">>, ["c"]], []))).

bad_attribute_name_test() ->
    %% attributes are checked when rendering; the tag when building
    ?assertError({aihtml, {bad_attribute_name, <<"on\"x">>}},
                 r(ah_span([], [], [{<<"on\"x">>, 1}]))),
    ?assertError({aihtml, {bad_tag, <<"no tag">>}}, aihtml:ah_el(<<"no tag">>, [], [], [])).

%% Plain tags are #ah_el{} records: data until rendered, and they can be
%% written directly, with an id and a postback like components.
plain_element_record_test() ->
    E = ah_div([<<"x">>], [<<"p-2">>], [{title, t}]),
    ?assertMatch(#ah_el{tag = <<"div">>, body = [<<"x">>], css = [<<"p-2">>]}, E),
    ?assertEqual(r(E), r(#ah_el{tag = 'div', body = [<<"x">>], css = [<<"p-2">>],
                                attrs = [{title, t}]})),
    ?assertEqual(<<"<section class=\"card\" id=\"s\" title=\"t\">hi</section>">>,
                 r(#ah_el{tag = section, body = <<"hi">>, css = [<<"card">>], id = s,
                          attrs = [{title, t}]})),
    %% a void tag may leave body at its default
    ?assertEqual(<<"<img src=\"a.png\" alt=\"\">">>,
                 r(#ah_el{tag = img, attrs = [{src, <<"a.png">>}, {alt, <<>>}]})),
    ?assertEqual(r(ah_img([], [{src, <<"a.png">>}, {alt, <<>>}])),
                 r(#ah_el{tag = img, body = void, attrs = [{src, <<"a.png">>}, {alt, <<>>}]})),
    ?assertError({aihtml, {void_element_with_children, <<"img">>}},
                 r(#ah_el{tag = img, body = <<"x">>})),
    ?assertError({aihtml, {not_a_void_element, <<"p">>}}, r(#ah_el{tag = p, body = void})),
    %% postback: click, submit for a form, change for inputs
    Ev = fun(E1) -> {match, [V]} = re:run(r(E1), <<"data-ah-on=\"([a-z]+):">>,
                                          [{capture, all_but_first, binary}]),
                    V end,
    ?assertEqual(<<"click">>, Ev(#ah_el{tag = 'div', postback = go})),
    ?assertEqual(<<"submit">>, Ev(#ah_el{tag = form, postback = save})),
    ?assertEqual(<<"change">>, Ev(#ah_el{tag = select, postback = pick})),
    %% another module can render a plain element
    ?assertEqual(<<"<b>wrapped</b>">>, r(#ah_el{module = ?MODULE, tag = p})).

-spec render(tuple()) -> aihtml:html().
render(#ah_el{}) -> aihtml:ah_el(b, <<"wrapped">>, [], []).

void_with_children_test() ->
    ?assertError({aihtml, {void_element_with_children, <<"input">>}},
                 aihtml:ah_el(input, [<<"x">>], [], [])).

%%%===================================================================
%%% Prefabs
%%%===================================================================

%% Modifier resolution, against an entry that is not in any group.
-define(ENTRY, #{name => thing, category => test, signature => <<"thing()">>,
                 root => <<"ah-thing">>,
                 groups => #{variant => {[primary, danger], primary}, size => {[sm, lg], none}},
                 flags => [block], classes => #{lg => [<<"ah-thing-large">>]},
                 options => [title]}).

modifiers_test() ->
    ?assertEqual([<<"ah-thing">>, <<"ah-thing-large">>, <<"ah-thing-danger">>, <<"ah-thing-block">>,
                  <<"mt-2">>],
                 aihtml_catalog:classes(?ENTRY, [danger, lg, block, <<"mt-2">>])),
    ?assertEqual([<<"ah-thing">>, <<"ah-thing-primary">>], aihtml_catalog:classes(?ENTRY, [])).

unknown_modifier_test() ->
    ?assertError({aihtml, {unknown_modifier, thing, primay, _}},
                 aihtml_catalog:classes(?ENTRY, [primay])).

conflicting_modifiers_test() ->
    ?assertError({aihtml, {conflicting_modifiers, thing, variant, [primary, danger]}},
                 aihtml_catalog:classes(?ENTRY, [primary, danger])).

options_are_split_from_attrs_test() ->
    ?assertEqual({#{title => <<"T">>}, [{id, x}]},
                 aihtml_catalog:split_options(?ENTRY, [{title, <<"T">>}, {id, x}])).

%% The facade function of every entry is ah_<name>, and the signature says so.
every_catalog_entry_has_a_facade_function_test() ->
    Exports = aihtml:module_info(exports),
    [begin
         F = aihtml_catalog:builder(Name),
         ?assert(lists:keymember(F, 1, Exports)),
         [Fb | _] = binary:split(Sig, <<"(">>),
         ?assertEqual(atom_to_binary(F), Fb)
     end || #{name := Name, signature := Sig} <- aihtml_catalog:prefabs()].

%%%===================================================================
%%% Fetch, theme, page
%%%===================================================================

fetch_attrs_test() ->
    ?assertEqual(<<"<button data-ah-fetch=\"post\" data-ah-url=\"/more\" data-ah-target=\"#list\""
                   " data-ah-swap=\"append\" data-ah-confirm=\"Sure?\">More</button>">>,
                 r(aihtml:ah_el(button, <<"More">>, [], [fetch(post, <<"/more">>, <<"#list">>,
                                                               #{swap => append,
                                                                 confirm => <<"Sure?">>})]))).

fetch_bad_method_test() ->
    ?assertError({aihtml, {bad_fetch_method, head}}, fetch(head, <<"/">>, this)).

theme_attrs_test() ->
    ?assertEqual([{<<"data-theme">>, <<"dark">>}, {<<"data-palette">>, <<"default">>},
                  {<<"data-typography">>, <<"default">>}, {<<"data-skin">>, <<"brutal">>}],
                 aihtml_theme:attrs(#{appearance => dark, skin => <<"brutal">>})).

theme_bad_value_test() ->
    ?assertError({aihtml, {bad_theme_value, palette, pink, _}},
                 aihtml_theme:attrs(#{palette => pink})).

theme_values_are_in_the_css_test() ->
    Dir = filename:join(code:priv_dir(aihtml), "css"),
    Css = iolist_to_binary([element(2, file:read_file(F))
                            || F <- filelib:wildcard(filename:join([Dir, "**", "*.css"]))]),
    [?assertMatch({V, {_, _}}, {V, binary:match(Css, [<<"[", Attr/binary, "=\"", V/binary, "\"]">>,
                                                    <<"[", Attr/binary, "=", V/binary, "]">>])})
     || {_, Attr, Values, Default} <- aihtml_theme:axes(), A <- Values, A =/= Default,
        V <- [atom_to_binary(A, utf8)]].

page_test() ->
    H = iolist_to_binary(aihtml:page(ah_p(<<"x">>), #{title => <<"T">>,
                                                   theme => #{palette => green}})),
    ?assertMatch(<<"<!DOCTYPE html>\n<html lang=\"en\" data-theme=\"light\""
                   " data-palette=\"green\"", _/binary>>, H),
    ?assertMatch({_, _}, binary:match(H, <<"<title>T</title>">>)),
    %% the stylesheet has a content hash, found in its manifest
    Css = aihtml_assets:css(),
    ?assertMatch(<<"aihtml-", _:8/binary, ".css">>, Css),
    ?assertMatch({_, _}, binary:match(H, <<"<link rel=\"stylesheet\" href=\"/aihtml/css/", Css/binary, "\">">>)),
    %% the runtime is the bundle's entry module, found in its manifest
    #{file := Entry, imports := Imports} = aihtml_assets:entry(),
    ?assertMatch({_, _}, binary:match(H, <<"<body class=\"ah-body\" data-ah-action=\"/aihtml/action\" data-ah-events=\"/aihtml/events\"><p>x</p>"
                                           "<script type=\"module\" src=\"/aihtml/js/", Entry/binary, "\"></script></body>">>)),
    [?assertMatch({_, _}, binary:match(H, <<"<link rel=\"modulepreload\" href=\"/aihtml/js/", I/binary, "\">">>))
     || I <- Imports],
    ?assertEqual(nomatch, binary:match(H, <<"jquery">>)),
    %% options: another mount point, jQuery for the page's own scripts,
    %% deferred extra scripts, no runtime
    H2 = iolist_to_binary(aihtml:page(ah_p(<<"x">>), #{assets => <<"/static/ah/">>,
                                                    jquery => <<"/j.js">>, js => [<<"/app.js">>]})),
    ?assertMatch({_, _}, binary:match(H2, <<"<script src=\"/j.js\"></script><script type=\"module\" src=\"/static/ah/js/",
                                            Entry/binary, "\"></script><script src=\"/app.js\" defer></script>">>)),
    %% the stylesheet follows `assets' unless `css' is given
    ?assertMatch({_, _}, binary:match(H2, <<"<link rel=\"stylesheet\" href=\"/static/ah/css/", Css/binary, "\">">>)),
    H4 = iolist_to_binary(aihtml:page(ah_p(<<"x">>), #{assets => <<"/static/ah/">>,
                                                    css => [<<"/app.css">>]})),
    ?assertMatch({_, _}, binary:match(H4, <<"<link rel=\"stylesheet\" href=\"/app.css\">">>)),
    ?assertEqual(nomatch, binary:match(H4, Css)),
    H3 = iolist_to_binary(aihtml:page(ah_p(<<"x">>), #{runtime => false})),
    ?assertEqual(nomatch, binary:match(H3, <<"<script type=\"module\"">>)).

%% data-ah behaviour names are lower-case words joined by hyphens
behaviour_names_test() ->
    Bad = [{N, B} || #{name := N, behavior := B} <- aihtml_catalog:prefabs(),
                     B =/= none,
                     re:run(B, <<"^[a-z]+(-[a-z]+)*$">>) =:= nomatch],
    ?assertEqual([], Bad).

%% every behaviour of the catalog is defined in the browser runtime, so the
%% bundle's lazy loader can find it: AH.register("<name>") in a component
%% file (or core.ts), or "// ah-define: <name>" for a helper-registered one
behaviours_are_defined_in_js_test() ->
    %% the sources: priv and src are symlinked into _build, assets is not
    Js = filename:join([code:lib_dir(aihtml), "src", "..", "assets", "js"]),
    Src = iolist_to_binary([element(2, file:read_file(F))
                            || F <- [filename:join(Js, "core.ts") |
                                     filelib:wildcard(filename:join(Js, "components/*.ts"))]]),
    Missing = [B || #{behavior := B} <- aihtml_catalog:prefabs(), B =/= none,
                    binary:match(Src, [<<"register(\"", B/binary, "\"">>,
                                       <<"// ah-define: ", B/binary, "\n">>]) =:= nomatch],
    ?assertEqual([], Missing).

page_seo_test() ->
    H = iolist_to_binary(aihtml:page(ah_p(<<"x">>), #{
          title => <<"T">>,
          description => <<"A <page> & more">>,
          canonical => <<"https://ex.com/a">>,
          robots => <<"index, follow">>,
          alternates => [{<<"zh">>, <<"https://ex.com/zh/a">>}],
          og => #{title => <<"T">>, image => [<<"https://ex.com/1.png">>, <<"https://ex.com/2.png">>],
                  type => article},
          meta => [{<<"twitter:card">>, <<"summary">>}],
          json_ld => #{<<"@type">> => <<"Article">>, headline => <<"</script><b>">>}})),
    Has = fun(B) -> ?assertMatch({_, _}, binary:match(H, B)) end,
    Has(<<"<meta name=\"description\" content=\"A &lt;page&gt; &amp; more\">">>),
    Has(<<"<meta name=\"robots\" content=\"index, follow\">">>),
    Has(<<"<link rel=\"canonical\" href=\"https://ex.com/a\">">>),
    Has(<<"<link rel=\"alternate\" hreflang=\"zh\" href=\"https://ex.com/zh/a\">">>),
    %% og keys sorted, one tag per image
    Has(<<"<meta property=\"og:image\" content=\"https://ex.com/1.png\">"
          "<meta property=\"og:image\" content=\"https://ex.com/2.png\">"
          "<meta property=\"og:title\" content=\"T\">"
          "<meta property=\"og:type\" content=\"article\">">>),
    Has(<<"<meta name=\"twitter:card\" content=\"summary\">">>),
    %% JSON-LD cannot close its script element
    Has(<<"<script type=\"application/ld+json\">{\"@type\":\"Article\","
          "\"headline\":\"\\u003c/script>\\u003cb>\"}</script>">>),
    ?assertEqual(nomatch, binary:match(H, <<"</script><b>">>)),
    %% none of it without the options
    P = iolist_to_binary(aihtml:page(ah_p(<<"x">>), #{})),
    ?assertEqual(nomatch, binary:match(P, [<<"og:">>, <<"canonical">>, <<"ld+json">>, <<"description">>])).
