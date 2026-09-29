-module(aihtml_display_tests).

-include_lib("eunit/include/eunit.hrl").

-define(D, aihtml_display).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog and examples
%%%===================================================================

catalog_names_are_exported_test() ->
    Names = [N || #{name := N} <- ?D:catalog()],
    ?assertEqual(16, length(Names)),
    [?assert(erlang:function_exported(?D, N, 3)) || N <- Names],
    [?assert(lists:member(C, [form, layout, overlay, data, media, text]))
     || #{category := C} <- ?D:catalog()].

every_component_has_an_example_test() ->
    Ex = ?D:examples(),
    [?assert(lists:keymember(N, 1, Ex)) || #{name := N} <- ?D:catalog()],
    [?assert(is_binary(r(H))) || {_, _, H} <- Ex].

unknown_modifier_fails_test() ->
    ?assertError({aihtml, {unknown_modifier, avatar, huge, _}}, ?D:avatar(<<"A">>, [huge], [])),
    ?assertError({aihtml, {unknown_modifier, alert, danger, _}}, ?D:alert(<<"x">>, [danger], [])).

conflicting_modifiers_fail_test() ->
    ?assertError({aihtml, {conflicting_modifiers, chip, variant, _}},
                 ?D:chip(<<"x">>, [filled, soft], [])),
    ?assertError({aihtml, {conflicting_modifiers, progressbar, orientation, _}},
                 ?D:progressbar(1, [horizontal, vertical], [])).

literal_classes_are_appended_test() ->
    ?assert(has(<<"class=\"ah-kbd mx-1\"">>, ?D:kbd(<<"K">>, [<<"mx-1">>], []))).

%%%===================================================================
%%% Media
%%%===================================================================

avatar_defaults_and_modifiers_test() ->
    H = ?D:avatar(<<"JD">>, [lg, square, success], []),
    ?assert(has(<<"class=\"ah-avatar\" data-size=\"lg\" data-shape=\"square\" data-color=\"success\"">>, H)),
    ?assert(has(<<"<span class=\"ah-avatar__fallback\" aria-hidden=\"true\">JD</span>">>, H)),
    ?assert(has(<<"data-size=\"md\" data-shape=\"circle\" data-color=\"primary\"">>,
                ?D:avatar(<<"x">>, [], []))),
    ?assert(has(<<">?</span>">>, ?D:avatar(undefined, [], []))).

avatar_image_test() ->
    H = ?D:avatar(<<"A">>, [], [{src, <<"/a.png?x=1&y=\"2\"">>}, {alt, <<"Ann">>}]),
    ?assert(has(<<"<img class=\"ah-avatar__image\" src=\"/a.png?x=1&amp;y=&quot;2&quot;\" alt=\"Ann\">">>, H)),
    ?assertNot(has(<<"role=\"img\"">>, H)),
    ?assert(has(<<"role=\"img\" aria-label=\"Ann\"">>, ?D:avatar(<<"A">>, [], [{alt, <<"Ann">>}]))).

badge_count_max_zero_test() ->
    ?assert(has(<<"data-invisible=\"false\" aria-hidden=\"true\">99+</span>">>,
                ?D:badge(<<"x">>, [], [{count, 120}]))),
    ?assert(has(<<">9+</span>">>, ?D:badge(<<"x">>, [], [{count, 12}, {max, 9}]))),
    ?assert(has(<<"data-invisible=\"true\"">>, ?D:badge(<<"x">>, [], [{count, 0}]))),
    ?assert(has(<<"data-invisible=\"false\"">>, ?D:badge(<<"x">>, [show_zero], [{count, 0}]))),
    ?assert(has(<<"data-invisible=\"true\"">>, ?D:badge(<<"x">>, [invisible], []))).

badge_dot_anchor_and_standalone_test() ->
    H = ?D:badge(<<"anchor">>, [online, circular, bottom, left], [{count, 5}]),
    ?assert(has(<<"data-overlap=\"circular\" data-anchor-vertical=\"bottom\" data-anchor-horizontal=\"left\"">>, H)),
    ?assert(has(<<"data-variant=\"online\" data-color=\"primary\" data-dot=\"true\"">>, H)),
    ?assertNot(has(<<">5<">>, H)),
    S = ?D:badge(undefined, [error], [{count, <<"<b>">>}]),
    ?assert(has(<<"ah-badge-root ah-badge-root--standalone">>, S)),
    ?assert(has(<<"data-invisible=\"false\">&lt;b&gt;</span>">>, S)).

chip_test() ->
    H = ?D:chip(<<"<Tag>">>, [removable, outlined, info, small], [{avatar, <<"JD">>}]),
    ?assert(has(<<"data-variant=\"outlined\" data-color=\"info\" data-size=\"small\" "
                  "data-disabled=\"false\" data-clickable=\"false\" data-ah=\"chip\" "
                  "data-ah-value=\"&lt;Tag&gt;\"">>, H)),
    ?assert(has(<<"<span class=\"ah-chip__avatar\">JD</span><span class=\"ah-chip__label\">&lt;Tag&gt;</span>">>, H)),
    ?assert(has(<<"<button class=\"ah-chip__delete\" type=\"button\"">>, H)),
    C = ?D:chip(<<"c">>, [clickable], [{value, 7}]),
    ?assert(has(<<"data-clickable=\"true\" data-ah=\"chip\" data-ah-value=\"7\" role=\"button\" tabindex=\"0\"">>, C)),
    ?assertNot(has(<<"ah-chip__delete">>, C)),
    ?assert(has(<<"data-disabled=\"true\"">>, ?D:chip(<<"d">>, [disabled], []))).

aspect_ratio_test() ->
    ?assert(has(<<"style=\"aspect-ratio: 16 / 9;\"">>, ?D:aspect_ratio([], [], []))),
    ?assert(has(<<"style=\"aspect-ratio: 4 / 3; max-width: 10px\"">>,
                ?D:aspect_ratio([], [], [{ratio, <<"4:3">>}, {style, <<"max-width: 10px">>}]))),
    ?assert(has(<<"aspect-ratio: 1.5;">>, ?D:aspect_ratio([], [], [{ratio, 1.5}]))),
    ?assert(has(<<"aspect-ratio: 21 / 9;">>, ?D:aspect_ratio([], [], [{ratio, {21, 9}}]))),
    ?assertError({aihtml, {bad_ratio, _}},
                 ?D:aspect_ratio([], [], [{ratio, <<"1;background:red">>}])).

%%%===================================================================
%%% Text
%%%===================================================================

kbd_test() ->
    ?assertEqual(<<"<kbd class=\"ah-kbd\" data-size=\"lg\">&lt;Esc&gt;</kbd>">>,
                 r(?D:kbd(<<"<Esc>">>, [lg], []))),
    ?assertEqual(<<"<kbd class=\"ah-kbd\" data-size=\"md\">Tab</kbd>">>, r(?D:kbd("Tab", [], []))),
    C = ?D:kbd([<<"Ctrl">>, <<"K">>], [], []),
    ?assert(has(<<"<kbd class=\"ah-kbd-combo\" data-size=\"md\"><kbd class=\"ah-kbd\" data-size=\"md\">Ctrl</kbd>"
                  "<span class=\"ah-kbd-combo__sep\" aria-hidden=\"true\">+</span>">>, C)).

time_ago_format_test() ->
    Now = 1790000000,
    T = fun(Ago) -> ?D:time_ago(Now - Ago, [], [{now, Now}]) end,
    ?assert(has(<<">just now</time>">>, T(59))),
    ?assert(has(<<">1m ago</time>">>, T(60))),
    ?assert(has(<<">59m ago</time>">>, T(3599))),
    ?assert(has(<<">2h ago</time>">>, T(7200))),
    ?assert(has(<<">29d ago</time>">>, T(29 * 86400))),
    ?assert(has(<<">1mo ago</time>">>, T(30 * 86400))),
    ?assert(has(<<">just now</time>">>, T(-500))).

time_ago_markup_test() ->
    Now = calendar:datetime_to_gregorian_seconds({{2026, 7, 22}, {8, 5, 0}}) - 62167219200,
    H = ?D:time_ago({{2026, 7, 22}, {8, 0, 0}}, [], [{now, Now}]),
    ?assertEqual(<<"<time class=\"ah-time-ago\" datetime=\"2026-07-22T08:00:00Z\" "
                   "title=\"2026-07-22 08:00:00 UTC\" data-ah=\"time-ago\" data-ah-title=\"true\">"
                   "5m ago</time>">>, r(H)),
    ?assert(has(<<"datetime=\"2026-07-22T08:00:00Z\"">>,
                ?D:time_ago(<<"2026-07-22T08:00:00Z">>, [], [{now, Now}]))),
    L = ?D:time_ago(Now - 120, [], [{now, Now}, {live, false}, {title, false},
                                    {labels, #{minutes => <<"<{n}> min">>}}]),
    ?assert(has(<<"data-ah-live=\"false\" data-ah-label-minutes=\"&lt;{n}&gt; min\">&lt;2&gt; min</time>">>, L)),
    ?assertNot(has(<<"title=">>, L)),
    ?assertError({aihtml, {bad_timestamp, _}}, ?D:time_ago(<<"yesterday">>, [], [])).

expandable_text_test() ->
    Long = binary:copy(<<"é"/utf8>>, 12),
    H = ?D:expandable_text(Long, [], [{threshold, 10}]),
    ?assert(has(<<"data-expanded=\"false\" data-truncated=\"true\"">>, H)),
    ?assert(has(<<"<span data-ah-part=\"short\">", (binary:copy(<<"é"/utf8>>, 10))/binary, "…"/utf8,
                  "</span><span data-ah-part=\"full\" hidden>">>, H)),
    ?assert(has(<<"aria-expanded=\"false\"">>, H)),
    ?assert(has(<<">展开</button>"/utf8>>, H)),
    E = ?D:expandable_text(Long, [], [{threshold, 10}, {expanded, true}, {collapse_label, <<"less">>}]),
    ?assert(has(<<"<span data-ah-part=\"short\" hidden>">>, E)),
    ?assert(has(<<">less</button>">>, E)),
    S = ?D:expandable_text(<<"<b>short</b>">>, [], []),
    ?assert(has(<<"data-truncated=\"false\"">>, S)),
    ?assert(has(<<"&lt;b&gt;short&lt;/b&gt;">>, S)),
    ?assertNot(has(<<"<button">>, S)).

alert_test() ->
    H = ?D:alert(<<"<msg>">>, [], []),
    ?assert(has(<<"<div class=\"ah-alert ah-alert-info\" role=\"alert\" data-ah=\"alert\">">>, H)),
    ?assert(has(<<"<div class=\"ah-alert-body\">&lt;msg&gt;</div>">>, H)),
    ?assert(has(<<"<svg">>, H)),
    ?assertNot(has(<<"ah-alert-close">>, H)),
    D = ?D:alert(<<"m">>, [error, dismissible, <<"mt-2">>], [{title, <<"T&">>}, {icon, false}, {id, a1}]),
    ?assert(has(<<"class=\"ah-alert ah-alert-error ah-alert-dismissible mt-2\"">>, D)),
    ?assert(has(<<"<div class=\"ah-alert-title\">T&amp;</div>">>, D)),
    ?assert(has(<<"<button class=\"ah-alert-close\" type=\"button\" aria-label=\"Close\">">>, D)),
    ?assert(has(<<"id=\"a1\"">>, D)),
    ?assertNot(has(<<"<svg">>, D)).

%%%===================================================================
%%% Data
%%%===================================================================

progressbar_test() ->
    H = ?D:progressbar(150, [show_text], [{min, 50}, {max, 250}]),
    ?assert(has(<<"class=\"ah-progressbar ah-progressbar-horizontal\" role=\"progressbar\" "
                  "aria-valuemin=\"50\" aria-valuemax=\"250\" aria-valuenow=\"150\" "
                  "aria-valuetext=\"50%\"">>, H)),
    ?assert(has(<<"<div class=\"ah-progressbar-value\" style=\"width: 50%;\">">>, H)),
    ?assert(has(<<"<span class=\"ah-progressbar-text\">50%</span>">>, H)),
    ?assert(has(<<"style=\"width: 100%;\"">>, ?D:progressbar(999, [], []))),
    ?assert(has(<<"style=\"display: none;\">0%</span>">>, ?D:progressbar(-5, [], []))).

progressbar_variants_test() ->
    V = ?D:progressbar(30, [vertical, reverse, success, striped, animated, disabled], []),
    ?assert(has(<<"class=\"ah-progressbar ah-progressbar-success ah-progressbar-reverse "
                  "ah-progressbar-vertical ah-progressbar-animated ah-progressbar-disabled "
                  "ah-progressbar-striped\"">>, V)),
    ?assert(has(<<"<div class=\"ah-progressbar-value-vertical\" style=\"height: 30%;\">">>, V)),
    ?assert(has(<<"aria-orientation=\"vertical\"">>, V)),
    I = ?D:progressbar(undefined, [indeterminate], []),
    ?assert(has(<<"ah-progressbar-indeterminate">>, I)),
    ?assert(has(<<"aria-busy=\"true\"">>, I)),
    ?assertNot(has(<<"aria-valuenow">>, I)),
    ?assert(has(<<"<div class=\"ah-progressbar-value\"></div>">>, I)).

progressbar_ranges_and_text_test() ->
    H = ?D:progressbar(50, [], [{color_ranges, [{30, success}, {80, <<"#ff0000">>}]},
                                {text, <<"<half>">>}]),
    ?assert(has(<<"data-range-index=\"0\" data-ah-stop=\"30\" style=\"background-color: "
                  "var(--ah-color-success); z-index: 2; width: 30%;\"">>, H)),
    ?assert(has(<<"data-range-index=\"1\" data-ah-stop=\"80\" style=\"background-color: "
                  "#ff0000; z-index: 1; width: 50%;\"">>, H)),
    ?assert(has(<<"&lt;half&gt;</span>">>, H)),
    ?assert(has(<<"data-ah-text=\"custom\"">>, H)),
    ?assertError({aihtml, {bad_color, _}},
                 ?D:progressbar(1, [], [{color_ranges, [{5, <<"red;}x{">>}]}])).

progress_circle_geometry_test() ->
    C = 2 * math:pi() * 45,
    Offset = fun(H) ->
                     {match, [O]} = re:run(r(H), "stroke-dashoffset=\"([0-9.]+)\"",
                                           [{capture, all_but_first, binary}]),
                     binary_to_float(<<O/binary, (case binary:match(O, <<".">>) of
                                                      nomatch -> <<".0">>; _ -> <<>> end)/binary>>)
             end,
    ?assert(abs(Offset(?D:progress_circle(0, [], [])) - C) < 0.001),
    ?assert(abs(Offset(?D:progress_circle(25, [], [])) - 0.75 * C) < 0.001),
    ?assert(abs(Offset(?D:progress_circle(100, [], []))) < 0.001),
    ?assert(abs(Offset(?D:progress_circle(250, [], []))) < 0.001),
    H = ?D:progress_circle(42.9, [lg, success], [{label, <<"Up<load>">>}]),
    ?assert(has(<<"viewBox=\"0 0 100 100\"">>, H)),
    ?assert(has(<<"cx=\"50\" cy=\"50\" r=\"45\"">>, H)),
    ?assert(has(<<"stroke-dasharray=\"282.7433\"">>, H)),
    ?assert(has(<<"class=\"ah-progress-circle ah-progress-circle--success ah-progress-circle--lg\"">>, H)),
    ?assert(has(<<">42%</span>">>, H)),
    ?assert(has(<<"aria-label=\"Up&lt;load&gt;\"">>, H)),
    ?assert(has(<<"<span class=\"ah-progress-circle-label\">Up&lt;load&gt;</span>">>, H)),
    ?assertNot(has(<<"ah-progress-circle-value">>, ?D:progress_circle(5, [], [{show_value, false}]))),
    ?assert(has(<<"ah-progress-circle-disabled">>, ?D:progress_circle(5, [disabled], []))).

meter_state_test() ->
    St = fun(V, O) ->
                 {match, [S]} = re:run(r(?D:meter(V, [], O)), "data-state=\"([a-z]+)\"",
                                       [{capture, all_but_first, binary}]),
                 S
         end,
    T = [{low, 25}, {high, 75}],
    ?assertEqual(<<"low">>, St(10, T)),
    ?assertEqual(<<"optimum">>, St(50, T)),
    ?assertEqual(<<"high">>, St(90, T)),
    ?assertEqual(<<"optimum">>, St(90, [{optimum, 90} | T])),
    ?assertEqual(<<"low">>, St(10, [{optimum, 90} | T])),
    ?assertEqual(<<"low">>, St(90, [{optimum, 5} | T])),
    ?assertEqual(<<"optimum">>, St(10, [{optimum, 5} | T])).

meter_markup_test() ->
    H = ?D:meter(30, [lg], [{min, 20}, {max, 40}, {label, <<"CPU&">>}, {show_value, true},
                             {helper_text, <<"h">>}]),
    ?assert(has(<<"class=\"ah-meter\" data-size=\"lg\"">>, H)),
    ?assert(has(<<"<span class=\"ah-meter__label\">CPU&amp;</span><span class=\"ah-meter__value\">30</span>">>, H)),
    ?assert(has(<<"role=\"meter\" aria-valuenow=\"30\" aria-valuemin=\"20\" aria-valuemax=\"40\"">>, H)),
    ?assert(has(<<"style=\"width: 50%;\"">>, H)),
    ?assert(has(<<"<div class=\"ah-meter__helper\">h</div>">>, H)),
    ?assertNot(has(<<"ah-meter__head">>, ?D:meter(1, [], []))).

statistic_format_test() ->
    N = fun(V, O) ->
                {match, [S]} = re:run(r(?D:statistic(V, [], O)),
                                      "ah-statistic__number\">([^<]*)<",
                                      [{capture, all_but_first, binary}, unicode]),
                S
        end,
    ?assertEqual(<<"1,284,500">>, N(1284500, [])),
    ?assertEqual(<<"1284500">>, N(1284500, [{group_separator, false}])),
    ?assertEqual(<<"-1,234.50">>, N(-1234.5, [{precision, 2}])),
    ?assertEqual(<<"999">>, N(999, [])),
    ?assertEqual(<<"1,000">>, N(1000.0, [])),
    ?assertEqual(<<"3">>, N(2.6, [{precision, 0}])),
    ?assertEqual(<<"n/a">>, N(<<"n/a">>, [])).

statistic_markup_test() ->
    H = ?D:statistic(5, [success], [{title, <<"T">>}, {prefix, <<"$">>}, {suffix, <<"%">>},
                                    {delta, -1.5}, {precision, 1}]),
    ?assert(has(<<"data-color=\"success\" data-loading=\"false\"">>, H)),
    ?assert(has(<<"<span class=\"ah-statistic__prefix\">$</span>">>, H)),
    ?assert(has(<<"data-direction=\"down\"">>, H)),
    ?assert(has(<<"<span>1.5</span>">>, H)),
    ?assert(has(<<"data-direction=\"flat\"">>, ?D:statistic(1, [], [{delta, 0}]))),
    ?assertNot(has(<<"ah-statistic__delta">>, ?D:statistic(1, [], []))),
    ?assert(has(<<"data-loading=\"true\"">>, ?D:statistic(1, [loading], []))).

kpi_card_test() ->
    H = ?D:kpi_card(<<"1<2">>, [success], [{title, <<"Users">>}, {trend, 5}, {icon, users},
                                           {trend_label, <<"vs">>}]),
    ?assert(has(<<"class=\"ah-kpi-card ah-kpi-card--success ah-kpi-card-trend-up\"">>, H)),
    ?assert(has(<<"<span class=\"ah-kpi-card-trend-value\">+5.0%</span>">>, H)),
    ?assert(has(<<"<div class=\"ah-kpi-card-value\">1&lt;2</div>">>, H)),
    ?assert(has(<<"ah-kpi-card-icon-wrapper">>, H)),
    ?assert(has(<<"ah-kpi-card-trend-label\">vs<">>, H)),
    D = ?D:kpi_card(<<"1">>, [disabled], [{trend, -3.25}]),
    ?assert(has(<<"class=\"ah-kpi-card ah-kpi-card-disabled ah-kpi-card-trend-down\"">>, D)),
    ?assert(has(<<">-3.3%<">>, D) orelse has(<<">-3.2%<">>, D)),
    ?assertNot(has(<<"ah-kpi-card-trend">>, ?D:kpi_card(<<"1">>, [], []))),
    ?assertError({aihtml, {unknown_icon, rocket}}, ?D:kpi_card(<<"1">>, [], [{icon, rocket}])).

timeline_test() ->
    Items = [#{date => <<"d0">>, title => <<"<t0>">>, description => <<"x">>},
             #{date => <<"d1">>, title => <<"t1">>, dot => success, expanded => true,
               description => <<"y">>},
             #{date => <<"d2">>, title => <<"t2">>}],
    H = ?D:timeline(Items, [], []),
    B = r(H),
    ?assert(has(<<"class=\"ah-timeline ah-timeline-position-both ah-collapsible\"">>, H)),
    %% both: item 0 on the far side (date near), item 1 near
    ?assertMatch({_, _}, binary:match(B, <<"<div class=\"ah-timeline-near-cell\"><div class=\"ah-timeline-date\">d0</div>">>)),
    ?assertMatch({_, _}, binary:match(B, <<"<div class=\"ah-timeline-far-cell\"><div class=\"ah-timeline-date\">d1</div>">>)),
    ?assert(has(<<"&lt;t0&gt;">>, H)),
    ?assert(has(<<"ah-timeline-dot ah-timeline-dot-success">>, H)),
    ?assert(has(<<"class=\"ah-timeline-item ah-timeline-item-expanded\" ah-collapsible role=\"button\" "
                  "tabindex=\"0\" aria-expanded=\"true\"">>, H)),
    %% no description, nothing to expand
    ?assert(has(<<"<div class=\"ah-timeline-item\"><div class=\"ah-timeline-item-pointer\">">>, H)),
    N = ?D:timeline(Items, [near, horizontal], [{collapsible, false}]),
    ?assert(has(<<"class=\"ah-timeline ah-timeline-position-near ah-timeline-horizontal\"">>, N)),
    ?assertNot(has(<<"ah-collapsible">>, N)),
    ?assertError({aihtml, {bad_dot, pink}}, ?D:timeline([#{dot => pink}], [], [])).

ranking_list_test() ->
    Items = [#{name => <<"<DE>">>, code => de, value => 10, tag => <<"Free">>, sub_value => <<"s">>},
             #{name => <<"US">>, code => <<"US">>, value => 9, tag => <<"Beta">>},
             #{name => <<"X">>, value => 1, rank => 7}],
    H = ?D:ranking_list(Items, [dense, clickable], [{title, <<"Top">>},
                                                    {tag_colors, #{<<"Beta">> => error}}]),
    ?assert(has(<<"class=\"ah-ranking-list ah-ranking-list--dense\"">>, H)),
    ?assert(has(<<"<span class=\"ah-ranking-list__rank\" data-rank=\"1\">1</span>">>, H)),
    ?assert(has(<<"data-rank=\"7\">7<">>, H)),
    ?assert(has(<<"🇩🇪"/utf8>>, H)),
    ?assert(has(<<"🇺🇸"/utf8>>, H)),
    ?assert(has(<<"&lt;DE&gt;">>, H)),
    ?assert(has(<<"ah-ranking-list__tag ah-ranking-list__tag--success\">Free">>, H)),
    ?assert(has(<<"ah-ranking-list__tag ah-ranking-list__tag--error\">Beta">>, H)),
    ?assert(has(<<"ah-ranking-list__item ah-ranking-list__item--clickable\" data-idx=\"0\" role=\"button\"">>, H)),
    M = ?D:ranking_list(Items, [], [{max_items, 1}, {show_rank, false}, {flag_style, none}]),
    ?assertNot(has(<<"US">>, M)),
    ?assertNot(has(<<"ah-ranking-list__rank">>, M)),
    ?assertNot(has(<<"ah-ranking-list__flag">>, M)).

tag_cloud_weights_test() ->
    Tags = [#{label => <<"a">>, value => 10}, #{label => <<"b">>, value => 20},
            {<<"c">>, 30, <<"/c?x=1&y">>}],
    H = ?D:tag_cloud(Tags, [], []),
    ?assert(has(<<"style=\"font-size: 10px;\"">>, H)),
    ?assert(has(<<"style=\"font-size: 17px;\"">>, H)),
    ?assert(has(<<"style=\"font-size: 24px;\" href=\"/c?x=1&amp;y\"">>, H)),
    ?assert(has(<<"<ul class=\"ah-tagcloud\">">>, H)),
    ?assert(has(<<"<div class=\"ah-tagcloud\" data-ah=\"tag-cloud\">">>, H)),
    One = ?D:tag_cloud([{<<"only">>, 5}], [], [{min_font_size, 1}, {max_font_size, 2},
                                              {font_size_unit, 'rem'}]),
    ?assert(has(<<"font-size: 1.5rem;">>, One)).

tag_cloud_options_test() ->
    Tags = [{<<"b x">>, 20}, {<<"a">>, 10}, {<<"c">>, 30}],
    G = ?D:tag_cloud(Tags, [], [{min_color, <<"#000000">>}, {max_color, <<"#ffffff">>}]),
    ?assert(has(<<"color: rgb(128,128,128);\"">>, G)),
    ?assert(has(<<"color: rgb(0,0,0);\"">>, G)),
    S = r(?D:tag_cloud(Tags, [], [{sort_by, value}, {sort_order, descending},
                                  {text_case, title_case}, {display_value, true}])),
    {P1, _} = binary:match(S, <<">C (30)<">>),
    {P2, _} = binary:match(S, <<">B X (20)<">>),
    ?assert(P1 < P2),
    L = ?D:tag_cloud(Tags, [], [{display_limit, 2}, {take_top_weighted, true}]),
    ?assertNot(has(<<">a<">>, L)),
    ?assert(has(<<">b x<">>, L)),
    F = ?D:tag_cloud(Tags, [], [{min_value, 15}, {max_value, 25}]),
    ?assertNot(has(<<">c<">>, F)),
    ?assertError({aihtml, {bad_color, _}},
                 ?D:tag_cloud(Tags, [], [{min_color, <<"red">>}, {max_color, <<"#fff000">>}])),
    ?assertError({aihtml, {bad_unit, _}}, ?D:tag_cloud(Tags, [], [{font_size_unit, <<"px;x">>}])).

tag_cloud_escaping_test() ->
    H = ?D:tag_cloud([{<<"<script>">>, 1}], [disabled], []),
    ?assert(has(<<"&lt;script&gt;">>, H)),
    ?assertNot(has(<<"<script>">>, H)),
    ?assert(has(<<"class=\"ah-tagcloud ah-tagcloud-disabled\"">>, H)).
