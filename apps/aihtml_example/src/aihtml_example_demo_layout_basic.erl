%% @doc Demos of the layout components (aihtml_layout_basic), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_layout_basic).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([card_basic/0, card_header/0, card_media/0,
         panel_scroll/0, panel_header/0, panel_collapsed/0,
         expander_basic/0, expander_structured/0, expander_icons/0, expander_styles/0,
         expander_accordion/0,
         tabs_basic/0, tabs_positions/0, tabs_hover/0, tabs_scrollable/0,
         tab_bar_editor/0, tab_bar_fixed/0,
         breadcrumbs_basic/0, breadcrumbs_separators/0, breadcrumbs_collapsed/0,
         pagination_basic/0, pagination_full/0, pagination_siblings/0,
         pagination_simple/0, pagination_links/0, pagination_disabled/0,
         pagination_record/0,
         steps_basic/0, steps_wizard/0, steps_vertical/0,
         skeleton_text/0, skeleton_shapes/0,
         loader_overlay/0, loader_positions/0, loader_inline/0, loader_hidden/0,
         empty_basic/0, empty_compact/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => card, title => <<"Card">>,
       summary => <<"带标题、内容和底部的卡片容器。"/utf8>>,
       demos => [{<<"标题与正文"/utf8>>, card_basic},
                 {<<"副标题、右侧操作、底部、悬停"/utf8>>, card_header},
                 {<<"媒体区与无内边距正文"/utf8>>, card_media}]},
     #{component => panel, title => <<"Panel">>,
       summary => <<"可滚动的内容面板，可带标题栏、操作和折叠按钮。"/utf8>>,
       demos => [{<<"固定高度的滚动区"/utf8>>, panel_scroll},
                 {<<"标题栏、操作与折叠"/utf8>>, panel_header},
                 {<<"初始折叠"/utf8>>, panel_collapsed}]},
     #{component => expander, title => <<"Expander">>,
       summary => <<"点击标题展开或收起一块内容，值为 true 或 false。"/utf8>>,
       demos => [{<<"展开与收起"/utf8>>, expander_basic},
                 {<<"结构化标题与左侧箭头"/utf8>>, expander_structured},
                 {<<"加减号图标与淡入动画"/utf8>>, expander_icons},
                 {<<"标题在下、无边距、禁用"/utf8>>, expander_styles},
                 {<<"手风琴：同名的只展开一个"/utf8>>, expander_accordion}]},
     #{component => tabs, title => <<"Tabs">>,
       summary => <<"切换多块内容的标签页，值为当前标签的键。"/utf8>>,
       demos => [{<<"基本用法与禁用标签"/utf8>>, tabs_basic},
                 {<<"标签在下、左、右"/utf8>>, tabs_positions},
                 {<<"悬停切换、无动画"/utf8>>, tabs_hover},
                 {<<"可滚动的标签栏"/utf8>>, tabs_scrollable}]},
     #{component => tab_bar, title => <<"TabBar">>,
       summary => <<"编辑器式可关闭的标签条，值为当前标签的 id。"/utf8>>,
       demos => [{<<"可关闭、未保存标记与图标"/utf8>>, tab_bar_editor},
                 {<<"不可关闭"/utf8>>, tab_bar_fixed}]},
     #{component => breadcrumbs, title => <<"Breadcrumbs">>,
       summary => <<"层级路径导航，末项是当前页。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, breadcrumbs_basic},
                 {<<"分隔符与图标"/utf8>>, breadcrumbs_separators},
                 {<<"折叠中间项、末项可点"/utf8>>, breadcrumbs_collapsed}]},
     #{component => pagination, title => <<"Pagination">>,
       summary => <<"页码导航，值为当前页，服务端据此渲染新的一页。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, pagination_basic},
                 {<<"首末页、总数、每页条数、跳转"/utf8>>, pagination_full},
                 {<<"当前页两侧各两页"/utf8>>, pagination_siblings},
                 {<<"简洁模式"/utf8>>, pagination_simple},
                 {<<"链接模式：不需要脚本"/utf8>>, pagination_links},
                 {<<"禁用"/utf8>>, pagination_disabled},
                 {<<"record 写法"/utf8>>, pagination_record}]},
     #{component => steps, title => <<"Steps">>,
       summary => <<"分步流程指示，值为当前步骤的序号。"/utf8>>,
       demos => [{<<"步骤与说明"/utf8>>, steps_basic},
                 {<<"带内容面板和上一步、下一步"/utf8>>, steps_wizard},
                 {<<"竖排、出错与禁用的步骤"/utf8>>, steps_vertical}]},
     #{component => skeleton, title => <<"Skeleton">>,
       summary => <<"内容加载中的占位骨架。"/utf8>>,
       demos => [{<<"文本行"/utf8>>, skeleton_text},
                 {<<"圆形与矩形"/utf8>>, skeleton_shapes}]},
     #{component => loader, title => <<"Loader">>,
       summary => <<"转圈的加载指示，默认盖住所在的容器。"/utf8>>,
       demos => [{<<"遮罩容器"/utf8>>, loader_overlay},
                 {<<"文字位置"/utf8>>, loader_positions},
                 {<<"行内"/utf8>>, loader_inline},
                 {<<"先隐藏，用方法显示"/utf8>>, loader_hidden}]},
     #{component => empty, title => <<"Empty">>,
       summary => <<"没有内容时的占位：图标、标题、说明和操作。"/utf8>>,
       demos => [{<<"图标、说明与操作"/utf8>>, empty_basic},
                 {<<"紧凑"/utf8>>, empty_compact}]}].

%%% Card

-spec card_basic() -> aihtml:html().
card_basic() ->
    'div'(card(p(<<"Orders ship within two business days.">>), [],
               [{title, <<"Shipping">>}]),
          [<<"w-72">>], []).

-spec card_header() -> aihtml:html().
card_header() ->
    'div'(card(p(<<"3 open issues, 12 closed this week.">>), [hover],
               [{title, <<"Project Atlas">>}, {subtitle, <<"Updated today">>},
                {extra, button(<<"Edit">>, edit, [outlined, sm], [])},
                {footer, small(<<"Owner: Lin">>)}]),
          [<<"w-80">>], []).

-spec card_media() -> aihtml:html().
card_media() ->
    row([card(p(<<"A card with a media strip.">>), [],
              [{media, 'div'([], [<<"h-24 bg-gradient-to-r from-sky-400 to-indigo-500">>], [])},
               {title, <<"Media">>}]),
         card(ul([li(<<"Inbox">>, [<<"px-4 py-2 border-b border-line">>], []),
                  li(<<"Archive">>, [<<"px-4 py-2">>], [])]),
              [flush], [{title, <<"Flush body">>}])]).

%%% Panel

-spec panel_scroll() -> aihtml:html().
panel_scroll() ->
    'div'(panel([p(<<"Log line ", (integer_to_binary(I))/binary>>) || I <- lists:seq(1, 20)],
                [bordered, <<"p-2">>], [{height, 160}]),
          [<<"w-80">>], []).

-spec panel_header() -> aihtml:html().
panel_header() ->
    'div'(panel([p(<<"Deployed ", (integer_to_binary(I))/binary, " minutes ago">>)
                 || I <- lists:seq(1, 12)],
                [bordered],
                [{title, <<"Activity">>}, {actions, button(<<"Refresh">>, refresh, [outlined, sm], [])},
                 {collapsible, true}, {max_height, 180}]),
          [<<"w-80">>], []).

-spec panel_collapsed() -> aihtml:html().
panel_collapsed() ->
    'div'(panel(p(<<"Advanced settings go here.">>), [bordered],
                [{title, <<"Advanced">>}, {collapsible, true}, {collapsed, true}]),
          [<<"w-80">>], []).

%%% Expander

-spec expander_basic() -> aihtml:html().
expander_basic() ->
    col([expander(p(<<"Returns are free within 30 days.">>), [],
                  [{header, <<"Return policy">>}]),
         expander(p(<<"We ship worldwide.">>), [],
                  [{header, <<"Shipping">>}, {expanded, false},
                   {actions, button(<<"Contact us">>, contact, [outlined, sm], [])}])]).

-spec expander_structured() -> aihtml:html().
expander_structured() ->
    expander(p(<<"Invoice details.">>), [],
             [{header, #{title => <<"Invoice #1024">>, subheader => <<"Due in 5 days">>,
                         extra => <<"$320.00">>}},
              {arrow_position, left}, {expanded, false}]).

-spec expander_icons() -> aihtml:html().
expander_icons() ->
    expander(p(<<"Plus and minus swap places.">>), [square],
             [{header, <<"More options">>}, {expand_icon, <<"+">>},
              {collapse_icon, <<"−"/utf8>>}, {animation, fade}, {expanded, false}]).

-spec expander_styles() -> aihtml:html().
expander_styles() ->
    col([expander(p(<<"The header sits below.">>), [bottom],
                  [{header, <<"Header at the bottom">>}, {expanded, false}]),
         expander(p(<<"No frame around it.">>), [no_gutters],
                  [{header, <<"No gutters">>}, {expanded, false}]),
         expander(p(<<"Hidden">>), [disabled],
                  [{header, <<"Disabled">>}, {expanded, false}])]).

-spec expander_accordion() -> aihtml:html().
expander_accordion() ->
    col([expander(p(<<"Create an account first.">>), [],
                  [{header, <<"How do I start?">>}, {accordion, faq}]),
         expander(p(<<"Yes, any time from settings.">>), [],
                  [{header, <<"Can I cancel?">>}, {accordion, faq}, {expanded, false}]),
         expander(p(<<"Email support@example.com.">>), [],
                  [{header, <<"Where is support?">>}, {accordion, faq}, {expanded, false}])]).

%%% Tabs

-spec tabs_basic() -> aihtml:html().
tabs_basic() ->
    tabs([{overview, <<"Overview">>, p(<<"Product overview.">>)},
          {specs, <<"Specs">>, p(<<"Weight 1.2 kg, 13 inch display.">>)},
          {reviews, <<"Reviews">>, p(<<"No reviews yet.">>), #{disabled => true}},
          {faq, <<"FAQ">>, p(<<"Questions and answers.">>)}],
         specs, [], [{name, section}]).

-spec tabs_positions() -> aihtml:html().
tabs_positions() ->
    Tabs = [{a, <<"Mail">>, p(<<"Inbox">>)}, {b, <<"Calendar">>, p(<<"Today">>)},
            {c, <<"Contacts">>, p(<<"People">>)}],
    col([tabs(Tabs, a, [bottom], []),
         row([box(tabs(Tabs, b, [left], [])),
              box(tabs(Tabs, c, [right], []))])]).

-spec tabs_hover() -> aihtml:html().
tabs_hover() ->
    tabs([{day, <<"Day">>, p(<<"Hourly view.">>)}, {week, <<"Week">>, p(<<"Seven days.">>)},
          {month, <<"Month">>, p(<<"Whole month.">>)}],
         week, [], [{selection_mode, hover}, {animation, none}]).

-spec tabs_scrollable() -> aihtml:html().
tabs_scrollable() ->
    'div'(tabs([{I, <<"Document ", (integer_to_binary(I))/binary>>,
                 p(<<"Contents of document ", (integer_to_binary(I))/binary>>)}
                || I <- lists:seq(1, 10)],
               1, [], [{scrollable, true}]),
          [<<"w-96">>], []).

%%% Tab bar

-spec tab_bar_editor() -> aihtml:html().
tab_bar_editor() ->
    tab_bar([{index, <<"index.erl">>}, {core, <<"core.erl">>, #{dirty => true}},
             {readme, <<"README.md">>, #{icon => <<"📄"/utf8>>}}, {config, <<"rebar.config">>}],
            core, [], []).

-spec tab_bar_fixed() -> aihtml:html().
tab_bar_fixed() ->
    tab_bar([{one, <<"One">>}, {two, <<"Two">>}, {three, <<"Three">>}], one, [],
            [{closable, false}]).

%%% Breadcrumbs

-spec breadcrumbs_basic() -> aihtml:html().
breadcrumbs_basic() ->
    breadcrumbs([{<<"Home">>, <<"/">>}, {<<"Users">>, <<"/users">>}, <<"Lin">>], [], []).

-spec breadcrumbs_separators() -> aihtml:html().
breadcrumbs_separators() ->
    col([breadcrumbs([{<<"Home">>, <<"/">>}, {<<"Library">>, <<"/lib">>}, <<"Data">>], [],
                     [{separator, none}]),
         breadcrumbs([#{label => <<"Home">>, href => <<"/">>, icon => <<"⌂"/utf8>>},
                      {<<"Docs">>, <<"/docs">>}, <<"Guide">>], [],
                     [{separator, <<"›"/utf8>>}])]).

-spec breadcrumbs_collapsed() -> aihtml:html().
breadcrumbs_collapsed() ->
    col([breadcrumbs([{<<"Root">>, <<"#">>}, {<<"A">>, <<"#">>}, {<<"B">>, <<"#">>},
                      {<<"C">>, <<"#">>}, {<<"D">>, <<"#">>}, <<"Current">>], [],
                     [{max_items, 4}]),
         breadcrumbs([{<<"Home">>, <<"/">>}, {<<"Reports">>, <<"/reports">>}], [],
                     [{active_last, true}])]).

%%% Pagination

-spec pagination_basic() -> aihtml:html().
pagination_basic() ->
    pagination(200, 1, [], [{show_size_selector, false}, {name, page}]).

-spec pagination_full() -> aihtml:html().
pagination_full() ->
    pagination(500, 12, [], [{page_size, 20}, {show_first_last, true}, {show_total, true},
                             {show_jumper, true}]).

-spec pagination_siblings() -> aihtml:html().
pagination_siblings() ->
    pagination(300, 15, [], [{siblings, 2}, {show_size_selector, false}]).

-spec pagination_simple() -> aihtml:html().
pagination_simple() ->
    pagination(95, 3, [simple], [{show_size_selector, false}]).

-spec pagination_links() -> aihtml:html().
pagination_links() ->
    pagination(120, 4, [], [{href, <<"?page={page}&size={size}">>},
                            {show_size_selector, false}]).

-spec pagination_disabled() -> aihtml:html().
pagination_disabled() ->
    pagination(50, 2, [disabled], [{show_size_selector, false}]).

-spec pagination_record() -> aihtml:html().
pagination_record() ->
    #ah_pagination{total = 480, value = 6, page_size = 20, siblings = 1,
                   show_total = true, show_first_last = true, show_size_selector = false,
                   labels = #{total => <<"{0} orders">>}, name = page}.

%%% Steps

-spec steps_basic() -> aihtml:html().
steps_basic() ->
    steps([{<<"Account">>, <<"Create an account">>}, {<<"Profile">>, <<"Your details">>},
           {<<"Confirm">>, <<"Check and submit">>}], 1, [], []).

-spec steps_wizard() -> aihtml:html().
steps_wizard() ->
    steps([#{title => <<"Cart">>, content => p(<<"Your cart.">>)},
           #{title => <<"Shipping">>, content => p(<<"Shipping address.">>)},
           #{title => <<"Payment">>, content => p(<<"Payment method.">>)},
           #{title => <<"Done">>, content => p(<<"Thank you.">>)}],
          0, [], [{name, step}]).

-spec steps_vertical() -> aihtml:html().
steps_vertical() ->
    steps([{<<"Draft">>, <<"Written">>},
           #{title => <<"Review">>, status => error, description => <<"Changes requested">>},
           #{title => <<"Publish">>, disabled => true}],
          1, [vertical], [{clickable, false}]).

%%% Skeleton

-spec skeleton_text() -> aihtml:html().
skeleton_text() ->
    row([box(skeleton([], [])), box(skeleton([text, static], [{lines, 2}]))]).

-spec skeleton_shapes() -> aihtml:html().
skeleton_shapes() ->
    row([skeleton([circle], [{width, 48}]),
         box(skeleton([rect], [{height, 80}])),
         box(skeleton([rect], [{height, 80}, {radius, 0}]))]).

%%% Loader

-spec loader_overlay() -> aihtml:html().
loader_overlay() ->
    frame([p(<<"Refreshing the report…"/utf8>>), loader([], [])]).

-spec loader_positions() -> aihtml:html().
loader_positions() ->
    row([frame([loader([top], [{text, <<"Top">>}])]),
         frame([loader([left], [{text, <<"Left">>}])]),
         frame([loader([right], [{text, <<"Saving">>}])])]).

-spec loader_inline() -> aihtml:html().
loader_inline() ->
    row([loader([inline], []), loader([inline, right], [{text, <<"Syncing">>}])]).

-spec loader_hidden() -> aihtml:html().
loader_hidden() ->
    frame([p(<<"Shown by AH.invoke(el, 'show') or a server call.">>),
           loader([hidden], [{id, <<"report-loader">>}])]).

%%% Empty

-spec empty_basic() -> aihtml:html().
empty_basic() ->
    'div'(empty(button(<<"Create project">>, create, [], []), [],
                [{icon, <<"📭"/utf8>>}, {title, <<"No projects yet">>},
                 {description, <<"Projects you create will show up here.">>}]),
          [<<"w-80 border border-line rounded">>], []).

-spec empty_compact() -> aihtml:html().
empty_compact() ->
    'div'(empty([], [compact], [{title, <<"No results">>},
                                {description, <<"Try another search.">>}]),
          [<<"w-80 border border-line rounded">>], []).

%% Layout helpers shared by the demos.
row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).

col(Children) ->
    'div'(Children, [<<"flex flex-col gap-2">>], []).

box(Child) ->
    'div'(Child, [<<"w-64">>], []).

frame(Children) ->
    'div'(Children, [<<"relative w-56 h-32 p-3 border border-line rounded">>], []).
