%% @doc Demos of the display components (aihtml_display), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_display).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([avatar_sizes/0, avatar_shapes/0, avatar_images/0,
         badge_counts/0, badge_status/0, badge_corners/0, badge_standalone/0,
         chip_variants/0, chip_features/0,
         aspect_ratios/0, aspect_ratio_image/0,
         kbd_keys/0, kbd_combos/0,
         time_ago_units/0, time_ago_options/0,
         expandable_default/0, expandable_labels/0,
         alert_variants/0, alert_dismissible/0,
         progress_values/0, progress_styles/0, progress_ranges/0, progress_vertical/0,
         circle_sizes/0, circle_colors/0, circle_states/0,
         meter_states/0, meter_sizes/0,
         statistic_basic/0, statistic_delta/0,
         kpi_trends/0, kpi_colors/0, kpi_record/0,
         timeline_both/0, timeline_near/0, timeline_horizontal/0,
         ranking_basic/0, ranking_dense/0,
         tag_cloud_weights/0, tag_cloud_gradient/0, tag_cloud_values/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => avatar, title => <<"Avatar">>,
       summary => <<"头像，图片取不到时回落为首字母。"/utf8>>,
       demos => [{<<"尺寸"/utf8>>, avatar_sizes},
                 {<<"形状与颜色"/utf8>>, avatar_shapes},
                 {<<"图片与加载失败"/utf8>>, avatar_images}]},
     #{component => badge, title => <<"Badge">>,
       summary => <<"挂在元素角上的计数、圆点或在线状态。"/utf8>>,
       demos => [{<<"计数与上限"/utf8>>, badge_counts},
                 {<<"圆点与在线状态"/utf8>>, badge_status},
                 {<<"角落位置"/utf8>>, badge_corners},
                 {<<"独立使用"/utf8>>, badge_standalone}]},
     #{component => chip, title => <<"Chip">>,
       summary => <<"可点击、可删除的小标签。"/utf8>>,
       demos => [{<<"变体与颜色"/utf8>>, chip_variants},
                 {<<"头像、删除、点击、禁用"/utf8>>, chip_features}]},
     #{component => aspect_ratio, title => <<"AspectRatio">>,
       summary => <<"按固定宽高比约束内容。"/utf8>>,
       demos => [{<<"常用比例"/utf8>>, aspect_ratios},
                 {<<"图片铺满"/utf8>>, aspect_ratio_image}]},
     #{component => kbd, title => <<"Kbd">>,
       summary => <<"键盘按键与组合键。"/utf8>>,
       demos => [{<<"单键与尺寸"/utf8>>, kbd_keys},
                 {<<"组合键"/utf8>>, kbd_combos}]},
     #{component => time_ago, title => <<"TimeAgo">>,
       summary => <<"「3m ago」式相对时间，浏览器每分钟刷新。"/utf8>>,
       demos => [{<<"各时间单位"/utf8>>, time_ago_units},
                 {<<"自定义文案、固定时间"/utf8>>, time_ago_options}]},
     #{component => expandable_text, title => <<"ExpandableText">>,
       summary => <<"长文本截断，点「展开」看全文。"/utf8>>,
       demos => [{<<"默认"/utf8>>, expandable_default},
                 {<<"自定义按钮文案、初始展开"/utf8>>, expandable_labels}]},
     #{component => alert, title => <<"Alert">>,
       summary => <<"页面内的提示框，四种语气，可关闭。"/utf8>>,
       demos => [{<<"四种变体"/utf8>>, alert_variants},
                 {<<"标题与关闭按钮"/utf8>>, alert_dismissible}]},
     #{component => progressbar, title => <<"Progressbar">>,
       summary => <<"线性进度条，支持文字、颜色分段、条纹与不确定状态。"/utf8>>,
       demos => [{<<"数值与文字"/utf8>>, progress_values},
                 {<<"颜色、条纹、不确定"/utf8>>, progress_styles},
                 {<<"颜色分段"/utf8>>, progress_ranges},
                 {<<"竖向与反向"/utf8>>, progress_vertical}]},
     #{component => progress_circle, title => <<"ProgressCircle">>,
       summary => <<"环形进度，适合放在卡片角上。"/utf8>>,
       demos => [{<<"尺寸"/utf8>>, circle_sizes},
                 {<<"颜色"/utf8>>, circle_colors},
                 {<<"标签、不确定、禁用"/utf8>>, circle_states}]},
     #{component => meter, title => <<"Meter">>,
       summary => <<"带阈值分档的量表：偏低、正常、偏高。"/utf8>>,
       demos => [{<<"三种状态"/utf8>>, meter_states},
                 {<<"尺寸与说明文字"/utf8>>, meter_sizes}]},
     #{component => statistic, title => <<"Statistic">>,
       summary => <<"大号数字，带前后缀、千分位和涨跌。"/utf8>>,
       demos => [{<<"前后缀与精度"/utf8>>, statistic_basic},
                 {<<"涨跌与加载中"/utf8>>, statistic_delta}]},
     #{component => kpi_card, title => <<"KpiCard">>,
       summary => <<"指标卡：一个数字加环比趋势。"/utf8>>,
       demos => [{<<"趋势与图标"/utf8>>, kpi_trends},
                 {<<"颜色与禁用"/utf8>>, kpi_colors},
                 {<<"record 写法"/utf8>>, kpi_record}]},
     #{component => timeline, title => <<"Timeline">>,
       summary => <<"按时间排列的事件轴，卡片可展开。"/utf8>>,
       demos => [{<<"两侧交替"/utf8>>, timeline_both},
                 {<<"单侧"/utf8>>, timeline_near},
                 {<<"横向"/utf8>>, timeline_horizontal}]},
     #{component => ranking_list, title => <<"RankingList">>,
       summary => <<"带名次、国旗和标签的排行榜。"/utf8>>,
       demos => [{<<"国旗、标签、奖牌色"/utf8>>, ranking_basic},
                 {<<"紧凑、可点击、前 N 名"/utf8>>, ranking_dense}]},
     #{component => tag_cloud, title => <<"TagCloud">>,
       summary => <<"按权重调字号的标签云。"/utf8>>,
       demos => [{<<"按权重调字号"/utf8>>, tag_cloud_weights},
                 {<<"颜色渐变与排序"/utf8>>, tag_cloud_gradient},
                 {<<"显示权重、大小写、取前 N 个"/utf8>>, tag_cloud_values}]}].

%%% Avatar

-spec avatar_sizes() -> aihtml:html().
avatar_sizes() ->
    row([avatar(<<"SG">>, [sm], []),
         avatar(<<"SG">>, [], []),
         avatar(<<"SG">>, [lg], []),
         avatar(<<"SG">>, [xl], [])]).

-spec avatar_shapes() -> aihtml:html().
avatar_shapes() ->
    row([avatar(<<"AB">>, [square], []),
         avatar(<<"CD">>, [rounded, success], []),
         avatar(<<"EF">>, [warning], []),
         avatar(<<"GH">>, [error], []),
         avatar(<<"IJ">>, [info], []),
         avatar(<<"KL">>, [secondary], []),
         avatar(undefined, [], [])]).

-spec avatar_images() -> aihtml:html().
avatar_images() ->
    Photo = <<"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 40 40'>"
              "<rect width='40' height='40' fill='%2360a5fa'/><circle cx='20' cy='16' r='7' fill='white'/>"
              "<rect x='8' y='26' width='24' height='14' rx='7' fill='white'/></svg>">>,
    row([avatar(<<"IM">>, [lg], [{src, Photo}, {alt, <<"Jane">>}]),
         avatar(<<"BR">>, [lg], [{src, <<"data:image/png;base64,AAAA">>}, {alt, <<"Broken">>}])]).

%%% Badge

-spec badge_counts() -> aihtml:html().
badge_counts() ->
    row([badge(avatar(<<"A">>, [square], []), [], [{count, 5}]),
         badge(avatar(<<"B">>, [square], []), [error], [{count, 120}]),
         badge(avatar(<<"C">>, [square], []), [info], [{count, 12}, {max, 9}]),
         badge(avatar(<<"D">>, [square], []), [success, show_zero], [{count, 0}])]).

-spec badge_status() -> aihtml:html().
badge_status() ->
    row([badge(avatar(<<"D">>, [square], []), [dot, warning], []),
         badge(avatar(<<"E">>, [], []), [online, circular, bottom], []),
         badge(avatar(<<"F">>, [], []), [busy, circular, bottom], []),
         badge(avatar(<<"G">>, [], []), [away, circular, bottom], []),
         badge(avatar(<<"H">>, [], []), [offline, circular, bottom], [])]).

-spec badge_corners() -> aihtml:html().
badge_corners() ->
    row([badge(avatar(<<"TR">>, [square], []), [], [{count, 1}]),
         badge(avatar(<<"TL">>, [square], []), [secondary, left], [{count, 2}]),
         badge(avatar(<<"BR">>, [square], []), [success, bottom], [{count, 3}]),
         badge(avatar(<<"BL">>, [square], []), [info, bottom, left], [{count, <<"new">>}])]).

-spec badge_standalone() -> aihtml:html().
badge_standalone() ->
    row([span([<<"Inbox ">>, badge(undefined, [], [{count, 42}])]),
         badge(undefined, [success], [{count, <<"beta">>}]),
         badge(undefined, [error], [{count, 1000}])]).

%%% Chip

-spec chip_variants() -> aihtml:html().
chip_variants() ->
    Colors = [default, primary, success, warning, error, info],
    'div'([row([chip(atom_to_binary(C), [Variant, C], []) || C <- Colors])
           || Variant <- [filled, outlined, soft]],
          [<<"flex flex-col gap-3">>], []).

-spec chip_features() -> aihtml:html().
chip_features() ->
    row([chip(<<"Small">>, [small, primary], []),
         chip(<<"Jane Doe">>, [soft, primary], [{avatar, <<"JD">>}]),
         chip(<<"Erlang">>, [removable, outlined, info], [{value, erlang}]),
         chip(<<"Clickable">>, [clickable, soft, success], []),
         chip(<<"Disabled">>, [disabled, primary], [])]).

%%% AspectRatio

-spec aspect_ratios() -> aihtml:html().
aspect_ratios() ->
    row(['div'(aspect_ratio('div'(R, [<<"h-full flex items-center justify-center "
                                         "bg-primary/15 text-primary">>], []),
                            [], [{ratio, R}]),
               [<<"w-48">>], [])
         || R <- [<<"16/9">>, <<"4:3">>, <<"1/1">>]]).

-spec aspect_ratio_image() -> aihtml:html().
aspect_ratio_image() ->
    Sky = <<"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 4 3'>"
            "<rect width='4' height='3' fill='%2393c5fd'/><circle cx='3' cy='1' r='.5' fill='%23fde047'/>"
            "<path d='M0 3 1.5 1.5 3 3z' fill='%2322c55e'/></svg>">>,
    'div'(aspect_ratio(img([], [{src, Sky}, {alt, <<"Landscape">>}]), [], [{ratio, {21, 9}}]),
          [<<"max-w-md">>], []).

%%% Kbd

-spec kbd_keys() -> aihtml:html().
kbd_keys() ->
    row([kbd(<<"Esc">>, [], []),
         kbd(<<"⌘"/utf8>>, [], []),
         kbd(<<"Tab">>, [], []),
         kbd(<<"Enter">>, [lg], [])]).

-spec kbd_combos() -> aihtml:html().
kbd_combos() ->
    row([kbd([<<"Ctrl">>, <<"Shift">>, <<"P">>], [], []),
         kbd([<<"⌘"/utf8>>, <<"K">>], [lg], []),
         span([<<"Press ">>, kbd([<<"Ctrl">>, <<"C">>], [], []), <<" to copy">>])]).

%%% TimeAgo

-spec time_ago_units() -> aihtml:html().
time_ago_units() ->
    Now = erlang:system_time(second),
    row([time_ago(Now - Ago, [], [])
         || Ago <- [5, 3 * 60, 2 * 3600, 3 * 86400, 90 * 86400]]).

-spec time_ago_options() -> aihtml:html().
time_ago_options() ->
    Now = erlang:system_time(second),
    Chinese = #{just_now => <<"刚刚"/utf8>>, minutes => <<"{n} 分钟前"/utf8>>,
                hours => <<"{n} 小时前"/utf8>>, days => <<"{n} 天前"/utf8>>,
                months => <<"{n} 个月前"/utf8>>},
    row([time_ago(Now - 600, [], [{labels, Chinese}]),
         time_ago({{2026, 1, 1}, {0, 0, 0}}, [<<"text-muted">>], [{live, false}]),
         time_ago(<<"2026-07-22T08:00:00Z">>, [], [{title, false}])]).

%%% ExpandableText

-spec expandable_default() -> aihtml:html().
expandable_default() ->
    expandable_text(<<"aihtml renders every page on the server as Erlang function calls; "
                      "jQuery only adds behaviour. This paragraph is long enough to be cut "
                      "at the threshold and shows a toggle to read the rest.">>,
                    [], [{threshold, 60}]).

-spec expandable_labels() -> aihtml:html().
expandable_labels() ->
    'div'([expandable_text(<<"Release notes: morph swaps keep focus, shared Mustache "
                             "templates render the same bytes on both ends, and popups "
                             "follow their anchor.">>,
                           [], [{threshold, 40}, {expand_label, <<"Show more">>},
                                {collapse_label, <<"Show less">>}]),
           expandable_text(<<"This one starts expanded, so the whole text is visible "
                             "and the toggle folds it.">>,
                           [], [{threshold, 30}, {expanded, true}]),
           expandable_text(<<"Short text is shown as is.">>, [], [])],
          [<<"flex flex-col gap-3">>], []).

%%% Alert

-spec alert_variants() -> aihtml:html().
alert_variants() ->
    stack([alert(<<"A new version is available.">>, [], []),
           alert(<<"Your changes were saved.">>, [success], []),
           alert(<<"Your trial ends in 3 days.">>, [warning], []),
           alert(<<"Could not reach the server.">>, [error], []),
           alert(<<"No icon, plain message.">>, [], [{icon, false}])]).

-spec alert_dismissible() -> aihtml:html().
alert_dismissible() ->
    stack([alert(<<"Your changes were saved.">>, [success, dismissible], [{title, <<"Saved">>}]),
           alert([<<"Could not reach the server. ">>, a(<<"Retry">>, [<<"underline">>], [{href, <<"#">>}])],
                 [error, dismissible], [{title, <<"Connection failed">>}])]).

%%% Progressbar

-spec progress_values() -> aihtml:html().
progress_values() ->
    stack([progressbar(35, [show_text], []),
           progressbar(9, [show_text], [{max, 10}, {text, <<"9 of 10 files">>}]),
           progressbar(60, [], [{aria_label, <<"Upload">>}])]).

-spec progress_styles() -> aihtml:html().
progress_styles() ->
    stack([progressbar(70, [success, striped, animated, show_text], []),
           progressbar(45, [warning, striped], []),
           progressbar(undefined, [indeterminate, info], [{aria_label, <<"Loading">>}]),
           progressbar(50, [disabled, show_text], [])]).

-spec progress_ranges() -> aihtml:html().
progress_ranges() ->
    progressbar(80, [show_text], [{color_ranges, [{30, success}, {60, warning}, {100, error}]}]).

-spec progress_vertical() -> aihtml:html().
progress_vertical() ->
    row([progressbar(30, [vertical, show_text], [{style, <<"height: 120px">>}]),
         progressbar(60, [vertical, reverse, success, show_text], [{style, <<"height: 120px">>}]),
         'div'(progressbar(40, [reverse, error], []), [<<"flex-1">>], [])]).

%%% ProgressCircle

-spec circle_sizes() -> aihtml:html().
circle_sizes() ->
    row([progress_circle(25, [sm], []),
         progress_circle(50, [], []),
         progress_circle(75, [lg], [])]).

-spec circle_colors() -> aihtml:html().
circle_colors() ->
    row([progress_circle(60, [Color], []) || Color <- [primary, success, warning, info, error]]).

-spec circle_states() -> aihtml:html().
circle_states() ->
    row([progress_circle(75, [lg, success], [{label, <<"Uploaded">>}]),
         progress_circle(60, [info], [{show_value, false}, {label, <<"No value">>}]),
         progress_circle(undefined, [indeterminate], [{label, <<"Working">>}]),
         progress_circle(30, [disabled], [])]).

%%% Meter

-spec meter_states() -> aihtml:html().
meter_states() ->
    Zones = [{low, 25}, {high, 75}, {show_value, true}],
    stack([meter(15, [], [{label, <<"Low">>} | Zones]),
           meter(62, [], [{label, <<"Normal">>} | Zones]),
           meter(88, [], [{label, <<"High">>} | Zones])]).

-spec meter_sizes() -> aihtml:html().
meter_sizes() ->
    stack([meter(40, [sm], [{label, <<"Disk">>}]),
           meter(15, [], [{low, 25}, {high, 75}, {optimum, 90}, {label, <<"Battery">>},
                          {show_value, true}, {helper_text, <<"Low: charge soon">>}]),
           meter(700, [lg], [{max, 1000}, {label, <<"Memory (MB)">>}, {show_value, true}])]).

%%% Statistic

-spec statistic_basic() -> aihtml:html().
statistic_basic() ->
    row([statistic(1284500, [primary], [{title, <<"Revenue">>}, {prefix, <<"¥"/utf8>>}]),
         statistic(98.456, [success], [{title, <<"Uptime">>}, {suffix, <<"%">>}, {precision, 2}]),
         statistic(1284500, [], [{title, <<"No separators">>}, {group_separator, false}])]).

-spec statistic_delta() -> aihtml:html().
statistic_delta() ->
    row([statistic(3210, [], [{title, <<"Orders">>}, {delta, 128}]),
         statistic(-3250.5, [error], [{title, <<"Balance">>}, {precision, 1}, {delta, -120}]),
         statistic(42, [], [{title, <<"Tickets">>}, {delta, 0}]),
         statistic(0, [loading], [{title, <<"Loading">>}])]).

%%% KpiCard

-spec kpi_trends() -> aihtml:html().
kpi_trends() ->
    grid([kpi_card(<<"12,480">>, [], [{title, <<"Active users">>}, {trend, 5.2},
                                      {trend_label, <<"vs last month">>}, {icon, users}]),
          kpi_card(<<"845">>, [warning], [{title, <<"Installs">>}, {trend, -3.5},
                                          {trend_label, <<"vs last week">>}, {icon, install}]),
          kpi_card(<<"4.8">>, [], [{title, <<"Rating">>}, {icon, star}])]).

-spec kpi_colors() -> aihtml:html().
kpi_colors() ->
    grid([kpi_card(<<"3,210">>, [success], [{title, <<"Downloads">>}, {trend, 12}, {icon, download}]),
          kpi_card(<<"96%">>, [info], [{title, <<"SLA">>}, {trend, 0.4}]),
          kpi_card(<<"7">>, [error], [{title, <<"Incidents">>}, {trend, -30}]),
          kpi_card(<<"—"/utf8>>, [disabled], [{title, <<"Disabled">>}])]).

-spec kpi_record() -> aihtml:html().
kpi_record() ->
    grid([#ah_kpi_card{value = <<"1,024">>, color = success, icon = users,
                       title = <<"Signups">>, trend = 8.5, trend_label = <<"vs last week">>},
          #ah_kpi_card{value = <<"37">>, color = error, css = [<<"shadow-sm">>],
                       title = <<"Churned">>, trend = -2.1}]).

%%% Timeline

-spec timeline_both() -> aihtml:html().
timeline_both() ->
    timeline([#{date => <<"2026-01">>, title => <<"Project start">>, subtitle => <<"Kick-off">>,
                description => <<"Scope agreed, team formed.">>},
              #{date => <<"2026-03">>, title => <<"Alpha">>, dot => success, expanded => true,
                description => <<"First internal release.">>},
              #{date => <<"2026-06">>, title => <<"Beta">>, dot => warning},
              #{date => <<"2026-09">>, title => <<"Launch">>, subtitle => <<"GA">>, dot => danger}],
             [], []).

-spec timeline_near() -> aihtml:html().
timeline_near() ->
    timeline([#{date => <<"09:12">>, title => <<"Order placed">>},
              #{date => <<"09:40">>, title => <<"Paid">>, dot => success},
              #{date => <<"14:05">>, title => <<"Shipped">>,
                description => <<"Parcel 4711 handed to the carrier.">>}],
             [near], []).

-spec timeline_horizontal() -> aihtml:html().
timeline_horizontal() ->
    timeline([#{date => <<"Q1">>, title => <<"Design">>},
              #{date => <<"Q2">>, title => <<"Build">>, dot => success},
              #{date => <<"Q3">>, title => <<"Test">>, dot => warning},
              #{date => <<"Q4">>, title => <<"Ship">>, dot => danger}],
             [horizontal], [{collapsible, false}]).

%%% RankingList

-spec ranking_basic() -> aihtml:html().
ranking_basic() ->
    ranking_list([#{name => <<"Germany">>, code => de, value => <<"12,300">>,
                    sub_value => <<"+4%">>, tag => <<"Free">>},
                  #{name => <<"United States">>, code => us, value => <<"9,870">>, tag => <<"Paid">>},
                  #{name => <<"Japan">>, code => jp, value => <<"7,450">>, secondary => <<"Asia">>,
                    tag => <<"Progress">>},
                  #{name => <<"Brazil">>, code => br, value => <<"3,120">>, tag => <<"Out of date">>}],
                 [<<"max-w-lg">>], [{title, <<"Top countries">>}]).

-spec ranking_dense() -> aihtml:html().
ranking_dense() ->
    ranking_list([#{name => <<"Erlang">>, value => 98, tag => <<"BEAM">>},
                  #{name => <<"Elixir">>, value => 95, tag => <<"BEAM">>},
                  #{name => <<"Gleam">>, value => 90, tag => <<"New">>},
                  #{name => <<"LFE">>, value => 70}],
                 [dense, clickable, <<"max-w-lg">>],
                 [{title, <<"Top 3">>}, {max_items, 3},
                  {tag_colors, #{<<"BEAM">> => success, <<"New">> => warning}}]).

%%% TagCloud

-spec tag_cloud_weights() -> aihtml:html().
tag_cloud_weights() ->
    tag_cloud([{<<"Erlang">>, 40}, {<<"jQuery">>, 25}, {<<"CSS">>, 15}, {<<"sigil">>, 30},
               #{label => <<"OTP">>, value => 35, url => <<"#otp">>},
               {<<"html">>, 8}, {<<"Tailwind">>, 20}],
              [], []).

-spec tag_cloud_gradient() -> aihtml:html().
tag_cloud_gradient() ->
    tag_cloud([{<<"Erlang">>, 40}, {<<"jQuery">>, 25}, {<<"CSS">>, 15}, {<<"sigil">>, 30},
               {<<"OTP">>, 35}, {<<"html">>, 8}, {<<"Tailwind">>, 20}],
              [], [{min_color, <<"#93c5fd">>}, {max_color, <<"#1e3a8a">>}, {max_font_size, 32},
                   {sort_by, value}, {sort_order, descending}]).

-spec tag_cloud_values() -> aihtml:html().
tag_cloud_values() ->
    tag_cloud([{<<"erlang">>, 40}, {<<"jquery">>, 25}, {<<"css">>, 15}, {<<"sigil">>, 30},
               {<<"otp">>, 35}, {<<"html">>, 8}],
              [], [{display_value, true}, {text_case, first_upper},
                   {display_limit, 4}, {take_top_weighted, true}]).

%% Layout helpers shared by the demos.
row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-4">>], []).

stack(Children) ->
    'div'(Children, [<<"flex flex-col gap-3">>], []).

grid(Children) ->
    'div'(Children, [<<"grid grid-cols-1 sm:grid-cols-3 gap-4">>], []).
