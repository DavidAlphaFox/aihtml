%% @doc GET /examples: real pages composed from several components. The
%% dashboard is modelled on sigil's dashboard demo, while staying a plain
%% aihtml page: components, Tailwind utilities and no client-side app shell.
-module(aihtml_example_examples).

-include_lib("aihtml/include/aihtml.hrl").

-export([init/2, render/1]).

-define(SITE, aihtml_example_site).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req0, State) ->
    Page = case cowboy_req:binding(page, Req0) of
               undefined -> <<"index">>;
               Name -> Name
           end,
    case valid(Page) of
        true ->
            Title = case Page of
                        <<"dashboard">> -> <<"Dashboard — aihtml examples"/utf8>>;
                        _ -> <<"aihtml examples"/utf8>>
                    end,
            {ok, ?SITE:reply(Req0, Title, fun() -> render(Page) end, #{}), State};
        false ->
            {ok, cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>},
                                  <<"no such example">>, Req0), State}
    end.

-spec render(binary()) -> aihtml:html().
render(<<"dashboard">>) -> dashboard();
render(<<"index">>) -> index().

valid(<<"dashboard">>) -> true;
valid(<<"index">>) -> true;
valid(_) -> false.

%%%===================================================================
%%% Index
%%%===================================================================

index() ->
    ah_main([aihtml_example_site:topbar(examples),
             ah_div([ah_h1(<<"Examples"/utf8>>, [<<"text-4xl font-bold">>], []),
                     ah_p(<<"不止是组件沙盒：这些页面按真实业务布局组合组件。"/utf8>>,
                          [<<"mt-3 text-lg text-muted">>], []),
                     ah_div([example(<<"Dashboard"/utf8>>, <<"/examples/dashboard">>,
                                     <<"仪表盘"/utf8>>,
	                                    <<"侧边导航、指标拖拽、目标进度、图表、应用排行、消息、任务、"
	                                      "订单、转化与区域汇总。"/utf8>>)],
                            [<<"mt-8 grid gap-6 md:grid-cols-2 xl:grid-cols-3">>], [])],
                    [<<"max-w-6xl mx-auto px-6 py-16">>], [])],
            [<<"min-h-screen">>], []).

example(Title, Href, Chip, Desc) ->
    ah_a([ah_div([ah_chip(Chip, [primary, soft, small], [])],
                 [<<"flex items-center justify-between">>], []),
          ah_h2(Title, [<<"mt-4 text-xl font-semibold">>], []),
          ah_p(Desc, [<<"mt-2 text-sm text-muted leading-relaxed">>], []),
          ah_span(<<"Open →"/utf8>>, [<<"mt-4 inline-block text-sm font-semibold text-primary">>], [])],
           [<<"example-card block p-6 rounded-surface border border-line bg-surface hover">>],
           [{href, Href}]).

%%%===================================================================
%%% Dashboard
%%%===================================================================

dashboard() ->
    ah_div([side_nav(),
            ah_main([top_bar(), page_head(),
                     ah_div([metrics(),
                             hero(),
                             goals(),
                             chart_row(),
                             related_row(),
                             content_row(),
                             order_row(),
                             invoice_row(),
                             product_row(),
                             conversion_row(),
                             summary_row()],
                            [<<"px-4 pb-10 xl:px-8 flex flex-col gap-6">>], [])],
                    [<<"flex-1 min-w-0 min-h-screen overflow-x-hidden bg-surface-2">>], [])],
           [<<"flex min-h-screen bg-surface-2">>], []).

side_nav() ->
    Groups = [#{label => <<"Workspace"/utf8>>,
                items => [#{key => dashboard, label => <<"Dashboard"/utf8>>,
                            href => <<"/examples/dashboard">>},
                          #{key => analytics, label => <<"Analytics"/utf8>>, href => <<"#">>},
                          #{key => reports, label => <<"Reports"/utf8>>, href => <<"#">>}]},
              #{label => <<"Operations"/utf8>>,
                items => [#{key => orders, label => <<"Orders"/utf8>>, href => <<"#">>},
                          #{key => customers, label => <<"Customers"/utf8>>, href => <<"#">>},
                          #{key => integrations, label => <<"Integrations"/utf8>>, href => <<"#">>,
                            children => [#{key => billing, label => <<"Billing"/utf8>>, href => <<"#">>},
                                         #{key => webhooks, label => <<"Webhooks"/utf8>>, href => <<"#">>}]}]},
              #{label => <<"System"/utf8>>,
                items => [#{key => team, label => <<"Team"/utf8>>, href => <<"#">>},
                          #{key => settings, label => <<"Settings"/utf8>>, href => <<"#">>}]}],
    ah_sidenav(Groups, dashboard, [<<"dashboard-nav shrink-0">>],
               [{brand, #{name => <<"aihtml"/utf8>>, logo => ah_strong(<<"A">>),
                          href => <<"/examples">>}},
                {footer, ah_div([ah_p(<<"Pro plan"/utf8>>, [<<"text-sm font-semibold">>], []),
                                 ah_p(<<"12 of 20 seats used"/utf8>>,
                                      [<<"mt-1 text-xs text-muted">>], []),
                                 ah_div([ah_button(<<"Manage"/utf8>>, manage_plan, [sm, outlined],
                                                   [{type, button}])],
                                        [<<"mt-3">>], [])],
                                [], [])},
                {collapsible, true},
                {style, <<"--ah-ssn-width:16rem; --ah-ssn-height:100vh">>}])
    .

top_bar() ->
    ah_header(ah_div([ah_breadcrumbs([<<"Home"/utf8>>, <<"Examples"/utf8>>, <<"Dashboard"/utf8>>],
                                     [], []),
                     ah_div([ah_button(<<"New report"/utf8>>, new_report, [primary, sm],
                                       [{type, button}]),
                             ah_button(<<"Export"/utf8>>, export, [outlined, sm],
                                       [{type, button}]),
                             ah_avatar(<<"DA"/utf8>>, [md], [{title, <<"David"/utf8>>}])],
                            [<<"flex items-center gap-3">>], [])],
                    [<<"flex min-h-20 flex-wrap items-center justify-between gap-4 px-4 py-4 xl:px-8">>],
                    []),
            [<<"sticky top-0 z-20 border-b border-line bg-surface/85 backdrop-blur">>], []).

page_head() ->
    ah_div([ah_div([ah_h1(<<"Welcome back, David"/utf8>>,
                          [<<"text-3xl font-bold tracking-tight">>], []),
                    ah_p(<<"Revenue is up 12.4% this week. Three campaigns need review before Friday."/utf8>>,
                          [<<"mt-2 text-sm text-muted">>], [])],
                   [<<"min-w-0">>], []),
            ah_div([ah_dropdownlist([{last_7, <<"Last 7 days"/utf8>>},
                                     {last_30, <<"Last 30 days"/utf8>>},
                                     {quarter, <<"This quarter"/utf8>>}], last_7,
	                                    [], [{style, <<"width: 10rem">>}])],
                   [<<"shrink-0">>], [])],
           [<<"flex flex-wrap items-end justify-between gap-4 pt-6 xl:px-0">>], []).

metrics() ->
    Cards = [#ah_kpi_card{value = <<"18,765"/utf8>>, color = success, title = <<"Active users"/utf8>>,
                           trend = 2.6, trend_label = <<"vs last week"/utf8>>, icon = users},
             #ah_kpi_card{value = <<"4,876"/utf8>>, color = info, title = <<"Installed"/utf8>>,
                           trend = 0.2, trend_label = <<"vs last week"/utf8>>, icon = install},
             #ah_kpi_card{value = <<"678"/utf8>>, color = error, title = <<"Failed jobs"/utf8>>,
                           trend = -0.1, trend_label = <<"vs last week"/utf8>>, icon = download},
             #ah_kpi_card{value = <<"$48.2k"/utf8>>, color = warning, title = <<"Revenue"/utf8>>,
                           trend = 3.6, trend_label = <<"vs last week"/utf8>>, icon = trending_up}],
    Items = [{metric_key(I), Card} || {I, Card} <- lists:enumerate(1, Cards)],
    ah_sortable(Items, undefined,
                [<<"grid grid-cols-1 gap-6 sm:grid-cols-2 xl:grid-cols-4">>, handle],
                [{aria_label, <<"Reorder metrics"/utf8>>}]).

metric_key(1) -> active_users;
metric_key(2) -> installed;
metric_key(3) -> failed_jobs;
metric_key(4) -> revenue.

%%%-------------------------------------------------------------------
%%% A compact welcome hero and quarterly goals, following the sigil
%%% dashboard's rhythm without a second bespoke design system.
%%%-------------------------------------------------------------------

hero() ->
    ah_div([ah_div([ah_p(<<"WELCOME BACK"/utf8>>,
                          [<<"text-xs font-bold tracking-widest text-primary">>], []),
                    ah_h2(<<"Ship faster with aihtml"/utf8>>,
                          [<<"mt-2 text-2xl font-bold tracking-tight xl:text-3xl">>], []),
                    ah_p(<<"Your team closed 32 issues this week — 18% above average. "
                           "Campaign reviews and billing migration are the next milestones."/utf8>>,
                          [<<"mt-3 max-w-2xl text-sm leading-relaxed text-muted">>], []),
                    ah_div([ah_button(<<"Explore tasks"/utf8>>, explore_tasks,
                                      [primary, sm], [{type, button}]),
                            ah_button(<<"View report"/utf8>>, view_report,
                                      [outlined, sm], [{type, button}])],
                           [<<"mt-5 flex flex-wrap items-center gap-3">>], [])],
                   [<<"min-w-0 flex-1">>], []),
            ah_div([mini_metric(<<"Deployment success"/utf8>>, <<"98.2%"/utf8>>, success, 98),
                    mini_metric(<<"Release velocity"/utf8>>, <<"32 / week"/utf8>>, info, 74),
                    mini_metric(<<"Open risks"/utf8>>, <<"3"/utf8>>, warning, 24)],
                   [<<"grid w-full max-w-md shrink-0 gap-3 sm:grid-cols-3">>], [])],
           [<<"relative overflow-hidden rounded-surface border border-line bg-surface p-6 xl:p-7">>],
           [{style, <<"background: radial-gradient(640px 220px at 88% -10%, "
                      "color-mix(in srgb, var(--ah-color-primary) 20%, transparent), transparent)">>}]).

mini_metric(Label, Value, Color, Pct) ->
    ah_div([ah_p(Label, [<<"truncate text-xs font-semibold text-muted">>], []),
            ah_p(Value, [<<"mt-1 text-lg font-bold">>, color_class(Color)], []),
            ah_progressbar(Pct, [Color], [{aria_label, Label}])],
           [<<"rounded-surface border border-line bg-surface-2 p-3">>], []).

goals() ->
    Goals = [{<<"Quarterly revenue"/utf8>>, <<"$184.3k"/utf8>>, <<"of $250.0k target"/utf8>>,
              <<"+6.4%"/utf8>>, 74, success},
             {<<"Active subscribers"/utf8>>, <<"4.1k"/utf8>>, <<"of 5.0k target"/utf8>>,
              <<"+3.2%"/utf8>>, 82, info},
             {<<"Support SLA"/utf8>>, <<"94%"/utf8>>, <<"30-day rolling average"/utf8>>,
              <<"-1.1%"/utf8>>, 94, warning}],
    ah_card([ah_div([goal(G) || G <- Goals], [<<"grid gap-5 md:grid-cols-3">>], [])],
            [], [{title, <<"Goal progress"/utf8>>}, {subtitle, <<"Track quarterly KPIs"/utf8>>}]).

goal({Label, Value, Sub, Delta, Pct, Color}) ->
    ah_div([ah_div([ah_span(Label, [<<"text-sm font-medium">>], []),
                    ah_chip(Delta, [Color, soft, small], [])],
                   [<<"flex items-center justify-between gap-2">>], []),
            ah_div([ah_strong(Value, [<<"text-2xl font-bold tracking-tight">>], [])],
                   [<<"mt-3">>], []),
            ah_p(Sub, [<<"mt-1 text-xs text-muted">>], []),
            ah_progressbar(Pct, [Color], [{aria_label, Label}])],
           [<<"rounded-surface border border-line p-4">>], []).

chart_row() ->
    ah_div([ah_card([revenue_chart()],
                    [<<"xl:col-span-2">>],
                    [{title, <<"Revenue trend"/utf8>>}, {subtitle, <<"Paid and free · last 7 days"/utf8>>}]),
            ah_card([#ah_donut_chart{items = [{<<"Mac"/utf8>>, 38566},
                                              {<<"Windows"/utf8>>, 55566},
                                              {<<"iOS"/utf8>>, 47057},
                                              {<<"Android"/utf8>>, 47056}],
                                     legend = bottom, labels = false, height = 300}],
                    [], [{title, <<"Current download"/utf8>>}, {subtitle, <<"By operating system"/utf8>>}]),
            ah_card([ah_bar_chart([{<<"Team A"/utf8>>, [43, 33, 22, 37, 67, 68, 37, 24, 55]},
                                   {<<"Team B"/utf8>>, [51, 70, 47, 67, 40, 37, 24, 70, 24]}],
                                  [], [{categories, [<<"Jan"/utf8>>, <<"Feb"/utf8>>, <<"Mar"/utf8>>,
                                                     <<"Apr"/utf8>>, <<"May"/utf8>>, <<"Jun"/utf8>>,
                                                     <<"Jul"/utf8>>, <<"Aug"/utf8>>, <<"Sep"/utf8>>]},
                                       {height, 300}])],
                    [<<"xl:col-span-2">>],
                    [{title, <<"Website visits"/utf8>>}, {subtitle, <<"(+43%) than last year"/utf8>>}])],
           [<<"grid grid-cols-1 gap-6 xl:grid-cols-3">>], []).

revenue_chart() ->
    AxisColor = <<"--ah-color-text-muted">>,
    SplitColor = <<"--ah-color-border-subtle">>,
    ah_chart(#{tooltip => #{trigger => axis},
               legend => #{bottom => 0, icon => circle},
               grid => #{left => 8, right => 16, top => 16, bottom => 36, containLabel => true},
               xAxis => #{type => category, boundaryGap => false,
                          data => [<<"Mon"/utf8>>, <<"Tue"/utf8>>, <<"Wed"/utf8>>, <<"Thu"/utf8>>,
                                   <<"Fri"/utf8>>, <<"Sat"/utf8>>, <<"Sun"/utf8>>],
                          axisLine => #{show => false}, axisTick => #{show => false},
                          axisLabel => #{color => AxisColor}},
               yAxis => #{type => value, splitLine => #{lineStyle => #{color => SplitColor}},
                          axisLabel => #{color => AxisColor}},
               series => [#{name => <<"Paid"/utf8>>, type => line, smooth => true, symbol => none,
                            data => [3200, 3800, 3500, 4200, 4800, 5100, 4900],
                            itemStyle => #{color => <<"--ah-color-success">>},
                            areaStyle => #{opacity => 0.18}},
                          #{name => <<"Free"/utf8>>, type => line, smooth => true, symbol => none,
                            data => [1200, 1500, 1800, 1600, 2100, 2400, 2200],
                            itemStyle => #{color => <<"--ah-color-info">>},
                            areaStyle => #{opacity => 0.12}}]},
             [], [{height, 300}, {aria_label, <<"Revenue trend chart"/utf8>>}]).

order_row() ->
    ah_div([ah_card([#ah_timeline{
                       items = [#{date => <<"Mar 15"/utf8>>, title => <<"Order #1248"/utf8>>,
                                  subtitle => <<"$1,250.00 · Completed"/utf8>>, dot => success},
                                #{date => <<"Mar 15"/utf8>>, title => <<"Order #1247"/utf8>>,
                                  subtitle => <<"$748.20 · Processing"/utf8>>, dot => primary},
                                #{date => <<"Mar 14"/utf8>>, title => <<"Order #1246"/utf8>>,
                                  subtitle => <<"$2,980.00 · Pending"/utf8>>, dot => warning},
                                #{date => <<"Mar 14"/utf8>>, title => <<"Order #1245"/utf8>>,
                                  subtitle => <<"$316.50 · Shipped"/utf8>>, dot => primary},
                                #{date => <<"Mar 13"/utf8>>, title => <<"Order #1244"/utf8>>,
                                  subtitle => <<"$890.75 · Delivered"/utf8>>, dot => success}],
                       position = near, collapsible = false}],
                    [<<"xl:col-span-2">>],
                    [{title, <<"Order timeline"/utf8>>}, {subtitle, <<"Last 5 orders"/utf8>>}]),
            ah_card([task_list()], [], [{title, <<"Tasks"/utf8>>},
                                        {subtitle, <<"1 of 5 completed"/utf8>>}])],
           [<<"grid grid-cols-1 gap-6 xl:grid-cols-3">>], []).

task_list() ->
    Tasks = [{<<"Prepare monthly financial report"/utf8>>, true},
             {<<"Design Q3 marketing campaign"/utf8>>, false},
             {<<"Analyze customer feedback survey"/utf8>>, false},
             {<<"Update website hero copy"/utf8>>, false},
             {<<"Conduct competitor market research"/utf8>>, false}],
    ah_div([ah_checkbox(Label, integer_to_list(I), [<<"flex-1">>,
                                                     [<<"line-through text-muted">> || Done]],
                         [{checked, Done}])
            || {I, {Label, Done}} <- lists:enumerate(1, Tasks)],
           [<<"flex flex-col gap-3">>], []).

%%%-------------------------------------------------------------------
%%% Related applications, countries, news, inbox, product and traffic
%%% rankings, conversion quality, regional sales and summary gauges.
%%%-------------------------------------------------------------------

related_row() ->
    ah_div([ah_card([related_apps()],
                    [<<"xl:col-span-2">>],
                    [{title, <<"Related applications"/utf8>>},
                     {subtitle, <<"Most active this period"/utf8>>}]),
            ah_card([country_rows()], [],
                    [{title, <<"Top installed countries"/utf8>>},
                     {subtitle, <<"Installs by platform"/utf8>>}])],
           [<<"grid grid-cols-1 gap-6 xl:grid-cols-3">>], []).

related_apps() ->
    Row = fun({Name, Letter, Price, Color, Downloads, Size}) ->
                  ah_div([#ah_avatar{body = Letter, size = sm, shape = rounded, color = Color},
                          ah_div([ah_div([ah_p(Name, [<<"truncate text-sm font-semibold">>], []),
                                          ah_span(Price, [<<"text-xs font-bold">>, color_class(Color)], [])],
                                         [<<"flex items-center justify-between gap-3">>], []),
                                  ah_p([Downloads, <<" downloads · "/utf8>>, Size],
                                       [<<"mt-1 text-xs text-muted">>], [])],
                                 [<<"min-w-0 flex-1">>], []),
                          ah_button(<<"···"/utf8>>, undefined, [borderless, sm, <<"px-2">>],
                                    [{type, button}, {aria_label, <<"More"/utf8>>}])],
                         [<<"flex items-center gap-3 rounded-surface px-2 py-3 hover:bg-surface-2">>], [])
          end,
    Periods = [{seven, <<"Top 7 days"/utf8>>,
                [{<<"Microsoft 365"/utf8>>, <<"M"/utf8>>, <<"Free"/utf8>>, info, <<"9.91k"/utf8>>, <<"9.68 MB"/utf8>>},
                 {<<"Opera"/utf8>>, <<"O"/utf8>>, <<"Free"/utf8>>, error, <<"1.95k"/utf8>>, <<"1.9 MB"/utf8>>},
                 {<<"Acrobat Reader"/utf8>>, <<"A"/utf8>>, <<"$68.71"/utf8>>, warning, <<"9.12k"/utf8>>, <<"8.91 MB"/utf8>>},
                 {<<"Joplin"/utf8>>, <<"J"/utf8>>, <<"Free"/utf8>>, success, <<"6.98k"/utf8>>, <<"6.82 MB"/utf8>>}]},
               {thirty, <<"Top 30 days"/utf8>>,
                [{<<"Slack"/utf8>>, <<"S"/utf8>>, <<"Free"/utf8>>, warning, <<"12.4k"/utf8>>, <<"112 MB"/utf8>>},
                 {<<"Notion"/utf8>>, <<"N"/utf8>>, <<"Free"/utf8>>, primary, <<"10.8k"/utf8>>, <<"189 MB"/utf8>>},
                 {<<"Figma"/utf8>>, <<"F"/utf8>>, <<"Free"/utf8>>, success, <<"9.50k"/utf8>>, <<"98 MB"/utf8>>},
                 {<<"Linear"/utf8>>, <<"L"/utf8>>, <<"Free"/utf8>>, info, <<"4.12k"/utf8>>, <<"55 MB"/utf8>>}]},
               {all, <<"All time"/utf8>>,
                [{<<"VS Code"/utf8>>, <<"V"/utf8>>, <<"Free"/utf8>>, info, <<"184k"/utf8>>, <<"85 MB"/utf8>>},
                 {<<"Chrome"/utf8>>, <<"C"/utf8>>, <<"Free"/utf8>>, warning, <<"176k"/utf8>>, <<"120 MB"/utf8>>},
                 {<<"Spotify"/utf8>>, <<"S"/utf8>>, <<"Free"/utf8>>, success, <<"152k"/utf8>>, <<"256 MB"/utf8>>},
                 {<<"Photoshop"/utf8>>, <<"P"/utf8>>, <<"$239.88"/utf8>>, primary, <<"96k"/utf8>>, <<"1.8 GB"/utf8>>}]}],
    ah_tabs([{K, Label, ah_div([Row(A) || A <- Items], [<<"flex flex-col">>], [])}
             || {K, Label, Items} <- Periods], seven, [], [{animation, none}]).

country_rows() ->
    Countries = [{<<"🇩🇪"/utf8>>, <<"Germany"/utf8>>, <<"9.91k"/utf8>>, <<"1.95k"/utf8>>, <<"9.12k"/utf8>>},
                 {<<"🇬🇧"/utf8>>, <<"England"/utf8>>, <<"1.95k"/utf8>>, <<"9.12k"/utf8>>, <<"6.98k"/utf8>>},
                 {<<"🇫🇷"/utf8>>, <<"France"/utf8>>, <<"9.12k"/utf8>>, <<"6.98k"/utf8>>, <<"8.49k"/utf8>>},
                 {<<"🇰🇷"/utf8>>, <<"Korea"/utf8>>, <<"6.98k"/utf8>>, <<"8.49k"/utf8>>, <<"2.03k"/utf8>>},
                 {<<"🇺🇸"/utf8>>, <<"United States"/utf8>>, <<"8.49k"/utf8>>, <<"2.03k"/utf8>>, <<"3.36k"/utf8>>}],
    ah_div([ah_div([ah_span(Flag, [<<"text-lg leading-none">>], []),
                    ah_span(Name, [<<"min-w-0 flex-1 truncate text-sm font-medium">>], []),
                    ah_span([Android, <<" · "/utf8>>, Windows, <<" · "/utf8>>, Apple],
                            [<<"shrink-0 text-xs text-muted">>], [])],
                   [<<"flex items-center gap-3 border-b border-line py-3 last:border-0">>], [])
            || {Flag, Name, Android, Windows, Apple} <- Countries],
           [<<"flex flex-col">>], []).

content_row() ->
    News = [{<<"Release"/utf8>>, <<"Real-time analytics dashboard now live"/utf8>>, <<"2 h"/utf8>>, success, true},
            {<<"Report"/utf8>>, <<"April performance summary is ready"/utf8>>, <<"5 h"/utf8>>, info, true},
            {<<"Maintenance"/utf8>>, <<"Scheduled database upgrade Saturday"/utf8>>, <<"1 d"/utf8>>, warning, false},
            {<<"API"/utf8>>, <<"API v2 doubles webhook and rate limits"/utf8>>, <<"2 d"/utf8>>, primary, false}],
    Inbox = [{<<"LQ"/utf8>>, <<"Lara Quinn"/utf8>>, <<"Re: Q3 launch checklist"/utf8>>,
              <<"Thanks — let's lock the dates by Friday."/utf8>>, <<"12 min"/utf8>>, success},
             {<<"MD"/utf8>>, <<"Marco Diaz"/utf8>>, <<"Design review for billing"/utf8>>,
              <<"Pushed v3 of the layout for review."/utf8>>, <<"1 h"/utf8>>, info},
             {<<"SO"/utf8>>, <<"Sienna Ohira"/utf8>>, <<"Customer feedback summary"/utf8>>,
              <<"Dark mode is the most requested change."/utf8>>, <<"3 h"/utf8>>, primary},
             {<<"RS"/utf8>>, <<"Roman Schultz"/utf8>>, <<"Database migration window"/utf8>>,
              <<"Ops scheduled it for Saturday at 02:00."/utf8>>, <<"Yesterday"/utf8>>, warning}],
    ah_div([ah_card([news_rows(News)], [<<"xl:col-span-2">>],
                    [{title, <<"News"/utf8>>}, {subtitle, <<"5 latest updates"/utf8>>}]),
            ah_div([ah_card([authors()], [], [{title, <<"Top authors"/utf8>>},
                                              {subtitle, <<"By reactions"/utf8>>}]),
                    ah_card([inbox_rows(Inbox)], [], [{title, <<"Inbox"/utf8>>},
                                                      {subtitle, <<"Latest messages"/utf8>>}])],
                   [<<"flex flex-col gap-6">>], [])],
           [<<"grid grid-cols-1 gap-6 xl:grid-cols-3">>], []).

news_rows(News) ->
    ah_div([ah_div([#ah_avatar{body = integer_to_binary(20 + I * 7), size = sm,
                               shape = rounded, color = Color},
                    ah_div([ah_div([ah_p(Title, [<<"text-sm font-semibold">>], []),
                                    [ah_chip(<<"NEW"/utf8>>, [success, soft, small], []) || New]],
                                   [<<"flex items-center gap-2">>], []),
                            ah_div([ah_span(Category, [<<"text-xs font-semibold text-primary">>], []),
                                    ah_span(Time, [<<"text-xs text-muted">>], [])],
                                   [<<"mt-1 flex items-center gap-2">>], [])],
                           [<<"min-w-0 flex-1">>], [])],
                   [<<"flex items-start gap-3 border-b border-line py-3 last:border-0">>], [])
            || {I, {Category, Title, Time, Color, New}} <- lists:enumerate(1, News)],
           [<<"flex flex-col">>], []).

authors() ->
    Authors = [{<<"Jayvion Simon"/utf8>>, <<"9.91k"/utf8>>, <<"J"/utf8>>, success},
               {<<"Deja Brady"/utf8>>, <<"9.12k"/utf8>>, <<"D"/utf8>>, info},
               {<<"Lucian Obrien"/utf8>>, <<"1.95k"/utf8>>, <<"L"/utf8>>, warning}],
    ah_div([ah_div([#ah_avatar{body = Initial, size = sm, color = Color},
                    ah_div([ah_p(Name, [<<"truncate text-sm font-semibold">>], []),
                            ah_p([Hearts, <<" reactions"/utf8>>], [<<"text-xs text-muted">>], [])],
                           [<<"min-w-0 flex-1">>], []),
                    ah_chip([<<"#"/utf8>>, integer_to_binary(I)], [Color, soft, small], [])],
                   [<<"flex items-center gap-3 border-b border-line py-3 last:border-0">>], [])
            || {I, {Name, Hearts, Initial, Color}} <- lists:enumerate(1, Authors)],
           [<<"flex flex-col">>], []).

inbox_rows(Inbox) ->
    ah_div([ah_div([ah_span(Initial, [<<"flex size-9 shrink-0 items-center justify-center rounded-full text-xs font-bold text-white">>,
                                      avatar_color(Color)], [{aria_hidden, <<"true">>}]),
                    ah_div([ah_div([ah_p(Name, [<<"truncate text-sm font-semibold">>], []),
                                    ah_span(Time, [<<"shrink-0 text-xs text-muted">>], [])],
                                   [<<"flex items-center justify-between gap-2">>], []),
                            ah_p(Subject, [<<"mt-1 truncate text-xs font-semibold">>], []),
                            ah_p(Preview, [<<"mt-0.5 truncate text-xs text-muted">>], [])],
                           [<<"min-w-0 flex-1">>], [])],
                   [<<"flex items-start gap-3 border-b border-line py-3 last:border-0">>], [])
            || {Initial, Name, Subject, Preview, Time, Color} <- Inbox],
           [<<"flex flex-col">>], []).

avatar_color(success) -> <<" bg-success">>;
avatar_color(info) -> <<" bg-info">>;
avatar_color(warning) -> <<" bg-warning">>;
avatar_color(error) -> <<" bg-error">>;
avatar_color(primary) -> <<" bg-primary">>.

product_row() ->
    Products = [{<<"Atlas Foldable Tablet"/utf8>>, <<"1,284 sold"/utf8>>, <<"+9.2%"/utf8>>, success},
                {<<"Nimbus Pro Headphones"/utf8>>, <<"1,042 sold"/utf8>>, <<"+4.6%"/utf8>>, info},
                {<<"Lumen Smart Lamp"/utf8>>, <<"879 sold"/utf8>>, <<"+2.1%"/utf8>>, warning},
                {<<"Vega Mechanical Keyboard"/utf8>>, <<"712 sold"/utf8>>, <<"-1.4%"/utf8>>, error},
                {<<"Orbit Wireless Mouse"/utf8>>, <<"638 sold"/utf8>>, <<"+0.8%"/utf8>>, primary}],
    Traffic = [{<<"Google"/utf8>>, <<"9.12k"/utf8>>, <<"+18.2%"/utf8>>, error},
               {<<"Twitter"/utf8>>, <<"8.49k"/utf8>>, <<"+5.6%"/utf8>>, info},
               {<<"LinkedIn"/utf8>>, <<"6.98k"/utf8>>, <<"-2.4%"/utf8>>, primary},
               {<<"Facebook"/utf8>>, <<"1.95k"/utf8>>, <<"+12.8%"/utf8>>, warning}],
    ah_div([ah_card([ranked_rows(Products)], [], [{title, <<"Top products"/utf8>>},
                                                  {subtitle, <<"Best sellers this month"/utf8>>}]),
            ah_card([ranked_rows(Traffic)], [], [{title, <<"Traffic by site"/utf8>>},
                                                 {subtitle, <<"Visits last 7 days"/utf8>>}])],
           [<<"grid grid-cols-1 gap-6 xl:grid-cols-2">>], []).

ranked_rows(Rows) ->
    ah_div([ah_div([ah_span(integer_to_binary(I),
                            [<<"flex size-7 shrink-0 items-center justify-center rounded-full text-xs font-bold">>,
                             rank_color(I, Color)], []),
                    ah_div([ah_p(Label, [<<"truncate text-sm font-semibold">>], []),
                            ah_p(Value, [<<"text-xs text-muted">>], [])],
                           [<<"min-w-0 flex-1">>], []),
                    ah_chip(Delta, [Color, soft, small], [])],
                   [<<"flex items-center gap-3 border-b border-line py-3 last:border-0">>], [])
            || {I, {Label, Value, Delta, Color}} <- lists:enumerate(1, Rows)],
           [<<"flex flex-col">>], []).

rank_color(I, _) when I =< 3 -> <<" bg-primary-lighter text-primary-dark">>;
rank_color(_, success) -> <<" bg-success-lighter text-success-dark">>;
rank_color(_, info) -> <<" bg-info-lighter text-info-dark">>;
rank_color(_, warning) -> <<" bg-warning-lighter text-warning-dark">>;
rank_color(_, error) -> <<" bg-error-lighter text-error-dark">>;
rank_color(_, primary) -> <<" bg-primary-lighter text-primary-dark">>.

conversion_row() ->
    ah_div([ah_card([ah_div([conversion(<<"Italy"/utf8>>, 44, success),
                             conversion(<<"Japan"/utf8>>, 55, info),
                             conversion(<<"China"/utf8>>, 41, warning),
                             conversion(<<"Canada"/utf8>>, 64, error),
                             conversion(<<"France"/utf8>>, 72, primary)],
                            [<<"flex flex-col gap-4">>], [])],
                    [<<"xl:col-span-2">>],
                    [{title, <<"Conversion rates"/utf8>>}, {subtitle, <<"(+43%) than last year"/utf8>>}]),
            ah_card([sales_by_country()], [], [{title, <<"Sales by country"/utf8>>},
                                               {subtitle, <<"Top regions this month"/utf8>>}])],
           [<<"grid grid-cols-1 gap-6 xl:grid-cols-3">>], []).

conversion(Label, Pct, Color) ->
    ah_div([ah_div([ah_span(Label, [<<"text-sm font-medium">>], []),
                    ah_span([integer_to_binary(Pct), <<"%">>],
                            [<<"text-sm font-bold">>, color_class(Color)], [])],
                   [<<"flex items-center justify-between">>], []),
            ah_progressbar(Pct, [Color], [{aria_label, Label}])], [], []).

sales_by_country() ->
    Countries = [{<<"🇺🇸"/utf8>>, <<"United States"/utf8>>, <<"$48.2k"/utf8>>, 100, success},
                 {<<"🇩🇪"/utf8>>, <<"Germany"/utf8>>, <<"$28.6k"/utf8>>, 62, info},
                 {<<"🇯🇵"/utf8>>, <<"Japan"/utf8>>, <<"$21.4k"/utf8>>, 46, warning},
                 {<<"🇧🇷"/utf8>>, <<"Brazil"/utf8>>, <<"$15.7k"/utf8>>, 34, primary},
                 {<<"🇮🇳"/utf8>>, <<"India"/utf8>>, <<"$11.8k"/utf8>>, 26, error}],
    ah_div([ah_div([ah_div([ah_span(Flag, [<<"text-base leading-none">>], []),
                            ah_span(Name, [<<"min-w-0 flex-1 truncate text-sm font-medium">>], []),
                            ah_span(Value, [<<"shrink-0 text-sm font-bold">>, color_class(Color)], [])],
                           [<<"flex items-center gap-3">>], []),
                    ah_progressbar(Pct, [Color], [{aria_label, Name}])],
                   [<<"flex flex-col gap-2">>], [])
            || {Flag, Name, Value, Pct, Color} <- Countries],
           [<<"flex flex-col gap-4">>], []).

summary_row() ->
    ah_div([summary_card(<<"Conversion"/utf8>>, <<"38,566"/utf8>>, 48, <<"+12.0%"/utf8>>, success),
            summary_card(<<"Applications"/utf8>>, <<"55,566"/utf8>>, 75, <<"+8.4%"/utf8>>, info)],
           [<<"grid grid-cols-1 gap-6 xl:grid-cols-2">>], []).

summary_card(Label, Total, Pct, Delta, Color) ->
    ah_div([ah_progress_circle(Pct, [Color, lg], [{label, Label}]),
            ah_div([ah_p(Label, [<<"text-xs font-bold uppercase tracking-wide text-muted">>], []),
                    ah_p(Total, [<<"mt-1 text-3xl font-bold tracking-tight">>], []),
                    ah_div([ah_chip(Delta, [Color, soft, small], []),
                            ah_span(<<"vs last month"/utf8>>, [<<"text-xs text-muted">>], [])],
                           [<<"mt-3 flex items-center gap-2">>], [])],
                   [<<"min-w-0">>], [])],
           [<<"flex min-h-40 items-center justify-between gap-6 rounded-surface border border-line bg-surface p-6">>],
           [{style, <<"background: radial-gradient(420px 180px at 92% 10%, "
                      "color-mix(in srgb, var(--ah-color-primary) 12%, transparent), transparent)">>}]).

invoice_row() ->
    ah_card([invoice_table()], [flush],
            [{title, <<"Recent invoices"/utf8>>},
             {subtitle, <<"Last 4 payments"/utf8>>},
             {extra, ah_link_button(<<"View all"/utf8>>, <<"#">>, [sm], [])}]).

invoice_table() ->
    Rows = [[<<"INV-3412"/utf8>>, <<"Aimee Chen"/utf8>>, <<"$1,250.00"/utf8>>, <<"Paid"/utf8>>, success],
            [<<"INV-3411"/utf8>>, <<"Bruno Marais"/utf8>>, <<"$748.20"/utf8>>, <<"Pending"/utf8>>, warning],
            [<<"INV-3410"/utf8>>, <<"Camille Hong"/utf8>>, <<"$2,980.00"/utf8>>, <<"Paid"/utf8>>, success],
            [<<"INV-3409"/utf8>>, <<"Dimitri Volkov"/utf8>>, <<"$316.50"/utf8>>, <<"Refunded"/utf8>>, error]],
    Head = [<<"Invoice"/utf8>>, <<"Customer"/utf8>>, <<"Amount"/utf8>>, <<"Status"/utf8>>],
    Table = [ah_thead(ah_tr([ah_th(H, [<<"text-left">>], []) || H <- Head])),
             ah_tbody([invoice_row_item(Row) || Row <- Rows])],
    ah_div([ah_table(Table, [<<"w-full">>], [])], [<<"w-full overflow-x-auto">>], []).

invoice_row_item([Id, Customer, Amount, Status, Color]) ->
    ah_tr([ah_td(Id, [<<"font-mono text-xs">>], []),
           ah_td(Customer, [], []),
           ah_td(Amount, [<<"font-medium">>], []),
           ah_td([ah_chip(Status, [Color, soft, small], [])], [], [])]).

color_class(primary) -> <<"text-primary">>;
color_class(success) -> <<"text-success">>;
color_class(info) -> <<"text-info">>;
color_class(warning) -> <<"text-warning">>;
color_class(error) -> <<"text-error">>.
