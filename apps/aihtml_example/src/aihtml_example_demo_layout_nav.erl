%% @doc Demos of the navigation components (aihtml_layout_nav), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_layout_nav).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([menubar/0, menu_vertical/0, menu_columns/0, context_menu/0, menu_responsive/0,
         navbar_basic/0, navbar_vertical/0, navbar_minimized/0, navbar_links/0,
         sidenav_groups/0, sidenav_collapsed/0, sidenav_links/0,
         toolbar_editor/0, toolbar_overflow/0,
         splitter_columns/0, splitter_rows/0, splitter_nested/0,
         listmenu_drill/0, listmenu_filter/0, listmenu_nested_value/0,
         status_editor/0, status_segments/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => menu, title => <<"Menu">>,
       summary => <<"带多级子菜单的菜单栏，也可作右键菜单。"/utf8>>,
       demos => [{<<"菜单栏：悬停展开，键盘导航"/utf8>>, menubar},
                 {<<"竖排菜单"/utf8>>, menu_vertical},
                 {<<"多列子菜单与箭头"/utf8>>, menu_columns},
                 {<<"右键菜单"/utf8>>, context_menu},
                 {<<"窄屏折叠为汉堡抽屉"/utf8>>, menu_responsive}]},
     #{component => navbar, title => <<"NavBar">>,
       summary => <<"一排可选中的导航项，带品牌区和右侧内容。"/utf8>>,
       demos => [{<<"品牌、导航项、右侧按钮"/utf8>>, navbar_basic},
                 {<<"竖排"/utf8>>, navbar_vertical},
                 {<<"折叠：汉堡按钮加弹出列表"/utf8>>, navbar_minimized},
                 {<<"链接与自定义列宽"/utf8>>, navbar_links}]},
     #{component => sidenav, title => <<"SideNav">>,
       summary => <<"应用左侧导航：品牌区、分组树、底部插槽，可收窄。"/utf8>>,
       demos => [{<<"分组与当前项"/utf8>>, sidenav_groups},
                 {<<"收窄为图标"/utf8>>, sidenav_collapsed},
                 {<<"路由前缀生成链接"/utf8>>, sidenav_links}]},
     #{component => toolbar, title => <<"Toolbar">>,
       summary => <<"工具按钮和控件排成一行，放不下的收进溢出菜单。"/utf8>>,
       demos => [{<<"切换按钮、分组、分隔符、自定义控件"/utf8>>, toolbar_editor},
                 {<<"窄容器里的溢出菜单"/utf8>>, toolbar_overflow}]},
     #{component => splitter, title => <<"Splitter">>,
       summary => <<"两个面板之间可拖动的分割条，值是两边的百分比。"/utf8>>,
       demos => [{<<"左右分割"/utf8>>, splitter_columns},
                 {<<"上下分割"/utf8>>, splitter_rows},
                 {<<"嵌套"/utf8>>, splitter_nested}]},
     #{component => listmenu, title => <<"ListMenu">>,
       summary => <<"逐级钻取的列表菜单，一次显示一层。"/utf8>>,
       demos => [{<<"逐级钻取"/utf8>>, listmenu_drill},
                 {<<"过滤"/utf8>>, listmenu_filter},
                 {<<"初始值在深层时直接显示所在页"/utf8>>, listmenu_nested_value}]},
     #{component => status_bar, title => <<"StatusBar">>,
       summary => <<"窗口底部的状态条，左右两组分段。"/utf8>>,
       demos => [{<<"字数统计与保存状态"/utf8>>, status_editor},
                 {<<"自定义分段"/utf8>>, status_segments}]}].

%%%-------------------------------------------------------------------
%%% menu
%%%-------------------------------------------------------------------

-spec menubar() -> aihtml:html().
menubar() ->
    menu([#{key => file, label => <<"File">>,
            children => [{new, <<"New">>}, {open, <<"Open…"/utf8>>},
                         #{key => recent, label => <<"Recent">>,
                           children => [{report, <<"report.txt">>}, {notes, <<"notes.md">>}]},
                         divider,
                         {save, <<"Save">>},
                         #{key => export, label => <<"Export">>, disabled => true}]},
          #{key => edit, label => <<"Edit">>,
            children => [{undo, <<"Undo">>}, {redo, <<"Redo">>}, divider,
                         {cut, <<"Cut">>}, {copy, <<"Copy">>}, {paste, <<"Paste">>}]},
          #{key => help, label => <<"Help">>, href => <<"#help">>}],
         undefined, [], [{name, command}]).

-spec menu_vertical() -> aihtml:html().
menu_vertical() ->
    menu([{inbox, <<"Inbox">>}, {starred, <<"Starred">>},
          #{key => labels, label => <<"Labels">>,
            children => [{work, <<"Work">>}, {home, <<"Home">>}]},
          divider,
          {trash, <<"Trash">>}],
         inbox, [vertical], []).

-spec menu_columns() -> aihtml:html().
menu_columns() ->
    menu([#{key => view, label => <<"View">>,
            columns => [#{header => <<"Panels">>,
                          children => [{sidebar, <<"Sidebar">>}, {console, <<"Console">>}]},
                        #{header => <<"Zoom">>,
                          children => [{zoom_in, <<"Zoom in">>}, {zoom_out, <<"Zoom out">>}]}]},
          #{key => window, label => <<"Window">>,
            children => [{minimize, <<"Minimize">>}, {zoom, <<"Zoom">>}]}],
         undefined, [show_arrows], []).

-spec context_menu() -> aihtml:html().
context_menu() ->
    'div'(['div'(<<"在这里点右键"/utf8>>,
                 [<<"border border-dashed border-line rounded p-8 text-sm text-muted">>],
                 [{id, <<"ctx-area">>}]),
           menu([{cut, <<"Cut">>}, {copy, <<"Copy">>}, {paste, <<"Paste">>}, divider,
                 #{key => more, label => <<"More">>,
                   children => [{rename, <<"Rename">>}, {delete, <<"Delete">>}]}],
                undefined, [popup], [{popup_target, <<"#ctx-area">>}])],
          [], []).

-spec menu_responsive() -> aihtml:html().
menu_responsive() ->
    menu([{home, <<"Home">>}, {docs, <<"Docs">>},
          #{key => more, label => <<"More">>,
            children => [{blog, <<"Blog">>}, {about, <<"About">>}]}],
         home, [], [{minimize_width, 768}, {title, <<"Site">>}]).

%%%-------------------------------------------------------------------
%%% navbar
%%%-------------------------------------------------------------------

-spec navbar_basic() -> aihtml:html().
navbar_basic() ->
    navbar([{home, <<"Home">>}, {products, <<"Products">>}, {pricing, <<"Pricing">>},
            #{key => docs, label => <<"Docs">>, disabled => true}],
           products, [],
           [{brand, strong(<<"Acme">>)},
            {extra, button(<<"Sign in">>, undefined, [sm], [])},
            {name, section}]).

-spec navbar_vertical() -> aihtml:html().
navbar_vertical() ->
    'div'(navbar([{profile, <<"Profile">>}, {account, <<"Account">>},
                  {billing, <<"Billing">>}, {security, <<"Security">>}],
                 account, [vertical], []),
          [<<"w-56">>], []).

-spec navbar_minimized() -> aihtml:html().
navbar_minimized() ->
    'div'(navbar([{home, <<"Home">>}, {products, <<"Products">>}, {pricing, <<"Pricing">>}],
                 pricing, [minimized], [{title, <<"Pricing">>}]),
          [<<"w-72">>], []).

-spec navbar_links() -> aihtml:html().
navbar_links() ->
    navbar([#{key => overview, label => <<"Overview">>, href => <<"#overview">>},
            #{key => api, label => <<"API">>, href => <<"#api">>},
            #{key => faq, label => <<"FAQ">>, href => <<"#faq">>}],
           overview, [], [{columns, [<<"50%">>, <<"30%">>, <<"20%">>]}]).

%%%-------------------------------------------------------------------
%%% sidenav
%%%-------------------------------------------------------------------

-spec sidenav_groups() -> aihtml:html().
sidenav_groups() ->
    'div'([sidenav([#{label => <<"Overview">>,
                      items => [{dashboard, <<"Dashboard">>}, {analytics, <<"Analytics">>}]},
                    #{label => <<"Management">>,
                      items => [#{key => users, label => <<"Users">>,
                                  children => [{user_list, <<"List">>}, {roles, <<"Roles">>}]},
                                {settings, <<"Settings">>}]}],
                   roles, [],
                   [{brand, #{name => <<"Sigil">>, logo => strong(<<"S">>)}},
                    {footer, small(<<"v1.0">>, [<<"text-muted">>], [])},
                    {collapsible, true},
                    {style, <<"--ah-ssn-height:100%">>}]),
           'div'(<<"Content">>, [<<"p-4 text-sm text-muted">>], [])],
          [<<"flex h-[420px] border border-line rounded overflow-hidden">>], []).

-spec sidenav_collapsed() -> aihtml:html().
sidenav_collapsed() ->
    'div'(sidenav([#{key => dashboard, label => <<"Dashboard">>, icon => span(<<"▦"/utf8>>)},
                   #{key => inbox, label => <<"Inbox">>, icon => span(<<"✉"/utf8>>)},
                   #{key => settings, label => <<"Settings">>, icon => span(<<"⚙"/utf8>>)}],
                  inbox, [collapsed],
                  [{brand, #{logo => strong(<<"S">>), name => <<"Sigil">>}},
                   {collapsible, true},
                   {style, <<"--ah-ssn-height:100%">>}]),
          [<<"flex h-[300px] border border-line rounded overflow-hidden">>], []).

-spec sidenav_links() -> aihtml:html().
sidenav_links() ->
    'div'(sidenav([{getting_started, <<"Getting started">>},
                   {install, <<"Install">>},
                   #{key => github, label => <<"GitHub">>, href => <<"https://github.com">>,
                     target => <<"_blank">>}],
                  install, [],
                  [{route_prefix, <<"#/docs/">>}, {style, <<"--ah-ssn-height:auto">>}]),
          [<<"flex border border-line rounded overflow-hidden">>], []).

%%%-------------------------------------------------------------------
%%% toolbar
%%%-------------------------------------------------------------------

-spec toolbar_editor() -> aihtml:html().
toolbar_editor() ->
    toolbar([#{key => bold, label => <<"B">>, title => <<"Bold">>, toggle => true, pressed => true},
             #{key => italic, label => <<"I">>, title => <<"Italic">>, toggle => true},
             #{key => underline, label => <<"U">>, title => <<"Underline">>, toggle => true},
             separator,
             #{key => left, label => <<"Left">>},
             #{key => center, label => <<"Center">>},
             #{key => right, label => <<"Right">>},
             separator,
             #{key => undo, label => <<"Undo">>},
             #{key => redo, label => <<"Redo">>, disabled => true},
             separator,
             {custom, select([{p, <<"Paragraph">>}, {h1, <<"Heading">>}], p, [sm], [])}],
            [], [{aria_label, <<"Formatting">>}]).

-spec toolbar_overflow() -> aihtml:html().
toolbar_overflow() ->
    'div'(toolbar([#{key => new, label => <<"New">>},
                   #{key => open, label => <<"Open">>},
                   #{key => save, label => <<"Save">>, minimizable => false},
                   separator,
                   #{key => cut, label => <<"Cut">>},
                   #{key => copy, label => <<"Copy">>},
                   #{key => paste, label => <<"Paste">>},
                   separator,
                   #{key => print, label => <<"Print">>}],
                  [], [{popup_width, 160}]),
          [<<"w-64">>], []).

%%%-------------------------------------------------------------------
%%% splitter
%%%-------------------------------------------------------------------

-spec splitter_columns() -> aihtml:html().
splitter_columns() ->
    'div'(splitter([#{content => pane(<<"Left, at least 80px">>), size => <<"30%">>, min => 80},
                    #{content => pane(<<"Right">>), min => 80}],
                   [], [{name, split}]),
          [<<"h-40 border border-line rounded">>], []).

-spec splitter_rows() -> aihtml:html().
splitter_rows() ->
    'div'(splitter([#{content => pane(<<"Editor">>), size => <<"65%">>, min => 40},
                    pane(<<"Console">>)],
                   [horizontal], [{splitbar_size, 6}]),
          [<<"h-56 border border-line rounded">>], []).

-spec splitter_nested() -> aihtml:html().
splitter_nested() ->
    'div'(splitter([#{content => pane(<<"Tree">>), size => 160, min => 100},
                    splitter([pane(<<"Code">>), pane(<<"Preview">>)], [horizontal], [])],
                   [], []),
          [<<"h-56 border border-line rounded">>], []).

%%%-------------------------------------------------------------------
%%% listmenu
%%%-------------------------------------------------------------------

-spec listmenu_drill() -> aihtml:html().
listmenu_drill() ->
    'div'(listmenu(food(), undefined, [], [{name, food}]),
          [<<"w-72 border border-line rounded">>], []).

-spec listmenu_filter() -> aihtml:html().
listmenu_filter() ->
    'div'(listmenu(food(), undefined, [],
                   [{filter, true}, {filter_placeholder, <<"Filter food">>},
                    {animation, fade}]),
          [<<"w-72 border border-line rounded">>], []).

-spec listmenu_nested_value() -> aihtml:html().
listmenu_nested_value() ->
    'div'(listmenu(food(), lemon, [], [{back_label, <<"Up">>}]),
          [<<"w-72 border border-line rounded">>], []).

%%%-------------------------------------------------------------------
%%% status_bar
%%%-------------------------------------------------------------------

-spec status_editor() -> aihtml:html().
status_editor() ->
    status_bar([<<"Ln 12, Col 4">>,
                #{content => <<"UTF-8">>, align => right},
                #{content => <<"Markdown">>, align => right}],
               [], [{content, <<"Hello 世界，这是一段中英混排文本。\n\n第二段。"/utf8>>},
                    {dirty, false}]).

-spec status_segments() -> aihtml:html().
status_segments() ->
    status_bar([#{count => 3, label => <<"errors">>,
                  details => [{<<"Errors">>, 3}, {<<"Warnings">>, 7}]},
                <<"main">>,
                #{content => <<"Spaces: 2">>, align => right}],
               [], [{dirty, true}, {labels, #{unsaved => <<"Modified">>}}]).

%% Shared by the demos above.
pane(Text) ->
    'div'(Text, [<<"p-3 text-sm">>], []).

food() ->
    [#{key => fruit, label => <<"Fruit">>,
       children => [{apple, <<"Apple">>}, {banana, <<"Banana">>},
                    #{key => citrus, label => <<"Citrus">>,
                      children => [{lemon, <<"Lemon">>}, {orange, <<"Orange">>}]}]},
     #{key => veg, label => <<"Vegetables">>,
       children => [{carrot, <<"Carrot">>}, {pea, <<"Pea">>}]},
     {bread, <<"Bread">>},
     #{key => cake, label => <<"Cake">>, disabled => true}].
