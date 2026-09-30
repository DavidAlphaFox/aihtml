%% @doc Demos of the Menu component (aihtml_menu), shown on
%% /components/menu. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_menu).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([menubar/0, menu_vertical/0, menu_columns/0, context_menu/0, menu_responsive/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => menu, title => <<"Menu">>,
       summary => <<"带多级子菜单的菜单栏，也可作右键菜单。"/utf8>>,
       demos => [{<<"菜单栏：悬停展开，键盘导航"/utf8>>, menubar},
                 {<<"竖排菜单"/utf8>>, menu_vertical},
                 {<<"多列子菜单与箭头"/utf8>>, menu_columns},
                 {<<"右键菜单"/utf8>>, context_menu},
                 {<<"窄屏折叠为汉堡抽屉"/utf8>>, menu_responsive}]}].

%%%-------------------------------------------------------------------
%%% menu
%%%-------------------------------------------------------------------

-spec menubar() -> aihtml:html().
menubar() ->
    ah_menu([#{key => file, label => <<"File">>,
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
    ah_menu([{inbox, <<"Inbox">>}, {starred, <<"Starred">>},
             #{key => labels, label => <<"Labels">>,
               children => [{work, <<"Work">>}, {home, <<"Home">>}]},
             divider,
             {trash, <<"Trash">>}],
            inbox, [vertical], []).

-spec menu_columns() -> aihtml:html().
menu_columns() ->
    ah_menu([#{key => view, label => <<"View">>,
               columns => [#{header => <<"Panels">>,
                             children => [{sidebar, <<"Sidebar">>}, {console, <<"Console">>}]},
                           #{header => <<"Zoom">>,
                             children => [{zoom_in, <<"Zoom in">>}, {zoom_out, <<"Zoom out">>}]}]},
             #{key => window, label => <<"Window">>,
               children => [{minimize, <<"Minimize">>}, {zoom, <<"Zoom">>}]}],
            undefined, [show_arrows], []).

-spec context_menu() -> aihtml:html().
context_menu() ->
    ah_div([ah_div(<<"在这里点右键"/utf8>>,
                   [<<"border border-dashed border-line rounded p-8 text-sm text-muted">>],
                   [{id, <<"ctx-area">>}]),
            ah_menu([{cut, <<"Cut">>}, {copy, <<"Copy">>}, {paste, <<"Paste">>}, divider,
                     #{key => more, label => <<"More">>,
                       children => [{rename, <<"Rename">>}, {delete, <<"Delete">>}]}],
                    undefined, [popup], [{popup_target, <<"#ctx-area">>}])],
           [], []).

-spec menu_responsive() -> aihtml:html().
menu_responsive() ->
    ah_menu([{home, <<"Home">>}, {docs, <<"Docs">>},
             #{key => more, label => <<"More">>,
               children => [{blog, <<"Blog">>}, {about, <<"About">>}]}],
            home, [], [{minimize_width, 768}, {title, <<"Site">>}]).
