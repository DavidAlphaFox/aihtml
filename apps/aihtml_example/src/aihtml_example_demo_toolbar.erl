%% @doc Demos of the Toolbar component (aihtml_toolbar), shown on
%% /components/toolbar. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_toolbar).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([toolbar_editor/0, toolbar_overflow/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => toolbar, title => <<"Toolbar">>,
       summary => <<"工具按钮和控件排成一行，放不下的收进溢出菜单。"/utf8>>,
       demos => [{<<"切换按钮、分组、分隔符、自定义控件"/utf8>>, toolbar_editor},
                 {<<"窄容器里的溢出菜单"/utf8>>, toolbar_overflow}]}].

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
