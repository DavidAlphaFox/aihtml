%% @doc Demos of dropdown_button (aihtml_dropdown_button), shown on
%% /components/dropdown_button. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_dropdown_button).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_button, [row/1, menu/0]).

-export([demos/0]).
-export([dropdown/0, dropdown_variants/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => dropdown_button, title => <<"DropdownButton">>,
       summary => <<"点击展开菜单的按钮，选中的项就是它的值。"/utf8>>,
       demos => [{<<"菜单：选中的项写入 data-ah-value"/utf8>>, dropdown},
                 {<<"变体与尺寸"/utf8>>, dropdown_variants}]}].

-spec dropdown() -> aihtml:html().
dropdown() ->
    row([dropdown_button(<<"Actions">>, menu(), [], [{name, action}]),
         dropdown_button(<<"Hover me">>, menu(), [outlined], [{auto_open, true}]),
         dropdown_button(<<"Disabled">>, menu(), [], [{disabled, true}])]).

-spec dropdown_variants() -> aihtml:html().
dropdown_variants() ->
    row([dropdown_button(<<"Primary">>, menu(), [primary], [{value, copy}]),
         dropdown_button(<<"Outlined">>, menu(), [outlined, rounded], []),
         dropdown_button(<<"Small">>, menu(), [sm, success], []),
         dropdown_button(<<"Large">>, menu(), [lg, warning], [])]).
