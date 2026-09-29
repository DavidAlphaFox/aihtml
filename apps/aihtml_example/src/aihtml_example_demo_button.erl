%% @doc Demos of button (aihtml_button), shown on
%% /components/button. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_button).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_button, [row/1]).

-export([demos/0]).
-export([button_variants/0, button_sizes/0, button_states/0, button_icons/0, button_records/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => button, title => <<"Button">>,
       summary => <<"原生按钮，九种变体、三种尺寸、圆角与图标。"/utf8>>,
       demos => [{<<"变体"/utf8>>, button_variants},
                 {<<"尺寸"/utf8>>, button_sizes},
                 {<<"圆角与禁用"/utf8>>, button_states},
                 {<<"图标"/utf8>>, button_icons},
                 {<<"record 写法"/utf8>>, button_records}]}].

-spec button_variants() -> aihtml:html().
button_variants() ->
    row([button(<<"Primary">>, save, [primary], []),
         button(<<"Secondary">>, save, [secondary], []),
         button(<<"Outlined">>, save, [outlined], []),
         button(<<"Success">>, save, [success], []),
         button(<<"Warning">>, save, [warning], []),
         button(<<"Error">>, save, [error], []),
         button(<<"Info">>, save, [info], []),
         button(<<"Default">>, save, [default], []),
         button(<<"Borderless">>, save, [borderless], [])]).

-spec button_sizes() -> aihtml:html().
button_sizes() ->
    row([button(<<"Small">>, undefined, [sm], []),
         button(<<"Medium">>, undefined, [], []),
         button(<<"Large">>, undefined, [lg], []),
         button(<<"Full width">>, undefined, [outlined, <<"w-full">>], [])]).

-spec button_states() -> aihtml:html().
button_states() ->
    row([button(<<"Round">>, undefined, [round], []),
         button(<<"Round secondary">>, undefined, [round, secondary], []),
         button(<<"Disabled">>, undefined, [], [{disabled, true}]),
         button(<<"Outlined disabled">>, undefined, [outlined], [{disabled, true}]),
         button(<<"Submit">>, undefined, [success], [{type, submit}])]).

-spec button_icons() -> aihtml:html().
button_icons() ->
    row([button(<<"Icon left">>, undefined, [], [{icon, <<"★"/utf8>>}]),
         button(<<"Icon right">>, undefined, [outlined],
                [{icon, <<"→"/utf8>>}, {icon_position, right}]),
         button(<<"Top">>, undefined, [default], [{icon, <<"☰"/utf8>>}, {icon_position, top}])]).

-spec button_records() -> aihtml:html().
button_records() ->
    row([#ah_button{body = <<"Save">>, variant = success, icon = <<"✓"/utf8>>},
         #ah_button{body = <<"Large outlined">>, variant = outlined, size = lg, round = true},
         #ah_button{body = <<"Disabled">>, disabled = true, css = [<<"opacity-80">>]}]).
