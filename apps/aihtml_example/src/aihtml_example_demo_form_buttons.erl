%% @doc Demos of the button components (aihtml_form_buttons), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_form_buttons).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([button_variants/0, button_sizes/0, button_states/0, button_icons/0, button_records/0,
         link_buttons/0, toggle_buttons/0, group_modes/0, group_layouts/0,
         segmented/0, segmented_full/0, dropdown/0, dropdown_variants/0,
         split/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => button, title => <<"Button">>,
       summary => <<"原生按钮，九种变体、三种尺寸、圆角与图标。"/utf8>>,
       demos => [{<<"变体"/utf8>>, button_variants},
                 {<<"尺寸"/utf8>>, button_sizes},
                 {<<"圆角与禁用"/utf8>>, button_states},
                 {<<"图标"/utf8>>, button_icons},
                 {<<"record 写法"/utf8>>, button_records}]},
     #{component => link_button, title => <<"LinkButton">>,
       summary => <<"外观是按钮的链接。"/utf8>>,
       demos => [{<<"看起来像按钮的链接"/utf8>>, link_buttons}]},
     #{component => toggle_button, title => <<"ToggleButton">>,
       summary => <<"可以保持按下状态的按钮，值为 true 或 false。"/utf8>>,
       demos => [{<<"按下与弹起"/utf8>>, toggle_buttons}]},
     #{component => button_group, title => <<"ButtonGroup">>,
       summary => <<"连在一起的一组按钮，可作单选或多选。"/utf8>>,
       demos => [{<<"默认、单选、多选"/utf8>>, group_modes},
                 {<<"竖排、填充、禁用"/utf8>>, group_layouts}]},
     #{component => segmented_control, title => <<"SegmentedControl">>,
       summary => <<"互斥的分段选择。"/utf8>>,
       demos => [{<<"尺寸"/utf8>>, segmented},
                 {<<"占满宽度、禁用项"/utf8>>, segmented_full}]},
     #{component => dropdown_button, title => <<"DropdownButton">>,
       summary => <<"点击展开菜单的按钮，选中的项就是它的值。"/utf8>>,
       demos => [{<<"菜单：选中的项写入 data-ah-value"/utf8>>, dropdown},
                 {<<"变体与尺寸"/utf8>>, dropdown_variants}]},
     #{component => split_button, title => <<"SplitButton">>,
       summary => <<"主操作按钮加一个展开更多操作的箭头。"/utf8>>,
       demos => [{<<"主操作加菜单"/utf8>>, split}]}].

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

-spec link_buttons() -> aihtml:html().
link_buttons() ->
    row([link_button(<<"Primary link">>, <<"#top">>, [], []),
         link_button(<<"Outlined">>, <<"#top">>, [outlined, round], []),
         link_button(<<"Small borderless">>, <<"#top">>, [borderless, sm], []),
         link_button(<<"Disabled">>, <<"#top">>, [secondary], [{disabled, true}])]).

-spec toggle_buttons() -> aihtml:html().
toggle_buttons() ->
    row([toggle_button(<<"Bold">>, false, [default], []),
         toggle_button(<<"Italic">>, true, [default], []),
         toggle_button(<<"Primary">>, true, [], []),
         toggle_button(<<"Outlined">>, false, [outlined, round], []),
         toggle_button(<<"Disabled">>, true, [secondary], [{disabled, true}])]).

-spec group_modes() -> aihtml:html().
group_modes() ->
    Views = [{list, <<"List">>}, {grid, <<"Grid">>}, {board, <<"Board">>}],
    row([button_group([<<"Left">>, <<"Middle">>, <<"Right">>], undefined, [], []),
         button_group(Views, grid, [radio], [{name, view}]),
         button_group([{b, <<"B">>}, {i, <<"I">>}, {u, <<"U">>}], [b, u], [checkbox, square], [])]).

-spec group_layouts() -> aihtml:html().
group_layouts() ->
    Views = [{list, <<"List">>}, {grid, <<"Grid">>}, {board, <<"Board">>}],
    row([button_group(Views, list, [radio, vertical], []),
         button_group(Views, board, [radio, filled], []),
         button_group(Views, list, [radio, outlined, square], []),
         button_group([{a, <<"Enabled">>}, {b, <<"Off">>, [{disabled, true}]}, {c, <<"On">>}],
                      c, [radio], []),
         button_group(Views, grid, [radio], [{disabled, true}])]).

-spec segmented() -> aihtml:html().
segmented() ->
    Views = [{list, <<"List">>}, {grid, <<"Grid">>}, {board, <<"Board">>}],
    row([segmented_control(Views, list, [sm], []),
         segmented_control(Views, grid, [], [{name, layout}]),
         segmented_control(Views, board, [lg], [])]).

-spec segmented_full() -> aihtml:html().
segmented_full() ->
    'div'([segmented_control([{day, <<"Day">>}, {week, <<"Week">>, [{disabled, true}]},
                              {month, <<"Month">>}], day, [full_width], []),
           segmented_control([{a, <<"A">>}, {b, <<"B">>}], a, [], [{disabled, true}])],
          [<<"flex flex-col gap-3">>], []).

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

-spec split() -> aihtml:html().
split() ->
    row([split_button(<<"Save">>, menu(), [], [{menu_align, start}]),
         split_button(<<"Secondary">>, menu(), [secondary], []),
         split_button(<<"Small">>, menu(), [success, sm], []),
         split_button(<<"Large">>, menu(), [error, lg], []),
         split_button(<<"Outlined">>, menu(), [outlined], []),
         split_button(<<"Disabled">>, menu(), [info], [{disabled, true}])]).

%% Shared by the menu demos.
menu() ->
    [{draft, <<"Save as draft">>}, {copy, <<"Save a copy">>}, divider,
     {template, <<"Save as template">>},
     {locked, <<"Publish">>, [{disabled, true}]}].

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-3">>], []).
