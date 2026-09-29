%% @doc Demos of the selection and form layout components
%% (aihtml_form_select), shown on /components/<name>. Each function is one
%% example, written the way an application writes it; the docs page prints
%% its source under it.
-module(aihtml_example_demo_form_select).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([dropdown_basic/0, dropdown_templates/0, dropdown_groups/0, dropdown_states/0,
         select_basic/0, select_sizes/0, select_groups/0,
         slider_basic/0, slider_range/0, slider_ticks/0, slider_vertical/0,
         field_positions/0, field_help/0, field_validate/0,
         form_basic/0, form_columns/0, form_validate/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => dropdownlist, title => <<"DropDownList">>,
       summary => <<"弹出列表的单选下拉，支持键盘、输入跳转、分组与过滤。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, dropdown_basic},
                 {<<"颜色模板"/utf8>>, dropdown_templates},
                 {<<"分组、禁用项与过滤"/utf8>>, dropdown_groups},
                 {<<"无箭头、禁用、占满宽度"/utf8>>, dropdown_states}]},
     #{component => select, title => <<"Select">>,
       summary => <<"原生下拉框，外观与 DropDownList 一致。"/utf8>>,
       demos => [{<<"基本用法与占位项"/utf8>>, select_basic},
                 {<<"尺寸与颜色"/utf8>>, select_sizes},
                 {<<"分组、多选与禁用"/utf8>>, select_groups}]},
     #{component => slider, title => <<"Slider">>,
       summary => <<"拖动或用键盘选择数值，也可以选一个区间。"/utf8>>,
       demos => [{<<"单值与提示气泡"/utf8>>, slider_basic},
                 {<<"区间选择"/utf8>>, slider_range},
                 {<<"刻度与步进按钮"/utf8>>, slider_ticks},
                 {<<"竖向与禁用"/utf8>>, slider_vertical}]},
     #{component => field, title => <<"Field">>,
       summary => <<"一行表单项：标签、控件，以及帮助或错误信息。"/utf8>>,
       demos => [{<<"标签位置"/utf8>>, field_positions},
                 {<<"必填、帮助与错误"/utf8>>, field_help},
                 {<<"客户端校验：气泡与标签提示"/utf8>>, field_validate}]},
     #{component => form_layout, title => <<"Form">>,
       summary => <<"声明式表单：用字段列表和取值生成整张表单。"/utf8>>,
       demos => [{<<"字段与取值"/utf8>>, form_basic},
                 {<<"多列、文字行与空行"/utf8>>, form_columns},
                 {<<"提交前校验"/utf8>>, form_validate}]}].

%%% DropDownList

-spec dropdown_basic() -> aihtml:html().
dropdown_basic() ->
    row([dropdownlist(fruits(), banana, [<<"w-48">>], [{name, fruit}]),
         dropdownlist(fruits(), undefined, [<<"w-48">>], [{placeholder, <<"Pick a fruit">>}])]).

-spec dropdown_templates() -> aihtml:html().
dropdown_templates() ->
    row([dropdownlist(fruits(), apple, [primary, <<"w-40">>], []),
         dropdownlist(fruits(), cherry, [success, <<"w-40">>], []),
         dropdownlist(fruits(), lemon, [warning, <<"w-40">>], []),
         dropdownlist(fruits(), grape, [danger, <<"w-40">>], [])]).

-spec dropdown_groups() -> aihtml:html().
dropdown_groups() ->
    Items = [{group, <<"Citrus">>, [{lemon, <<"Lemon">>}, {orange, <<"Orange">>}]},
             {group, <<"Berries">>, [{straw, <<"Strawberry">>},
                                     {blue, <<"Blueberry">>, #{disabled => true}},
                                     {rasp, <<"Raspberry">>}]}],
    row([dropdownlist(Items, rasp, [<<"w-56">>], [{filterable, true}, {name, berry}]),
         dropdownlist(Items, undefined, [<<"w-56">>], [{dropdown_height, 120}])]).

-spec dropdown_states() -> aihtml:html().
dropdown_states() ->
    'div'([row([dropdownlist(fruits(), cherry, [simple, <<"w-40">>], []),
                dropdownlist(fruits(), apple, [disabled, <<"w-40">>], [])]),
           dropdownlist(fruits(), mango, [block], [])],
          [<<"flex flex-col gap-3">>], []).

%%% Select

-spec select_basic() -> aihtml:html().
select_basic() ->
    row([select(fruits(), grape, [], [{name, fruit}]),
         select(fruits(), undefined, [], [{name, other}, {placeholder, <<"Choose…"/utf8>>}])]).

-spec select_sizes() -> aihtml:html().
select_sizes() ->
    row([select(fruits(), apple, [sm], []),
         select(fruits(), apple, [], []),
         select(fruits(), apple, [lg], []),
         select(fruits(), peach, [primary], []),
         select(fruits(), lemon, [danger], [])]).

-spec select_groups() -> aihtml:html().
select_groups() ->
    row([select([{group, <<"Warm">>, [red, orange]}, {group, <<"Cool">>, [blue, green]}],
                blue, [], [{name, colour}]),
         select(fruits(), [apple, mango], [], [{multiple, true}, {size, 4}]),
         select(fruits(), lemon, [], [{disabled, true}])]).

%%% Slider

-spec slider_basic() -> aihtml:html().
slider_basic() ->
    'div'([slider({0, 100}, 40, [tooltip], [{name, volume}, {aria_label, <<"Volume">>}]),
           slider({0, 100}, 70, [success], [{aria_label, <<"Brightness">>}])],
          [<<"flex flex-col gap-6 max-w-sm">>], []).

-spec slider_range() -> aihtml:html().
slider_range() ->
    'div'(slider({0, 1000, 50}, {200, 600}, [tooltip],
                 [{name, price}, {min_range, 100}, {ticks, 250}]),
          [<<"max-w-sm">>], []).

-spec slider_ticks() -> aihtml:html().
slider_ticks() ->
    'div'([slider({0, 10}, 3, [buttons, warning], [{ticks, 1}, {ticks_position, both}]),
           slider({0, 1, 0.05}, 0.5, [info], [{ticks, 0.25}, {minor_ticks, 0.05}])],
          [<<"flex flex-col gap-8 max-w-sm py-4">>], []).

-spec slider_vertical() -> aihtml:html().
slider_vertical() ->
    row([slider({0, 100}, 60, [vertical], [{ticks, 25}]),
         slider({0, 100}, {20, 80}, [vertical, secondary], []),
         slider({0, 100}, 30, [vertical, disabled], [])]).

%%% Field

-spec field_positions() -> aihtml:html().
field_positions() ->
    'div'([field(<<"Name">>, input(undefined, [], [{id, <<"fp-name">>}]),
                 [], [{for, <<"fp-name">>}, {label_width, 90}]),
           field(<<"Country">>, dropdownlist([{cn, <<"China">>}, {jp, <<"Japan">>}], cn, [block], []),
                 [top], []),
           field(<<"Remember me">>, checkbox(<<>>, yes, [], [{name, remember}, {id, <<"fp-rem">>}]),
                 [right], [{for, <<"fp-rem">>}])],
          [<<"max-w-md">>], []).

-spec field_help() -> aihtml:html().
field_help() ->
    'div'([field(<<"Name">>, input(undefined, [], [{id, <<"fh-name">>}]), [],
                 [{for, <<"fh-name">>}, {required, true}, {label_width, 90},
                  {help, <<"As printed on your card.">>}]),
           field(<<"Email">>, input(<<"not-an-email">>, [], [{id, <<"fh-email">>}]), [],
                 [{for, <<"fh-email">>}, {label_width, 90},
                  {error, <<"Please enter a valid email address.">>}]),
           field(<<"Plan">>, dropdownlist([free, pro], pro, [block], []), [],
                 [{label_width, 90}, {info, <<"You can change it later.">>}])],
          [<<"max-w-md">>], []).

-spec field_validate() -> aihtml:html().
field_validate() ->
    form([input(undefined, [<<"w-64">>],
                [{name, zip}, {placeholder, <<"ZIP code (tooltip)">>},
                 validate([{required, <<"Enter a ZIP code">>}, zip_code, {hint, tooltip}])]),
          input(undefined, [<<"w-64">>],
                [{name, nick}, {placeholder, <<"Nickname (label)">>},
                 validate([{min_length, 3}, {hint, label}])]),
          button(<<"Check">>, undefined, [outlined], [{type, submit}])],
         [<<"flex flex-col items-start gap-3">>], [{action, <<"#checked">>}]).

%%% Form

-spec form_basic() -> aihtml:html().
form_basic() ->
    Values = #{name => <<"Ada">>, plan => pro, budget => 400},
    form_layout(
      [#{label => <<"Name">>, key => name,
         control => fun(V) -> input(V, [], [{name, name}]) end},
       #{label => <<"Plan">>, key => plan,
         control => fun(V) -> dropdownlist([free, pro, team], V, [block], [{name, plan}]) end},
       #{label => <<"Budget">>, key => budget,
         control => fun(V) -> slider({0, 1000, 50}, V, [tooltip], [{name, budget}]) end},
       {<<>>, button(<<"Save">>, undefined, [], [{type, submit}])}],
      Values, [bordered, bg, <<"max-w-lg">>], [{label_width, 90}, {action, <<"#saved">>}]).

-spec form_columns() -> aihtml:html().
form_columns() ->
    form_layout(
      [{text, <<"Shipping address">>},
       {<<"Street">>, input(undefined, [], [{name, street}])},
       {columns, [{<<"City">>, input(undefined, [], [{name, city}])},
                  {<<"ZIP">>, input(undefined, [], [{name, zip}])}]},
       blank,
       #{label => <<"Country">>, label_position => top,
         control => select([{cn, <<"China">>}, {jp, <<"Japan">>}, {us, <<"USA">>}], us,
                           [block], [{name, country}])}],
      #{}, [bordered, <<"max-w-lg">>], [{tag, 'div'}, {label_width, 70}]).

-spec form_validate() -> aihtml:html().
form_validate() ->
    form_layout(
      [#{label => <<"User">>, key => user, required => true,
         control => fun(V) -> input(V, [], [{name, user},
                                            validate([required, {min_length, 3},
                                                      {starts_with_letter, <<"Must start with a letter">>}])])
                    end},
       #{label => <<"Email">>, key => email, required => true, help => <<"We never share it.">>,
         control => fun(V) -> input(V, [], [{name, email}, validate([required, email])]) end},
       #{label => <<"Age">>, key => age,
         control => fun(V) -> input(V, [], [{name, age}, validate([integer, {range, 18, 120}])]) end},
       #{label => <<"Plan">>, key => plan, required => true,
         control => fun(V) -> dropdownlist([free, pro, team], V, [block],
                                           [{name, plan}, validate([required])])
                    end},
       {<<>>, button(<<"Sign up">>, undefined, [], [{type, submit}])}],
      #{user => <<"x">>, age => <<"12">>},
      [bordered, bg, <<"max-w-lg">>], [{label_width, 70}, {action, <<"#signed-up">>}]).

%% Shared by the demos.
fruits() ->
    [{apple, <<"Apple">>}, {banana, <<"Banana">>}, {cherry, <<"Cherry">>},
     {grape, <<"Grape">>}, {lemon, <<"Lemon">>}, {mango, <<"Mango">>},
     {orange, <<"Orange">>}, {peach, <<"Peach">>}].

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
