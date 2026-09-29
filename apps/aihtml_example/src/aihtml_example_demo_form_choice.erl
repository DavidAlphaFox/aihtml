%% @doc Demos of the choice components (aihtml_form_choice), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_form_choice).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([checkbox_states/0, checkbox_sizes/0, checkbox_three_states/0,
         radio_basic/0, radio_sizes/0,
         switch_basic/0, switch_labels/0, switch_sizes/0,
         checkbox_group_vertical/0, checkbox_group_layouts/0, checkbox_group_disabled/0,
         radio_group_vertical/0, radio_group_layouts/0, radio_group_disabled/0,
         radio_cards_plans/0, radio_cards_icons/0, radio_cards_disabled/0,
         rating_basic/0, rating_half/0, rating_sizes/0, rating_readonly/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => checkbox, title => <<"Checkbox">>,
       summary => <<"复选框，支持半选、三态循环和锁定。"/utf8>>,
       demos => [{<<"状态"/utf8>>, checkbox_states},
                 {<<"尺寸"/utf8>>, checkbox_sizes},
                 {<<"三态与锁定"/utf8>>, checkbox_three_states}]},
     #{component => radiobutton, title => <<"RadioButton">>,
       summary => <<"单选按钮，同名的一组互斥。"/utf8>>,
       demos => [{<<"同名互斥"/utf8>>, radio_basic},
                 {<<"尺寸与禁用"/utf8>>, radio_sizes}]},
     #{component => switch_button, title => <<"SwitchButton">>,
       summary => <<"滑动开关，开或关两种状态。"/utf8>>,
       demos => [{<<"开与关"/utf8>>, switch_basic},
                 {<<"轨道文字"/utf8>>, switch_labels},
                 {<<"尺寸与自定义大小"/utf8>>, switch_sizes}]},
     #{component => checkbox_group, title => <<"CheckboxGroup">>,
       summary => <<"一组复选框，值是选中项的列表。"/utf8>>,
       demos => [{<<"竖排"/utf8>>, checkbox_group_vertical},
                 {<<"横排、标签在前"/utf8>>, checkbox_group_layouts},
                 {<<"整组禁用"/utf8>>, checkbox_group_disabled}]},
     #{component => radiobutton_group, title => <<"RadioButtonGroup">>,
       summary => <<"一组单选按钮，方向键切换选中项。"/utf8>>,
       demos => [{<<"竖排"/utf8>>, radio_group_vertical},
                 {<<"横排、标签在前"/utf8>>, radio_group_layouts},
                 {<<"整组禁用"/utf8>>, radio_group_disabled}]},
     #{component => radio_cards, title => <<"RadioCards">>,
       summary => <<"卡片式单选，每项可带说明和图标。"/utf8>>,
       demos => [{<<"套餐选择"/utf8>>, radio_cards_plans},
                 {<<"图标、三列"/utf8>>, radio_cards_icons},
                 {<<"禁用"/utf8>>, radio_cards_disabled}]},
     #{component => rating_group, title => <<"RatingGroup">>,
       summary => <<"星级评分，支持半星、悬停预览和只读。"/utf8>>,
       demos => [{<<"评分"/utf8>>, rating_basic},
                 {<<"半星"/utf8>>, rating_half},
                 {<<"尺寸与颜色"/utf8>>, rating_sizes},
                 {<<"只读与禁用"/utf8>>, rating_readonly}]}].

%% --- checkbox ------------------------------------------------------

-spec checkbox_states() -> aihtml:html().
checkbox_states() ->
    row([checkbox(<<"Unchecked">>, undefined, [], [{name, a}]),
         checkbox(<<"Checked">>, undefined, [], [{name, b}, {checked, true}]),
         checkbox(<<"Indeterminate">>, undefined, [], [{indeterminate, true}]),
         checkbox(<<"Disabled">>, undefined, [], [{disabled, true}]),
         checkbox(<<"Disabled checked">>, undefined, [], [{disabled, true}, {checked, true}])]).

-spec checkbox_sizes() -> aihtml:html().
checkbox_sizes() ->
    row([checkbox(<<"Small">>, undefined, [sm], [{checked, true}]),
         checkbox(<<"Medium">>, undefined, [md], [{checked, true}]),
         checkbox(<<"Large">>, undefined, [lg], [{checked, true}]),
         checkbox(<<"24px box">>, undefined, [], [{box_size, 24}, {checked, true}])]).

-spec checkbox_three_states() -> aihtml:html().
checkbox_three_states() ->
    row([checkbox(<<"Click me: on, mixed, off">>, undefined, [],
                  [{three_states, true}, {checked, true}]),
         checkbox(<<"Locked">>, undefined, [], [{locked, true}, {checked, true}])]).

%% --- radiobutton ---------------------------------------------------

-spec radio_basic() -> aihtml:html().
radio_basic() ->
    row([radiobutton(<<"Email">>, email, [], [{name, contact}, {checked, true}]),
         radiobutton(<<"Phone">>, phone, [], [{name, contact}]),
         radiobutton(<<"Post">>, post, [], [{name, contact}])]).

-spec radio_sizes() -> aihtml:html().
radio_sizes() ->
    row([radiobutton(<<"Small">>, s, [sm], [{name, size}, {checked, true}]),
         radiobutton(<<"Medium">>, m, [md], [{name, size}]),
         radiobutton(<<"Large">>, l, [lg], [{name, size}]),
         radiobutton(<<"Disabled">>, x, [], [{disabled, true}, {checked, true}])]).

%% --- switch_button -------------------------------------------------

-spec switch_basic() -> aihtml:html().
switch_basic() ->
    row([switch_button(<<"Wi-Fi">>, undefined, [], [{name, wifi}, {checked, true}]),
         switch_button(<<"Bluetooth">>, undefined, [], [{name, bluetooth}]),
         switch_button(<<"Disabled">>, undefined, [], [{disabled, true}]),
         switch_button(<<"Locked">>, undefined, [], [{locked, true}, {checked, true}])]).

-spec switch_labels() -> aihtml:html().
switch_labels() ->
    row([switch_button(<<"Notifications">>, undefined, [],
                       [{on_label, <<"On">>}, {off_label, <<"Off">>}, {checked, true}]),
         switch_button(<<"Auto save">>, undefined, [],
                       [{on_label, <<"是"/utf8>>}, {off_label, <<"否"/utf8>>}])]).

-spec switch_sizes() -> aihtml:html().
switch_sizes() ->
    row([switch_button(<<"Small">>, undefined, [sm], [{checked, true}]),
         switch_button(<<"Medium">>, undefined, [md], [{checked, true}]),
         switch_button(<<"Large">>, undefined, [lg], [{checked, true}]),
         switch_button(<<"80 x 32">>, undefined, [], [{width, 80}, {height, 32}])]).

%% --- checkbox_group ------------------------------------------------

-spec checkbox_group_vertical() -> aihtml:html().
checkbox_group_vertical() ->
    checkbox_group([{apple, <<"Apple">>}, {pear, <<"Pear">>}, {plum, <<"Plum">>},
                    {fig, <<"Fig (sold out)">>, [{disabled, true}]}],
                   [apple, plum], [], [{name, fruit}]).

-spec checkbox_group_layouts() -> aihtml:html().
checkbox_group_layouts() ->
    Days = [{mon, <<"Mon">>}, {tue, <<"Tue">>}, {wed, <<"Wed">>}, {thu, <<"Thu">>}, {fri, <<"Fri">>}],
    'div'([checkbox_group(Days, [mon, wed], [horizontal], [{name, days}]),
           checkbox_group(Days, [fri], [horizontal, label_before, sm], [{name, days2}])],
          [<<"flex flex-col gap-4">>], []).

-spec checkbox_group_disabled() -> aihtml:html().
checkbox_group_disabled() ->
    checkbox_group([{read, <<"Read">>}, {write, <<"Write">>}, {admin, <<"Admin">>}],
                   [read], [horizontal], [{name, perms}, {disabled, true}]).

%% --- radiobutton_group ---------------------------------------------

-spec radio_group_vertical() -> aihtml:html().
radio_group_vertical() ->
    radiobutton_group([{standard, <<"Standard shipping">>}, {express, <<"Express">>},
                       {pickup, <<"Pick up in store">>},
                       {drone, <<"Drone (coming soon)">>, [{disabled, true}]}],
                      express, [], [{name, shipping}]).

-spec radio_group_layouts() -> aihtml:html().
radio_group_layouts() ->
    Sizes = [{s, <<"S">>}, {m, <<"M">>}, {l, <<"L">>}, {xl, <<"XL">>}],
    'div'([radiobutton_group(Sizes, m, [horizontal], [{name, size}]),
           radiobutton_group(Sizes, l, [horizontal, label_before, lg], [{name, size2}])],
          [<<"flex flex-col gap-4">>], []).

-spec radio_group_disabled() -> aihtml:html().
radio_group_disabled() ->
    radiobutton_group([{yes, <<"Yes">>}, {no, <<"No">>}], yes, [horizontal],
                      [{name, agree}, {disabled, true}]).

%% --- radio_cards ---------------------------------------------------

-spec radio_cards_plans() -> aihtml:html().
radio_cards_plans() ->
    radio_cards([{free, <<"Free">>, #{description => <<"Personal trial, limited features">>}},
                 {pro, <<"Pro">>, #{description => <<"All features and priority support">>}},
                 {team, <<"Team">>, #{description => <<"Collaboration and permissions">>}},
                 {enterprise, <<"Enterprise">>, #{description => <<"Contact sales">>,
                                                   disabled => true}}],
                pro, [], [{name, plan}, {columns, 2}, {align, start}]).

-spec radio_cards_icons() -> aihtml:html().
radio_cards_icons() ->
    radio_cards([{card, <<"Card">>, #{icon => <<"💳"/utf8>>}},
                 {bank, <<"Bank transfer">>, #{icon => <<"🏦"/utf8>>}},
                 {cash, <<"Cash">>, #{icon => <<"💵"/utf8>>}}],
                card, [], [{name, payment}, {columns, 3}]).

-spec radio_cards_disabled() -> aihtml:html().
radio_cards_disabled() ->
    radio_cards([{monthly, <<"Monthly">>}, {yearly, <<"Yearly">>}], yearly, [],
                [{name, billing}, {columns, 2}, {disabled, true}]).

%% --- rating_group --------------------------------------------------

-spec rating_basic() -> aihtml:html().
rating_basic() ->
    rating_group(5, 3, [], [{name, stars}]).

-spec rating_half() -> aihtml:html().
rating_half() ->
    rating_group(5, 2.5, [], [{name, score}, {precision, 0.5}]).

-spec rating_sizes() -> aihtml:html().
rating_sizes() ->
    row([rating_group(5, 4, [sm], []),
         rating_group(5, 4, [md, primary], []),
         rating_group(5, 4, [lg, success], []),
         rating_group(10, 7, [sm, error], [])]).

-spec rating_readonly() -> aihtml:html().
rating_readonly() ->
    row([rating_group(5, 3.5, [], [{readonly, true}, {precision, 0.5}]),
         rating_group(5, 2, [], [{disabled, true}])]).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-6">>], []).
