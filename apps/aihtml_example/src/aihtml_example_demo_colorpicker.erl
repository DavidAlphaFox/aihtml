%% @doc Demos of the colour picker (aihtml_colorpicker), shown on
%% /components/colorpicker. Each function is one example, written the way
%% an application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_colorpicker).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([color_basic/0, color_swatches/0, color_alpha/0, color_states/0,
         color_inline/0, color_compact/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => colorpicker, title => <<"ColorPicker">>,
       summary => <<"HSV 取色面板加十六进制与 RGB 输入，值为 #rrggbb。"/utf8>>,
       demos => [{<<"弹出取色面板"/utf8>>, color_basic},
                 {<<"预设色板"/utf8>>, color_swatches},
                 {<<"透明度"/utf8>>, color_alpha},
                 {<<"空值、可清除与禁用"/utf8>>, color_states},
                 {<<"内嵌面板"/utf8>>, color_inline},
                 {<<"精简面板与自定义尺寸"/utf8>>, color_compact}]}].

-spec color_basic() -> aihtml:html().
color_basic() ->
    row([ah_colorpicker(<<"#3b82f6">>, [], [{name, brand}]),
         ah_colorpicker(<<"#F97316">>, [], [{name, accent}])]).

-spec color_swatches() -> aihtml:html().
color_swatches() ->
    ah_colorpicker(<<"#22c55e">>, [],
                   [{name, label_color},
                    {swatches, [<<"#ef4444">>, <<"#f97316">>, <<"#eab308">>, <<"#22c55e">>,
                                <<"#06b6d4">>, <<"#3b82f6">>, <<"#8b5cf6">>, <<"#ec4899">>]}]).

-spec color_alpha() -> aihtml:html().
color_alpha() ->
    row([ah_colorpicker(<<"#22c55e80">>, [alpha], [{name, overlay}]),
         ah_colorpicker({139, 92, 246, 200}, [inline, alpha], [])]).

-spec color_states() -> aihtml:html().
color_states() ->
    row([ah_colorpicker(undefined, [clearable], [{placeholder, <<"Pick a colour">>}]),
         ah_colorpicker(<<"#ec4899">>, [clearable], [{clear_label, <<"No colour">>}]),
         ah_colorpicker(<<"#f97316">>, [disabled], [])]).

-spec color_inline() -> aihtml:html().
color_inline() ->
    ah_colorpicker(<<"ff0000">>, [inline], [{name, highlight}]).

-spec color_compact() -> aihtml:html().
color_compact() ->
    row([ah_colorpicker(<<"#0ea5e9">>, [inline, no_inputs], [{width, 200}, {height, 120}]),
         ah_colorpicker(<<"#8b5cf6">>, [inline, no_preview], [{height, 100}])]).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-start gap-4">>], []).
