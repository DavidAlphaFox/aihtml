%% @doc Demos of the progressbar component (aihtml_progressbar), shown on
%% /components/progressbar. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_progressbar).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([progress_values/0, progress_styles/0, progress_ranges/0, progress_vertical/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => progressbar, title => <<"Progressbar">>,
       summary => <<"线性进度条，支持文字、颜色分段、条纹与不确定状态。"/utf8>>,
       demos => [{<<"数值与文字"/utf8>>, progress_values},
                 {<<"颜色、条纹、不确定"/utf8>>, progress_styles},
                 {<<"颜色分段"/utf8>>, progress_ranges},
                 {<<"竖向与反向"/utf8>>, progress_vertical}]}].

%%% Progressbar

-spec progress_values() -> aihtml:html().
progress_values() ->
    stack([progressbar(35, [show_text], []),
           progressbar(9, [show_text], [{max, 10}, {text, <<"9 of 10 files">>}]),
           progressbar(60, [], [{aria_label, <<"Upload">>}])]).

-spec progress_styles() -> aihtml:html().
progress_styles() ->
    stack([progressbar(70, [success, striped, animated, show_text], []),
           progressbar(45, [warning, striped], []),
           progressbar(undefined, [indeterminate, info], [{aria_label, <<"Loading">>}]),
           progressbar(50, [disabled, show_text], [])]).

-spec progress_ranges() -> aihtml:html().
progress_ranges() ->
    progressbar(80, [show_text], [{color_ranges, [{30, success}, {60, warning}, {100, error}]}]).

-spec progress_vertical() -> aihtml:html().
progress_vertical() ->
    row([progressbar(30, [vertical, show_text], [{style, <<"height: 120px">>}]),
         progressbar(60, [vertical, reverse, success, show_text], [{style, <<"height: 120px">>}]),
         'div'(progressbar(40, [reverse, error], []), [<<"flex-1">>], [])]).

%% Layout helpers of the demos.
row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-4">>], []).

stack(Children) ->
    'div'(Children, [<<"flex flex-col gap-3">>], []).
