%% @doc Demos of the aspect_ratio component (aihtml_aspect_ratio), shown on
%% /components/aspect_ratio. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_aspect_ratio).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([aspect_ratios/0, aspect_ratio_image/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => aspect_ratio, title => <<"AspectRatio">>,
       summary => <<"按固定宽高比约束内容。"/utf8>>,
       demos => [{<<"常用比例"/utf8>>, aspect_ratios},
                 {<<"图片铺满"/utf8>>, aspect_ratio_image}]}].

%%% AspectRatio

-spec aspect_ratios() -> aihtml:html().
aspect_ratios() ->
    row(['div'(aspect_ratio('div'(R, [<<"h-full flex items-center justify-center "
                                         "bg-primary/15 text-primary">>], []),
                            [], [{ratio, R}]),
               [<<"w-48">>], [])
         || R <- [<<"16/9">>, <<"4:3">>, <<"1/1">>]]).

-spec aspect_ratio_image() -> aihtml:html().
aspect_ratio_image() ->
    Sky = <<"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 4 3'>"
            "<rect width='4' height='3' fill='%2393c5fd'/><circle cx='3' cy='1' r='.5' fill='%23fde047'/>"
            "<path d='M0 3 1.5 1.5 3 3z' fill='%2322c55e'/></svg>">>,
    'div'(aspect_ratio(img([], [{src, Sky}, {alt, <<"Landscape">>}]), [], [{ratio, {21, 9}}]),
          [<<"max-w-md">>], []).

%% Layout helpers of the demos.
row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-4">>], []).
