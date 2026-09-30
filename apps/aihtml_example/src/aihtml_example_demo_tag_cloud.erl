%% @doc Demos of the tag_cloud component (aihtml_tag_cloud), shown on
%% /components/tag_cloud. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_tag_cloud).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([tag_cloud_weights/0, tag_cloud_gradient/0, tag_cloud_values/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => tag_cloud, title => <<"TagCloud">>,
       summary => <<"按权重调字号的标签云。"/utf8>>,
       demos => [{<<"按权重调字号"/utf8>>, tag_cloud_weights},
                 {<<"颜色渐变与排序"/utf8>>, tag_cloud_gradient},
                 {<<"显示权重、大小写、取前 N 个"/utf8>>, tag_cloud_values}]}].

%%% TagCloud

-spec tag_cloud_weights() -> aihtml:html().
tag_cloud_weights() ->
    ah_tag_cloud([{<<"Erlang">>, 40}, {<<"jQuery">>, 25}, {<<"CSS">>, 15}, {<<"sigil">>, 30},
                  #{label => <<"OTP">>, value => 35, url => <<"#otp">>},
                  {<<"html">>, 8}, {<<"Tailwind">>, 20}],
                 [], []).

-spec tag_cloud_gradient() -> aihtml:html().
tag_cloud_gradient() ->
    ah_tag_cloud([{<<"Erlang">>, 40}, {<<"jQuery">>, 25}, {<<"CSS">>, 15}, {<<"sigil">>, 30},
                  {<<"OTP">>, 35}, {<<"html">>, 8}, {<<"Tailwind">>, 20}],
                 [], [{min_color, <<"#93c5fd">>}, {max_color, <<"#1e3a8a">>}, {max_font_size, 32},
                      {sort_by, value}, {sort_order, descending}]).

-spec tag_cloud_values() -> aihtml:html().
tag_cloud_values() ->
    ah_tag_cloud([{<<"erlang">>, 40}, {<<"jquery">>, 25}, {<<"css">>, 15}, {<<"sigil">>, 30},
                  {<<"otp">>, 35}, {<<"html">>, 8}],
                 [], [{display_value, true}, {text_case, first_upper},
                      {display_limit, 4}, {take_top_weighted, true}]).
