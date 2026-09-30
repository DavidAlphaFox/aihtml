%% @doc Demos of the tag input (aihtml_tag_input), shown on
%% /components/tag_input. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_tag_input).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([tags_basic/0, tags_limits/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => tag_input, title => <<"TagInput">>,
       summary => <<"标签输入框，回车或逗号添加，退格删除末项。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, tags_basic},
                 {<<"数量上限、颜色与禁用"/utf8>>, tags_limits}]}].

-spec tags_basic() -> aihtml:html().
tags_basic() ->
    ah_tag_input([<<"erlang">>, <<"stimulus">>, <<"tailwind">>], [<<"w-96">>], [{name, tags}]).

-spec tags_limits() -> aihtml:html().
tags_limits() ->
    ah_div([ah_tag_input([<<"red">>], [<<"w-96">>], [{max_tags, 3}, {chip_color, error},
                                                     {chip_variant, filled},
                                                     {placeholder, <<"Up to 3 tags">>}]),
            ah_tag_input([<<"locked">>], [disabled, <<"w-96">>], [])],
           [<<"flex flex-col gap-3">>], []).
