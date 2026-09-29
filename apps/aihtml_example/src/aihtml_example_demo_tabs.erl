%% @doc Demos of tabs (aihtml_tabs), shown on /components/tabs. Each
%% function is one example, written the way an application writes it;
%% the docs page prints its source under it.
-module(aihtml_example_demo_tabs).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_layout, [row/1, col/1, box/1]).

-export([demos/0]).
-export([tabs_basic/0, tabs_positions/0, tabs_hover/0, tabs_scrollable/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => tabs, title => <<"Tabs">>,
       summary => <<"切换多块内容的标签页，值为当前标签的键。"/utf8>>,
       demos => [{<<"基本用法与禁用标签"/utf8>>, tabs_basic},
                 {<<"标签在下、左、右"/utf8>>, tabs_positions},
                 {<<"悬停切换、无动画"/utf8>>, tabs_hover},
                 {<<"可滚动的标签栏"/utf8>>, tabs_scrollable}]}].

-spec tabs_basic() -> aihtml:html().
tabs_basic() ->
    tabs([{overview, <<"Overview">>, p(<<"Product overview.">>)},
          {specs, <<"Specs">>, p(<<"Weight 1.2 kg, 13 inch display.">>)},
          {reviews, <<"Reviews">>, p(<<"No reviews yet.">>), #{disabled => true}},
          {faq, <<"FAQ">>, p(<<"Questions and answers.">>)}],
         specs, [], [{name, section}]).

-spec tabs_positions() -> aihtml:html().
tabs_positions() ->
    Tabs = [{a, <<"Mail">>, p(<<"Inbox">>)}, {b, <<"Calendar">>, p(<<"Today">>)},
            {c, <<"Contacts">>, p(<<"People">>)}],
    col([tabs(Tabs, a, [bottom], []),
         row([box(tabs(Tabs, b, [left], [])),
              box(tabs(Tabs, c, [right], []))])]).

-spec tabs_hover() -> aihtml:html().
tabs_hover() ->
    tabs([{day, <<"Day">>, p(<<"Hourly view.">>)}, {week, <<"Week">>, p(<<"Seven days.">>)},
          {month, <<"Month">>, p(<<"Whole month.">>)}],
         week, [], [{selection_mode, hover}, {animation, none}]).

-spec tabs_scrollable() -> aihtml:html().
tabs_scrollable() ->
    'div'(tabs([{I, <<"Document ", (integer_to_binary(I))/binary>>,
                 p(<<"Contents of document ", (integer_to_binary(I))/binary>>)}
                || I <- lists:seq(1, 10)],
               1, [], [{scrollable, true}]),
          [<<"w-96">>], []).
