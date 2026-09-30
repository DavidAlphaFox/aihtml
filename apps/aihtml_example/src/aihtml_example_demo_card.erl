%% @doc Demos of card (aihtml_card), shown on /components/card. Each
%% function is one example, written the way an application writes it;
%% the docs page prints its source under it.
-module(aihtml_example_demo_card).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_layout, [row/1]).

-export([demos/0]).
-export([card_basic/0, card_header/0, card_media/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => card, title => <<"Card">>,
       summary => <<"带标题、内容和底部的卡片容器。"/utf8>>,
       demos => [{<<"标题与正文"/utf8>>, card_basic},
                 {<<"副标题、右侧操作、底部、悬停"/utf8>>, card_header},
                 {<<"媒体区与无内边距正文"/utf8>>, card_media}]}].

-spec card_basic() -> aihtml:html().
card_basic() ->
    ah_div(ah_card(ah_p(<<"Orders ship within two business days.">>), [],
                   [{title, <<"Shipping">>}]),
           [<<"w-72">>], []).

-spec card_header() -> aihtml:html().
card_header() ->
    ah_div(ah_card(ah_p(<<"3 open issues, 12 closed this week.">>), [hover],
                   [{title, <<"Project Atlas">>}, {subtitle, <<"Updated today">>},
                    {extra, ah_button(<<"Edit">>, edit, [outlined, sm], [])},
                    {footer, ah_small(<<"Owner: Lin">>)}]),
           [<<"w-80">>], []).

-spec card_media() -> aihtml:html().
card_media() ->
    row([ah_card(ah_p(<<"A card with a media strip.">>), [],
                 [{media, ah_div([], [<<"h-24 bg-gradient-to-r from-sky-400 to-indigo-500">>], [])},
                  {title, <<"Media">>}]),
         ah_card(ah_ul([ah_li(<<"Inbox">>, [<<"px-4 py-2 border-b border-line">>], []),
                        ah_li(<<"Archive">>, [<<"px-4 py-2">>], [])]),
                 [flush], [{title, <<"Flush body">>}])]).
