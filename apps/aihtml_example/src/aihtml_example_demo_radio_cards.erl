%% @doc Demos of the radio cards (aihtml_radio_cards), shown on
%% /components/radio_cards. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_radio_cards).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([radio_cards_plans/0, radio_cards_icons/0, radio_cards_disabled/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => radio_cards, title => <<"RadioCards">>,
       summary => <<"卡片式单选，每项可带说明和图标。"/utf8>>,
       demos => [{<<"套餐选择"/utf8>>, radio_cards_plans},
                 {<<"图标、三列"/utf8>>, radio_cards_icons},
                 {<<"禁用"/utf8>>, radio_cards_disabled}]}].

-spec radio_cards_plans() -> aihtml:html().
radio_cards_plans() ->
    ah_radio_cards([{free, <<"Free">>, #{description => <<"Personal trial, limited features">>}},
                    {pro, <<"Pro">>, #{description => <<"All features and priority support">>}},
                    {team, <<"Team">>, #{description => <<"Collaboration and permissions">>}},
                    {enterprise, <<"Enterprise">>, #{description => <<"Contact sales">>,
                                                      disabled => true}}],
                   pro, [], [{name, plan}, {columns, 2}, {align, start}]).

-spec radio_cards_icons() -> aihtml:html().
radio_cards_icons() ->
    ah_radio_cards([{card, <<"Card">>, #{icon => <<"💳"/utf8>>}},
                    {bank, <<"Bank transfer">>, #{icon => <<"🏦"/utf8>>}},
                    {cash, <<"Cash">>, #{icon => <<"💵"/utf8>>}}],
                   card, [], [{name, payment}, {columns, 3}]).

-spec radio_cards_disabled() -> aihtml:html().
radio_cards_disabled() ->
    ah_radio_cards([{monthly, <<"Monthly">>}, {yearly, <<"Yearly">>}], yearly, [],
                   [{name, billing}, {columns, 2}, {disabled, true}]).
