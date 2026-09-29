%% @doc Demos of expander (aihtml_expander), shown on /components/expander. Each
%% function is one example, written the way an application writes it;
%% the docs page prints its source under it.
-module(aihtml_example_demo_expander).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_layout, [col/1]).

-export([demos/0]).
-export([expander_basic/0, expander_structured/0, expander_icons/0, expander_styles/0, expander_accordion/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => expander, title => <<"Expander">>,
       summary => <<"点击标题展开或收起一块内容，值为 true 或 false。"/utf8>>,
       demos => [{<<"展开与收起"/utf8>>, expander_basic},
                 {<<"结构化标题与左侧箭头"/utf8>>, expander_structured},
                 {<<"加减号图标与淡入动画"/utf8>>, expander_icons},
                 {<<"标题在下、无边距、禁用"/utf8>>, expander_styles},
                 {<<"手风琴：同名的只展开一个"/utf8>>, expander_accordion}]}].

-spec expander_basic() -> aihtml:html().
expander_basic() ->
    col([expander(p(<<"Returns are free within 30 days.">>), [],
                  [{header, <<"Return policy">>}]),
         expander(p(<<"We ship worldwide.">>), [],
                  [{header, <<"Shipping">>}, {expanded, false},
                   {actions, button(<<"Contact us">>, contact, [outlined, sm], [])}])]).

-spec expander_structured() -> aihtml:html().
expander_structured() ->
    expander(p(<<"Invoice details.">>), [],
             [{header, #{title => <<"Invoice #1024">>, subheader => <<"Due in 5 days">>,
                         extra => <<"$320.00">>}},
              {arrow_position, left}, {expanded, false}]).

-spec expander_icons() -> aihtml:html().
expander_icons() ->
    expander(p(<<"Plus and minus swap places.">>), [square],
             [{header, <<"More options">>}, {expand_icon, <<"+">>},
              {collapse_icon, <<"−"/utf8>>}, {animation, fade}, {expanded, false}]).

-spec expander_styles() -> aihtml:html().
expander_styles() ->
    col([expander(p(<<"The header sits below.">>), [bottom],
                  [{header, <<"Header at the bottom">>}, {expanded, false}]),
         expander(p(<<"No frame around it.">>), [no_gutters],
                  [{header, <<"No gutters">>}, {expanded, false}]),
         expander(p(<<"Hidden">>), [disabled],
                  [{header, <<"Disabled">>}, {expanded, false}])]).

-spec expander_accordion() -> aihtml:html().
expander_accordion() ->
    col([expander(p(<<"Create an account first.">>), [],
                  [{header, <<"How do I start?">>}, {accordion, faq}]),
         expander(p(<<"Yes, any time from settings.">>), [],
                  [{header, <<"Can I cancel?">>}, {accordion, faq}, {expanded, false}]),
         expander(p(<<"Email support@example.com.">>), [],
                  [{header, <<"Where is support?">>}, {accordion, faq}, {expanded, false}])]).
