%% @doc Demos of the SideNav component (aihtml_sidenav), shown on
%% /components/sidenav. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_sidenav).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([sidenav_groups/0, sidenav_collapsed/0, sidenav_links/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => sidenav, title => <<"SideNav">>,
       summary => <<"应用左侧导航：品牌区、分组树、底部插槽，可收窄。"/utf8>>,
       demos => [{<<"分组与当前项"/utf8>>, sidenav_groups},
                 {<<"收窄为图标"/utf8>>, sidenav_collapsed},
                 {<<"路由前缀生成链接"/utf8>>, sidenav_links}]}].

%%%-------------------------------------------------------------------
%%% sidenav
%%%-------------------------------------------------------------------

-spec sidenav_groups() -> aihtml:html().
sidenav_groups() ->
    ah_div([ah_sidenav([#{label => <<"Overview">>,
                          items => [{dashboard, <<"Dashboard">>}, {analytics, <<"Analytics">>}]},
                        #{label => <<"Management">>,
                          items => [#{key => users, label => <<"Users">>,
                                      children => [{user_list, <<"List">>}, {roles, <<"Roles">>}]},
                                    {settings, <<"Settings">>}]}],
                       roles, [],
                       [{brand, #{name => <<"Sigil">>, logo => ah_strong(<<"S">>)}},
                        {footer, ah_small(<<"v1.0">>, [<<"text-muted">>], [])},
                        {collapsible, true},
                        {style, <<"--ah-ssn-height:100%">>}]),
            ah_div(<<"Content">>, [<<"p-4 text-sm text-muted">>], [])],
           [<<"flex h-[420px] border border-line rounded overflow-hidden">>], []).

-spec sidenav_collapsed() -> aihtml:html().
sidenav_collapsed() ->
    ah_div(ah_sidenav([#{key => dashboard, label => <<"Dashboard">>, icon => ah_span(<<"▦"/utf8>>)},
                       #{key => inbox, label => <<"Inbox">>, icon => ah_span(<<"✉"/utf8>>)},
                       #{key => settings, label => <<"Settings">>, icon => ah_span(<<"⚙"/utf8>>)}],
                      inbox, [collapsed],
                      [{brand, #{logo => ah_strong(<<"S">>), name => <<"Sigil">>}},
                       {collapsible, true},
                       {style, <<"--ah-ssn-height:100%">>}]),
           [<<"flex h-[300px] border border-line rounded overflow-hidden">>], []).

-spec sidenav_links() -> aihtml:html().
sidenav_links() ->
    ah_div(ah_sidenav([{getting_started, <<"Getting started">>},
                       {install, <<"Install">>},
                       #{key => github, label => <<"GitHub">>, href => <<"https://github.com">>,
                         target => <<"_blank">>}],
                      install, [],
                      [{route_prefix, <<"#/docs/">>}, {style, <<"--ah-ssn-height:auto">>}]),
           [<<"flex border border-line rounded overflow-hidden">>], []).
