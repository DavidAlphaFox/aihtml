%% @doc Demos of breadcrumbs (aihtml_breadcrumbs), shown on /components/breadcrumbs. Each
%% function is one example, written the way an application writes it;
%% the docs page prints its source under it.
-module(aihtml_example_demo_breadcrumbs).

-include_lib("aihtml/include/aihtml.hrl").

-import(aihtml_example_fixture_layout, [col/1]).

-export([demos/0]).
-export([breadcrumbs_basic/0, breadcrumbs_separators/0, breadcrumbs_collapsed/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => breadcrumbs, title => <<"Breadcrumbs">>,
       summary => <<"层级路径导航，末项是当前页。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, breadcrumbs_basic},
                 {<<"分隔符与图标"/utf8>>, breadcrumbs_separators},
                 {<<"折叠中间项、末项可点"/utf8>>, breadcrumbs_collapsed}]}].

-spec breadcrumbs_basic() -> aihtml:html().
breadcrumbs_basic() ->
    breadcrumbs([{<<"Home">>, <<"/">>}, {<<"Users">>, <<"/users">>}, <<"Lin">>], [], []).

-spec breadcrumbs_separators() -> aihtml:html().
breadcrumbs_separators() ->
    col([breadcrumbs([{<<"Home">>, <<"/">>}, {<<"Library">>, <<"/lib">>}, <<"Data">>], [],
                     [{separator, none}]),
         breadcrumbs([#{label => <<"Home">>, href => <<"/">>, icon => <<"⌂"/utf8>>},
                      {<<"Docs">>, <<"/docs">>}, <<"Guide">>], [],
                     [{separator, <<"›"/utf8>>}])]).

-spec breadcrumbs_collapsed() -> aihtml:html().
breadcrumbs_collapsed() ->
    col([breadcrumbs([{<<"Root">>, <<"#">>}, {<<"A">>, <<"#">>}, {<<"B">>, <<"#">>},
                      {<<"C">>, <<"#">>}, {<<"D">>, <<"#">>}, <<"Current">>], [],
                     [{max_items, 4}]),
         breadcrumbs([{<<"Home">>, <<"/">>}, {<<"Reports">>, <<"/reports">>}], [],
                     [{active_last, true}])]).
