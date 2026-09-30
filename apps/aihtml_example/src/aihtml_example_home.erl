%% @doc GET /: the landing page, in the spirit of sigil's demo index.
-module(aihtml_example_home).

-include_lib("aihtml/include/aihtml.hrl").

-export([init/2, render/0]).

-define(SITE, aihtml_example_site).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, State) ->
    {ok, ?SITE:reply(Req, <<"aihtml — Erlang UI"/utf8>>, fun page/0, #{}), State}.

%% @doc The landing page, as html (for tests and tools).
-spec render() -> aihtml:html().
render() -> page().

page() ->
    Comps = ?SITE:components(),
    ah_div([?SITE:topbar(home), hero(Comps), components(Comps), features(),
            ah_footer(ah_p(<<"aihtml · Apache-2.0 · components ported from sigil (MIT)">>,
                           [<<"text-center text-sm text-muted">>], []),
                      [<<"py-10 border-t border-line">>], [])],
           [<<"min-h-screen">>], []).

hero(Comps) ->
    {_, _, Palettes, _} = lists:keyfind(palette, 1, aihtml_theme:axes()),
    Stats = [{length(Comps), <<"组件"/utf8>>},
             {length(Palettes), <<"配色"/utf8>>},
             {length(aihtml_theme:axes()), <<"主题轴"/utf8>>},
             {length(aihtml_tpl:names()), <<"共享模板"/utf8>>}],
    ah_section(ah_div([ah_h1(<<"aihtml">>, [<<"text-6xl font-bold tracking-tight">>], []),
                       ah_p(<<"用 Erlang 函数直接写 HTML 页面。组件由 Stimulus 控制器增强、Tailwind 定样式，"
                              "交互走无状态 action，服务端可以向页面推送更新。"/utf8>>,
                            [<<"mt-6 text-lg text-white/80 max-w-2xl mx-auto leading-relaxed">>], []),
                       ah_div([stat(N, L) || {N, L} <- Stats],
                              [<<"mt-10 flex justify-center gap-12 flex-wrap">>], []),
                       ah_div([ah_link_button(<<"浏览组件"/utf8>>, <<"/components">>, [lg], []),
                               ah_link_button(<<"实时演示"/utf8>>, <<"/demo">>, [lg, outlined], [])],
                              [<<"mt-10 flex justify-center gap-4">>], [])],
                      [<<"max-w-4xl mx-auto px-6 py-24 text-center">>], []),
               [<<"hero text-white">>], []).

stat(N, Label) ->
    ah_div([ah_p(N, [<<"text-4xl font-bold text-primary">>], []),
            ah_p(Label, [<<"mt-1 text-xs tracking-widest uppercase text-white/70">>], [])], [], []).

components(Comps) ->
    ah_section(ah_div([ah_h2(<<"组件"/utf8>>, [<<"text-3xl font-bold text-center mb-10">>], []),
                       [category(C, [E || #{category := Cat} = E <- Comps, Cat =:= C])
                        || C <- ?SITE:categories()]],
                      [<<"max-w-6xl mx-auto px-6 py-16">>], []),
               [], []).

category(_, []) -> [];
category(C, Entries) ->
    ah_div([ah_h3(?SITE:category_label(C), [<<"text-lg font-semibold pb-3 mb-5 border-b border-line">>], []),
            ah_div([component_card(E) || E <- Entries],
                   [<<"grid gap-4 sm:grid-cols-2 lg:grid-cols-3">>], [])],
           [<<"mb-12">>], []).

component_card(#{name := N} = E) ->
    Name = ?SITE:display_name(N),
    ah_a([ah_div(binary:part(Name, 0, 1),
                 [<<"w-10 h-10 shrink-0 grid place-items-center rounded-control font-bold"
                    " bg-primary-lighter text-primary-dark">>], []),
          ah_div([ah_p(Name, [<<"font-semibold">>], []),
                  ah_p(?SITE:summary(E), [<<"text-sm text-muted line-clamp-2">>], [])],
                 [<<"min-w-0">>], [])],
         [<<"component-card flex gap-4 items-start p-4 rounded-surface bg-surface border border-line">>],
         [{href, <<"/components/", (atom_to_binary(N))/binary>>}]).

features() ->
    F = [{<<"Erlang 写页面"/utf8>>,
          <<"ah_div([ah_button(...)], Css, Attrs) 这样的函数调用就是页面，渲染时统一转义，可组合、可测试。"/utf8>>},
         {<<"无状态 action"/utf8>>,
          <<"每个事件一次签名请求，响应是要应用的 DOM 操作。任意节点都能处理，重启不影响已打开的页面。"/utf8>>},
         {<<"服务端推送"/utf8>>,
          <<"页面订阅签名主题，pg 分发到整个集群，断线重连后自动补齐。"/utf8>>},
         {<<"四轴主题"/utf8>>,
          <<"外观、配色、排版、外形四个轴互不干扰，切换任意一个，所有组件一起变。"/utf8>>}],
    ah_section(ah_div([ah_h2(<<"特性"/utf8>>, [<<"text-3xl font-bold text-center mb-10">>], []),
                       ah_div([ah_card(ah_p(Text, [<<"text-sm text-muted leading-relaxed">>], []),
                                       [], [{title, Title}]) || {Title, Text} <- F],
                              [<<"grid gap-6 md:grid-cols-2">>], [])],
                      [<<"max-w-6xl mx-auto px-6 py-16">>], []),
               [<<"bg-surface-2">>], []).
