%% @doc GET /components/:name: one page per component, laid out like
%% sigil's docs -- category navigation on the left; tabs for the live
%% demos (each with its own source), the full demo code, and the API taken
%% from the component catalog.
-module(aihtml_example_docs).

-include_lib("aihtml/include/aihtml.hrl").

-export([init/2, render/1]).

-define(SITE, aihtml_example_site).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req0, State) ->
    Comps = ?SITE:components(),
    Name = cowboy_req:binding(name, Req0),
    case find(Name, Comps) of
        {ok, Entry} ->
            Title = <<(?SITE:display_name(maps:get(name, Entry)))/binary, " — aihtml"/utf8>>,
            {ok, ?SITE:reply(Req0, Title, page(Entry, Comps), #{}), State};
        error when Name =:= undefined ->
            #{name := First} = hd(Comps),
            {ok, cowboy_req:reply(302, #{<<"location">> => <<"/components/", (atom_to_binary(First))/binary>>},
                                  Req0), State};
        error ->
            {ok, cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>},
                                  <<"no such component">>, Req0), State}
    end.

%% @doc The page of one component, as html (for tests and tools).
-spec render(atom()) -> aihtml:html().
render(Name) ->
    Comps = ?SITE:components(),
    {ok, Entry} = find(atom_to_binary(Name), Comps),
    page(Entry, Comps).

find(undefined, _) -> error;
find(Name, Comps) ->
    case [E || #{name := N} = E <- Comps, atom_to_binary(N) =:= Name] of
        [E] -> {ok, E};
        [] -> error
    end.

page(#{name := Name} = Entry, Comps) ->
    'div'([sidebar(Name, Comps),
           main(['div'([header_bar(Entry),
                        tabs([{demo, <<"演示"/utf8>>, demos(Name)},
                              {code, <<"代码"/utf8>>, code_tab(Name)},
                              {api, <<"API">>, api(Entry)}],
                             demo, [<<"mt-6">>], [{id, <<"doc-tabs">>}])],
                       [<<"max-w-5xl mx-auto px-8 py-8">>], [])],
                [<<"flex-1 min-w-0">>], [])],
          [<<"flex min-h-screen">>], []).

%%%===================================================================
%%% Navigation
%%%===================================================================

sidebar(Active, Comps) ->
    Groups = [#{label => ?SITE:category_label(C),
                items => [#{key => N, label => ?SITE:display_name(N)}
                          || #{name := N, category := Cat} <- Comps, Cat =:= C]}
              || C <- ?SITE:categories()],
    %% sidenav is itself the sticky, full-height sidebar; only its width
    %% is set, through its own custom property.
    sidenav(Groups, Active, [<<"docs-nav shrink-0">>],
            [{brand, #{name => <<"aihtml">>, href => <<"/">>}},
             {route_prefix, <<"/components/">>},
             {style, <<"--ah-ssn-width:15rem">>}]).

header_bar(#{name := Name, signature := Sig, category := Cat} = E) ->
    'div'(['div'(['div'([chip(<<"组件"/utf8>>, [primary, soft, small], []),
                         chip(?SITE:category_label(Cat), [soft, small], [])],
                        [<<"flex gap-2">>], []),
                  h1(?SITE:display_name(Name), [<<"mt-2 text-3xl font-bold">>], []),
                  p(maps:get(doc, E), [<<"mt-2 text-muted leading-relaxed">>], []),
                  code(Sig, [<<"mt-3 inline-block font-mono text-sm px-2 py-1 rounded-control bg-surface-2">>], [])],
                 %% flex-1: the title block takes the free width and the
                 %% description wraps inside it. Below lg it keeps at least
                 %% 20rem, so on narrow screens the switcher moves under it
                 %% instead of squeezing the text into a thin column.
                 [<<"flex-1 min-w-[20rem] lg:min-w-0">>], []),
           %% a 2 x 2 grid is half as wide as the default row of four
           theme_switcher([<<"shrink-0 grid grid-cols-2 gap-x-3 gap-y-2">>], [])],
          %% side by side from lg up; on narrow screens the switcher goes below
          [<<"flex flex-wrap lg:flex-nowrap gap-6 justify-between items-start pb-6 border-b border-line">>], []).

%%%===================================================================
%%% Demos and code
%%%===================================================================

demos(Name) ->
    case aihtml_example_demos:for(Name) of
        [] -> empty(<<"这个组件还没有示例。"/utf8>>, [], []);
        Demos -> [demo(D) || D <- Demos]
    end.

demo({Title, Mod, Fun}) ->
    section([h3(Title, [<<"text-base font-semibold mb-3">>], []),
             'div'(Mod:Fun(), [<<"demo-stage p-5 rounded-surface border border-line bg-surface">>], []),
             source_block(aihtml_example_source:function(Mod, Fun))],
            [<<"mb-10">>], []).

code_tab(Name) ->
    case aihtml_example_demos:for(Name) of
        [] -> empty(<<"没有代码。"/utf8>>, [], []);
        Demos ->
            [Mod | _] = [M || {_, M, _} <- Demos],
            [p([<<"下面是 "/utf8>>, code(atom_to_binary(Mod), [<<"font-mono">>], []),
                <<" 中这个组件的全部示例函数，页面上的每个示例都由它们渲染。"/utf8>>],
               [<<"text-sm text-muted mb-4">>], []),
             source_block(iolist_to_binary(
                            lists:join(<<"\n">>, [aihtml_example_source:function(M, F)
                                                  || {_, M, F} <- Demos])))]
    end.

source_block(Src) ->
    'div'([span(<<"Erlang">>, [<<"source-lang">>], []),
           pre(code(aihtml_example_source:highlight(Src), [], []), [<<"source-code">>], [])],
          [<<"source-block mt-3">>], []).

%%%===================================================================
%%% API
%%%===================================================================

api(#{name := Name, root := Root, behavior := Behavior, events := Events} = E) ->
    OptionDocs = maps:get(option_docs, E, #{}),
    Methods = maps:get(methods, E, []),
    ['div'([h3(<<"签名"/utf8>>, [<<"api-h">>], []),
            code(maps:get(signature, E), [<<"font-mono">>], []),
            p([<<"函数写法的最后两个参数固定为 Css 和 Attrs。Css 里的原子是下表的修饰符，binary 是字面类名（通常是 Tailwind 工具类）；"
                 "Attrs 是 HTML 属性，其中下表列出的键是组件选项。"/utf8>>],
              [<<"mt-2 text-sm text-muted">>], [])], [], []),
     record_section(aihtml_example_records:record(Name), E),
     modifiers(E),
     table_section(<<"选项（Attrs 中的键）"/utf8>>, [<<"名称"/utf8>>, <<"说明"/utf8>>],
                   [[code(atom_to_binary(O), [<<"api-name">>], []), maps:get(O, OptionDocs, <<"—"/utf8>>)]
                    || O <- maps:get(options, E)]),
     table_section(<<"事件"/utf8>>, [<<"事件"/utf8>>, <<"用法"/utf8>>],
                   [[code(Ev, [<<"api-name">>], []),
                     code([<<"on(">>, event_atom(Ev), <<", {Mod, Action, Args})">>], [<<"font-mono text-xs">>], [])]
                    || Ev <- Events]),
     table_section(<<"方法"/utf8>>, [<<"名称"/utf8>>, <<"参数"/utf8>>, <<"说明"/utf8>>],
                   [[code(atom_to_binary(M), [<<"api-name">>], []),
                     code(maps:get(args, Md, <<>>), [<<"font-mono text-xs">>], []),
                     maps:get(doc, Md, <<>>)]
                    || #{name := M} = Md <- Methods]),
     [p([<<"服务端在 action 里调用："/utf8>>,
         code(<<"aihtml_action:call(Ctx, {id, Id}, Method, Args)">>, [<<"font-mono">>], []),
         <<"；浏览器里调用："/utf8>>,
         code(<<"AH.invoke(el, method, ...args)">>, [<<"font-mono">>], [])],
        [<<"text-sm text-muted -mt-4 mb-8">>], []) || Methods =/= []],
     table_section(<<"CSS 与行为"/utf8>>, [<<"项目"/utf8>>, <<"值"/utf8>>],
                   [[<<"根类名"/utf8>>, code(<<".", Root/binary>>, [<<"api-name">>], [])],
                    [<<"行为（data-ah）"/utf8>>, case Behavior of
                                                   none -> <<"无"/utf8>>;
                                                   B -> code(B, [<<"api-name">>], [])
                                               end],
                    [<<"函数"/utf8>>, code([<<"aihtml:">>, atom_to_binary(Name), <<"/">>,
                                           integer_to_binary(arity(E))], [<<"api-name">>], [])]])].

%% The component's element record: the same component written with named
%% fields (designs/05-records.md).
record_section(undefined, _E) -> [];
record_section(#{record := Rec, header := Header, doc := Doc, fields := Fields}, E) ->
    #{groups := Groups} = E,
    Docs = maps:get(option_docs, E, #{}),
    Rows = [[code(atom_to_binary(F), [<<"api-name">>], []),
             code(T, [<<"font-mono text-xs">>], []),
             code(D, [<<"font-mono text-xs">>], []),
             case {Docs, maps:is_key(F, Groups)} of
                 {#{F := Text}, _} -> Text;
                 {_, true} -> <<"修饰符组，取值见下方修饰符表"/utf8>>;
                 _ -> <<"—"/utf8>>
             end]
            || #{name := F, type := T, default := D} <- Fields],
    'div'([h3(<<"record 写法"/utf8>>, [<<"api-h">>], []),
           p([<<"页面模块 include "/utf8>>, code(<<"aihtml.hrl">>, [<<"font-mono">>], []),
              <<" 后可以直接写 "/utf8>>,
              code([<<"#">>, atom_to_binary(Rec), <<"{}">>], [<<"font-mono">>], []),
              <<"（定义在 "/utf8>>, code(Header, [<<"font-mono">>], []),
              <<"），与上面的函数写法得到同一个元素。修饰符、标志和选项都是字段："
                "字段名写错会编译失败，取值由 dialyzer 和渲染时检查；没写的字段取默认值。"
                "类型里 ah_ 开头的名字也定义在这个头文件中。"/utf8>>],
             [<<"mt-2 text-sm text-muted">>], []),
           [p(doc_text(Doc), [<<"mt-2 text-sm">>], []) || Doc =/= <<>>],
           table([thead(tr([th(H, [], []) || H <- [<<"字段"/utf8>>, <<"类型"/utf8>>,
                                                    <<"默认值"/utf8>>, <<"说明"/utf8>>]])),
                  tbody([tr([td(C, [], []) || C <- Row]) || Row <- Rows])],
                 [<<"api-table mt-3">>], []),
           p([<<"所有 record 还有公共字段 "/utf8>>,
              lists:join(<<"、"/utf8>>, [code(atom_to_binary(F), [<<"font-mono">>], [])
                                         || F <- aihtml_example_records:base_fields()]),
              <<"：css 只放字面类名，attrs 是 HTML 属性，postback 写成 "/utf8>>,
              code(<<"Action | {Action, Args}">>, [<<"font-mono">>], []),
              <<"，调用当前模块的 action/4。"/utf8>>],
             [<<"mt-2 text-sm text-muted">>], [])],
          [<<"mb-8">>], []).

%% Header comments quote names as `name'; show those as code.
doc_text(Doc) ->
    Parts = re:split(Doc, <<"`([^`']*)'">>, [{return, binary}]),
    doc_parts(Parts, text).

doc_parts([], _) -> [];
doc_parts([P | Rest], text) -> [P | doc_parts(Rest, code)];
doc_parts([P | Rest], code) -> [code(P, [<<"font-mono">>], []) | doc_parts(Rest, text)].

modifiers(#{groups := Groups, flags := Flags} = E) ->
    Docs = maps:get(option_docs, E, #{}),
    Rows = [[code(atom_to_binary(G), [<<"api-name">>], []),
             [[code(atom_to_binary(V), [<<"font-mono text-xs">>, [<<"font-bold">> || V =:= Default]], []),
               <<" ">>] || V <- Vs],
             case Default of none -> <<"—"/utf8>>; _ -> code(atom_to_binary(Default), [<<"font-mono text-xs">>], []) end]
            || {G, {Vs, Default}} <- lists:sort(maps:to_list(Groups))]
        ++ [[code(atom_to_binary(F), [<<"api-name">>], []), maps:get(F, Docs, <<"标志"/utf8>>), <<"—"/utf8>>]
            || F <- Flags],
    table_section(<<"修饰符（Css 中的原子）"/utf8>>, [<<"组 / 标志"/utf8>>, <<"取值"/utf8>>, <<"默认"/utf8>>], Rows).

table_section(_Title, _Head, []) -> [];
table_section(Title, Head, Rows) ->
    'div'([h3(Title, [<<"api-h">>], []),
           table([thead(tr([th(H, [], []) || H <- Head])),
                  tbody([tr([td(C, [], []) || C <- Row]) || Row <- Rows])],
                 [<<"api-table">>], [])],
          [<<"mb-8">>], []).

event_atom(<<"ah:", _/binary>> = Ev) -> [$', Ev, $'];
event_atom(Ev) -> Ev.

arity(#{signature := Sig}) ->
    case binary:split(Sig, <<"(">>) of
        [_, Args] -> length(binary:split(Args, <<",">>, [global]));
        _ -> 0
    end.
