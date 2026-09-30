%% @doc Shared pieces of the demo site: the page wrapper, the top bar and
%% the catalog helpers the home and docs pages use.
-module(aihtml_example_site).

-include_lib("aihtml/include/aihtml.hrl").

-export([reply/4, topbar/1, categories/0, category_label/1, display_name/1,
         components/0, summary/1]).

-spec reply(cowboy_req:req(), binary(), aihtml:html(), map()) -> cowboy_req:req().
reply(Req, Title, Body, Opts) ->
    aihtml_cowboy:reply(Req, Body, maps:merge(#{title => Title,
                                                css => [<<"/static/example.css">>]}, Opts)).

%% @doc The site's top bar; Active is home | components | demo | fetch.
-spec topbar(atom()) -> aihtml:element().
topbar(Active) ->
    Link = fun(Key, Href, Label) ->
                   ah_a(Label, [<<"px-3 py-2 text-sm rounded-control hover:text-primary">>,
                                [<<"text-primary font-semibold">> || Key =:= Active]],
                        [{href, Href}])
           end,
    ah_header(ah_div([ah_a(<<"aihtml">>, [<<"text-xl font-bold text-primary">>], [{href, <<"/">>}]),
                      ah_nav([Link(components, <<"/components">>, <<"组件"/utf8>>),
                              Link(demo, <<"/demo">>, <<"实时演示"/utf8>>),
                              Link(fetch, <<"/fetch">>, <<"片段模式"/utf8>>),
                              ah_a(<<"GitHub">>, [<<"px-3 py-2 text-sm hover:text-primary">>],
                                   [{href, <<"https://github.com/DavidAlphaFox/aihtml">>}])],
                             [<<"flex items-center gap-1">>], [])],
                     [<<"max-w-6xl mx-auto px-6 h-14 flex items-center justify-between">>], []),
              [<<"bg-surface border-b border-line sticky top-0 z-30">>], []).

%% @doc Component categories in site order.
-spec categories() -> [atom()].
categories() -> [form, layout, overlay, data, media, text].

-spec category_label(atom()) -> binary().
category_label(form) -> <<"表单与输入"/utf8>>;
category_label(layout) -> <<"布局与导航"/utf8>>;
category_label(overlay) -> <<"浮层"/utf8>>;
category_label(data) -> <<"数据展示"/utf8>>;
category_label(media) -> <<"媒体"/utf8>>;
category_label(text) -> <<"文本"/utf8>>;
category_label(C) -> atom_to_binary(C).

%% @doc The demo's title (RadioButton), else dropdown_button -> DropdownButton.
-spec display_name(atom()) -> binary().
display_name(Name) ->
    case aihtml_example_demos:info(Name) of
        #{title := T} -> T;
        _ -> iolist_to_binary([string:titlecase(P) || P <- string:split(atom_to_list(Name), "_", all)])
    end.

%% @doc Catalog entries of the components (the theme switcher excluded).
-spec components() -> [aihtml_catalog:entry()].
components() ->
    [E || #{category := C} = E <- aihtml_catalog:prefabs(), C =/= theme].

%% @doc The demo's Chinese summary, else the first sentence of the catalog doc.
-spec summary(aihtml_catalog:entry()) -> binary().
summary(#{name := Name} = E) ->
    case aihtml_example_demos:info(Name) of
        #{summary := S} -> S;
        _ -> doc_summary(E)
    end.

doc_summary(#{doc := Doc}) ->
    case re:run(Doc, <<"^(.*?[.;:])(\\s|$)">>, [{capture, [1], binary}, dotall]) of
        {match, [S]} -> S;
        nomatch -> Doc
    end.
