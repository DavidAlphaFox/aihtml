%% @doc Shared pieces of the demo site: the page wrapper, the top bar and
%% the catalog helpers the home and docs pages use.
-module(aihtml_example_site).

-include_lib("aihtml/include/aihtml.hrl").

-export([reply/4, lang/1, lang_switch/0, css/0, topbar/1, categories/0, category_label/1, display_name/1,
         components/0, summary/1]).

%% @doc Reply with a whole page of the site, in the visitor's language
%% (see lang/1). `Body' is html, or a fun building it: the fun runs in that
%% language, so what it builds (the language switch, subscriptions) sees
%% it.
-spec reply(cowboy_req:req(), binary(), aihtml:html() | fun(() -> aihtml:html()), map()) ->
          cowboy_req:req().
reply(Req0, Title, Body, Opts) ->
    {Lang, Req} = lang(Req0),
    aihtml_i18n:with(Lang, fun() ->
        Html = case Body of
                   F when is_function(F, 0) -> F();
                   _ -> Body
               end,
        aihtml_cowboy:reply(Req, Html, maps:merge(#{title => Title, css => [css()], lang => Lang},
                                                  Opts))
    end).

-define(COOKIE, <<"aihtml_lang">>).

%% @doc The language of a request: `?lang=zh' (or en) switches it and is
%% remembered in a cookie, so later pages keep it; without either, the
%% default language. Only languages with a catalog are taken, anything
%% else is ignored. Returns the request with the cookie set when ?lang=
%% chose the language.
-spec lang(cowboy_req:req()) -> {binary(), cowboy_req:req()}.
lang(Req) ->
    Qs = proplists:get_value(<<"lang">>, cowboy_req:parse_qs(Req)),
    Cookie = proplists:get_value(?COOKIE, cowboy_req:parse_cookies(Req)),
    case {valid_lang(Qs), valid_lang(Cookie)} of
        {{ok, L}, _} ->
            {L, cowboy_req:set_resp_cookie(?COOKIE, L, Req,
                                           #{path => <<"/">>, max_age => 31536000, same_site => lax})};
        {error, {ok, L}} -> {L, Req};
        {error, error} -> {aihtml_i18n:locale(), Req}
    end.

%% {ok, Tag} for a language tag naming a language with a catalog.
valid_lang(V) when is_binary(V) ->
    Tag = aihtml_i18n:normalize(V),
    [Base | _] = binary:split(Tag, <<"-">>),
    case Tag =:= string:lowercase(binary:replace(V, <<"_">>, <<"-">>, [global]))
         andalso lists:member(Base, aihtml_i18n:locales()) of
        true -> {ok, Tag};
        false -> error
    end;
valid_lang(_) ->
    error.

%% @doc Links switching the site between Chinese and English (?lang=, kept
%% in a cookie by lang/1); the current language is marked. Build it inside
%% reply/4's fun, where the current language is the page's.
-spec lang_switch() -> aihtml:element().
lang_switch() ->
    [Cur | _] = binary:split(aihtml_i18n:locale(), <<"-">>),
    ah_div([ah_a(Label, [<<"px-2 py-1 rounded-control">>,
                         case Tag =:= Cur of
                             true -> <<"bg-primary-lighter text-primary-dark font-semibold">>;
                             false -> <<"text-muted hover:text-primary">>
                         end],
                 [{href, <<"?lang=", Tag/binary>>}, {hreflang, Tag}, {lang, Tag},
                  {aria_current, Tag =:= Cur andalso <<"true">>}])
            || {Tag, Label} <- [{<<"zh">>, <<"中文"/utf8>>}, {<<"en">>, <<"English">>}]],
           [<<"flex items-center gap-1 text-sm">>],
           [{role, group}, {aria_label, <<"Language / 语言"/utf8>>}]).

%% @doc URL of the site's stylesheet: css/example-<hash>.css, named in the
%% manifest `npm run css:example' writes, or the unhashed css/example.css
%% while `npm run watch:example' rebuilds it during development (a full
%% build renames that file away). Read on every page; the files are tiny.
-spec css() -> binary().
css() ->
    Dir = filename:join([code:priv_dir(aihtml_example), "static", "css"]),
    Name = case filelib:is_regular(filename:join(Dir, "example.css")) of
               true -> <<"example.css">>;
               false ->
                   {ok, Bin} = file:read_file(filename:join(Dir, "manifest.json")),
                   maps:get(<<"example.css">>, json:decode(Bin))
           end,
    <<"/static/css/", Name/binary>>.

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
                              lang_switch(),
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
