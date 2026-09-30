%%%-------------------------------------------------------------------
%%% @doc A complete HTML document around a body.
%%%
%%% Options (all optional):
%%%
%%%   title        page title
%%%   lang         `<html lang>' and the language the page renders in (the
%%%                components' texts and formats, see aihtml_i18n), default
%%%                the current language (the application's default_locale,
%%%                "en" unless configured)
%%%
%%% What search engines and link previews read (all written into `<head>'):
%%%
%%%   description  `<meta name="description">'
%%%   canonical    the page's canonical URL, `<link rel="canonical">'
%%%   robots       `<meta name="robots">', e.g. <<"noindex, follow">>
%%%   og           Open Graph properties, #{title, description, url, image,
%%%                type, site_name, locale, ...} -> `<meta property="og:K">';
%%%                a list value writes the property once per element
%%%   meta         other `<meta name>' tags, #{Name => Content} or a list
%%%                of pairs (e.g. twitter:card)
%%%   alternates   language versions, [{Lang, Url}] ->
%%%                `<link rel="alternate" hreflang=Lang href=Url>'
%%%   json_ld      structured data (a map or a list of maps, schema.org),
%%%                written as `<script type="application/ld+json">'
%%%
%%% Presentation and runtime:
%%%
%%%   theme        `aihtml_theme:theme()', written onto `<html>'
%%%   persist      restore the viewer's saved theme before first paint,
%%%                default true (the runtime saves it)
%%%   assets       URL where aihtml's priv/static is served, default
%%%                <<"/aihtml/">>
%%%   css          stylesheet URLs, default [<<"/aihtml/aihtml.css">>]
%%%   runtime      URL of the runtime's entry module, default the bundle
%%%                in priv/static/js (aihtml_assets), or false to leave it
%%%                out
%%%   jquery       a jQuery URL for the page's own scripts, default false
%%%                (the runtime does not need it)
%%%   js           extra script URLs, loaded (deferred) after the runtime
%%%   head         extra `<head>' content
%%%   body_css     Css for `<body>'
%%%   body_attrs   Attrs for `<body>'
%%%   action       URL that actions are POSTed to, default <<"/aihtml/action">>
%%%   events       URL of the push stream, default <<"/aihtml/events">>; the
%%%                page only connects when it has subscriptions
%%%
%%% The default URLs assume `priv/static' of the aihtml application is
%%% served under `/aihtml/' (see the example application); change `assets'
%%% when it is served elsewhere. The runtime is an ES module; scripts in
%%% `js' are deferred so they run after it, in order.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_page).

-export([render/2]).

-export_type([opts/0]).

-type opts() :: #{title => aihtml_html:html(),
                  description => iodata(),
                  canonical => iodata(),
                  robots => iodata(),
                  og => #{atom() | binary() => iodata() | [iodata()]},
                  meta => #{atom() | binary() => iodata()} | [{atom() | binary(), iodata()}],
                  alternates => [{iodata() | atom(), iodata()}],
                  json_ld => map() | [map()],
                  lang => binary() | atom(),
                  theme => aihtml_theme:theme(),
                  persist => boolean(),
                  assets => iodata(),
                  css => [iodata()],
                  jquery => iodata() | false,
                  runtime => iodata() | false,
                  js => [iodata()],
                  head => aihtml_html:html(),
                  body_css => aihtml_html:css(),
                  body_attrs => aihtml_html:attrs(),
                  action => iodata(),
                  events => iodata()}.

-import(aihtml_html, [el/4, void/3]).

%% Applies the theme the viewer saved (assets/js/runtime/theme.ts) before the
%% stylesheet paints, so a reload never flashes the server default.
-define(BOOT,
        <<"(function(){try{var t=JSON.parse(localStorage.getItem('aihtml.theme')||'{}'),"
          "m={appearance:'data-theme',palette:'data-palette',"
          "typography:'data-typography',skin:'data-skin'},"
          "d=document.documentElement;for(var k in m)if(t[k])d.setAttribute(m[k],t[k]);"
          "}catch(e){}})();">>).

-spec render(aihtml_html:html(), opts()) -> iodata().
render(Body, Opts) ->
    Theme = maps:get(theme, Opts, #{}),
    Assets = iolist_to_binary(maps:get(assets, Opts, <<"/aihtml/">>)),
    {Runtime, Preload} = runtime(Assets, maps:get(runtime, Opts, default)),
    Scripts = [[el(script, [], [], [{src, iolist_to_binary(U)}])
                || U <- [maps:get(jquery, Opts, false)], U =/= false],
               [el(script, [], [], [{type, module}, {src, U}]) || U <- [Runtime], U =/= false],
               [el(script, [], [], [{src, iolist_to_binary(U)}, {defer, true}])
                || U <- maps:get(js, Opts, [])]],
    Head = el(head,
              [void(meta, [], [{charset, <<"utf-8">>}]),
               void(meta, [], [{name, viewport},
                               {content, <<"width=device-width, initial-scale=1">>}]),
               el(title, maps:get(title, Opts, <<>>), [], []),
               seo(Opts),
               [el(script, {safe, ?BOOT}, [], []) || maps:get(persist, Opts, true)],
               [void(link, [], [{rel, modulepreload}, {href, U}]) || U <- Preload],
               [void(link, [], [{rel, stylesheet}, {href, iolist_to_binary(U)}])
                || U <- maps:get(css, Opts, [<<"/aihtml/aihtml.css">>])],
               maps:get(head, Opts, [])],
              [], []),
    BodyEl = el(body,
                [Body, Scripts],
                [<<"ah-body">>, maps:get(body_css, Opts, [])],
                [{data_ah_action, iolist_to_binary(maps:get(action, Opts, <<"/aihtml/action">>))},
                 {data_ah_events, iolist_to_binary(maps:get(events, Opts, <<"/aihtml/events">>))},
                 maps:get(body_attrs, Opts, [])]),
    Lang = maps:get(lang, Opts, aihtml_i18n:locale()),
    Html = el(html, [Head, BodyEl], [], [{lang, Lang}, aihtml_theme:attrs(Theme)]),
    %% records render lazily, so the body's components see the language
    [<<"<!DOCTYPE html>\n">>, aihtml_i18n:with(Lang, fun() -> aihtml_html:render(Html) end)].

%% The entry module and the chunks it imports (modulepreload), from the
%% bundle's manifest unless the page names its own runtime.
runtime(_Assets, false) -> {false, []};
runtime(Assets, default) ->
    #{file := File, imports := Imports} = aihtml_assets:entry(),
    {<<Assets/binary, "js/", File/binary>>, [<<Assets/binary, "js/", I/binary>> || I <- Imports]};
runtime(_Assets, Url) -> {iolist_to_binary(Url), []}.

%% The tags search engines and link previews read, in a stable order.
seo(Opts) ->
    Name = fun(N, V) -> void(meta, [], [{name, N}, {content, text(V)}]) end,
    [[Name(description, V) || V <- opt(description, Opts)],
     [Name(robots, V) || V <- opt(robots, Opts)],
     [void(link, [], [{rel, canonical}, {href, text(V)}]) || V <- opt(canonical, Opts)],
     [void(link, [], [{rel, alternate}, {hreflang, text(L)}, {href, text(U)}])
      || {L, U} <- maps:get(alternates, Opts, [])],
     [void(meta, [], [{property, <<"og:", (text(K))/binary>>}, {content, text(V)}])
      || {K, Vs} <- pairs(maps:get(og, Opts, #{})), V <- values(Vs)],
     [Name(text(K), V) || {K, V} <- pairs(maps:get(meta, Opts, []))],
     [el(script, {safe, json_ld(D)}, [], [{type, <<"application/ld+json">>}])
      || D <- opt(json_ld, Opts)]].

opt(K, Opts) ->
    case maps:get(K, Opts, undefined) of
        undefined -> [];
        V -> [V]
    end.

%% A map's pairs sorted by key (so the head is the same on every node), a
%% list as given.
pairs(M) when is_map(M) -> lists:sort([{text(K), V} || K := V <- M]);
pairs(L) when is_list(L) -> L.

%% A list of values (og:image several times) or one value.
values([V | _] = L) when is_binary(V); is_list(V) -> L;
values(V) -> [V].

text(A) when is_atom(A) -> atom_to_binary(A, utf8);
text(V) -> unicode:characters_to_binary(V).

%% JSON inside <script>: `<' written as \u003c so the data cannot end the
%% element (or open a comment).
json_ld(Data) ->
    binary:replace(iolist_to_binary(aihtml_json:encode(Data)), <<"<">>, <<"\\u003c">>, [global]).
