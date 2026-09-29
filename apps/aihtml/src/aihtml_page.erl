%%%-------------------------------------------------------------------
%%% @doc A complete HTML document around a body.
%%%
%%% Options (all optional):
%%%
%%%   title        page title
%%%   lang         `<html lang>', default <<"en">>
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
                  lang => binary(),
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

%% Applies the theme the viewer saved (see core.js, AH.theme) before the
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
    Html = el(html, [Head, BodyEl], [],
              [{lang, maps:get(lang, Opts, <<"en">>)}, aihtml_theme:attrs(Theme)]),
    [<<"<!DOCTYPE html>\n">>, aihtml_html:render(Html)].

%% The entry module and the chunks it imports (modulepreload), from the
%% bundle's manifest unless the page names its own runtime.
runtime(_Assets, false) -> {false, []};
runtime(Assets, default) ->
    #{file := File, imports := Imports} = aihtml_assets:entry(),
    {<<Assets/binary, "js/", File/binary>>, [<<Assets/binary, "js/", I/binary>> || I <- Imports]};
runtime(_Assets, Url) -> {iolist_to_binary(Url), []}.
