%%%-------------------------------------------------------------------
%%% @doc A complete HTML document around a body.
%%%
%%% Options (all optional):
%%%
%%%   title        page title
%%%   lang         `<html lang>', default <<"en">>
%%%   theme        `aihtml_theme:theme()', written onto `<html>'
%%%   persist      restore the viewer's saved theme before first paint,
%%%                default true (the jQuery runtime saves it)
%%%   css          stylesheet URLs, default [<<"/aihtml/aihtml.css">>]
%%%   jquery       jQuery URL, or false to leave it out
%%%   runtime      aihtml.js URL, or false to leave it out
%%%   js           extra script URLs, loaded after the runtime
%%%   head         extra `<head>' content
%%%   body_css     Css for `<body>'
%%%   body_attrs   Attrs for `<body>'
%%%   action       URL that actions are POSTed to, default <<"/aihtml/action">>
%%%
%%% The default URLs assume `priv/static' of the aihtml application is
%%% served under `/aihtml/', see the example application.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_page).

-export([render/2]).

-export_type([opts/0]).

-type opts() :: #{title => aihtml_html:html(),
                  lang => binary(),
                  theme => aihtml_theme:theme(),
                  persist => boolean(),
                  css => [iodata()],
                  jquery => iodata() | false,
                  runtime => iodata() | false,
                  js => [iodata()],
                  head => aihtml_html:html(),
                  body_css => aihtml_html:css(),
                  body_attrs => aihtml_html:attrs(),
                  action => iodata()}.

-import(aihtml_html, [el/4, void/3]).

%% Applies the theme the viewer saved (see aihtml.js, AH.theme) before the
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
    Scripts = [U || U <- [maps:get(jquery, Opts, <<"/aihtml/vendor/jquery.min.js">>),
                          maps:get(runtime, Opts, <<"/aihtml/aihtml.js">>)],
                    U =/= false]
              ++ maps:get(js, Opts, []),
    Head = el(head,
              [void(meta, [], [{charset, <<"utf-8">>}]),
               void(meta, [], [{name, viewport},
                               {content, <<"width=device-width, initial-scale=1">>}]),
               el(title, maps:get(title, Opts, <<>>), [], []),
               [el(script, {safe, ?BOOT}, [], []) || maps:get(persist, Opts, true)],
               [void(link, [], [{rel, stylesheet}, {href, iolist_to_binary(U)}])
                || U <- maps:get(css, Opts, [<<"/aihtml/aihtml.css">>])],
               maps:get(head, Opts, [])],
              [], []),
    BodyEl = el(body,
                [Body, [el(script, [], [], [{src, iolist_to_binary(U)}]) || U <- Scripts]],
                [<<"ah-body">>, maps:get(body_css, Opts, [])],
                [{data_ah_action, iolist_to_binary(maps:get(action, Opts, <<"/aihtml/action">>))},
                 maps:get(body_attrs, Opts, [])]),
    Html = el(html, [Head, BodyEl], [],
              [{lang, maps:get(lang, Opts, <<"en">>)}, aihtml_theme:attrs(Theme)]),
    [<<"<!DOCTYPE html>\n">>, aihtml_html:render(Html)].
