%%%-------------------------------------------------------------------
%%% @doc A shimmering placeholder while content loads (sigil's skeleton).
%%%
%%% skeleton/2 builds an element record (#ah_skeleton{}, defined in
%%% include/aihtml_skeleton.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_skeleton).
-behaviour(aihtml_element).

-include("aihtml_skeleton.hrl").

-export([skeleton/2, render/1, fields/1, catalog/0]).

-import(aihtml_html, [el/4]).
-import(aihtml_lib_layout, [tf/1, len/1, style/1]).

-define(E, aihtml_element).

-type html() :: aihtml_html:html().

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% @doc A shimmering placeholder. Css: text (default) | circle | rect,
%% static (no shimmer), done (hidden).
%% Options: lines (text, default 3), width, height, radius, label.
-spec skeleton(css(), attrs()) -> #ah_skeleton{}.
skeleton(Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_skeleton{}, Css, Attrs).

%% @doc The field names of #ah_skeleton{}.
-spec fields(ah_skeleton) -> [atom()].
fields(ah_skeleton) -> record_info(fields, ah_skeleton).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_skeleton{}) -> html().
render(#ah_skeleton{width = W, height = H} = R) ->
    Classes = aihtml_element:classes(?MODULE, R),
    Variant = case R#ah_skeleton.variant of undefined -> text; V -> V end,
    Body = case Variant of
               text ->
                   N = max(1, R#ah_skeleton.lines),
                   [el(span, [], [<<"ah-skeleton__line">>],
                       [{style, style([{<<"width">>, case I of N -> <<"62%">>; _ -> <<"100%">> end},
                                       {<<"height">>, len(H)}])}])
                    || I <- lists:seq(1, N)];
               circle ->
                   D = case W of undefined -> 40; _ -> W end,
                   el(span, [], [<<"ah-skeleton__shape ah-skeleton__shape-circle">>],
                      [{style, style([{<<"width">>, len(D)},
                                      {<<"height">>, len(case H of undefined -> D; _ -> H end)}])}]);
               rect ->
                   el(span, [], [<<"ah-skeleton__shape">>],
                      [{style, style([{<<"width">>, len(case W of undefined -> <<"100%">>; _ -> W end)},
                                      {<<"height">>, len(case H of undefined -> 120; _ -> H end)},
                                      {<<"border-radius">>, len(R#ah_skeleton.radius)}])}])
           end,
    el('div', Body, Classes,
       [[{data_variant, Variant}, {data_animated, tf(not R#ah_skeleton.static)},
         {role, status}, {aria_busy, <<"true">>}, {aria_live, polite},
         {aria_label, R#ah_skeleton.label}], ?E:root_attrs(R, none)]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => skeleton, category => layout,
       option_docs => #{lines => <<"Lines of the text variant (default 3, the last one shorter).">>,
                        width => <<"Width (circle: diameter), integer px or CSS length.">>,
                        height => <<"Height of lines or shape.">>,
                        radius => <<"Corner radius of the rect variant.">>,
                        label => <<"aria-label (default Loading).">>,
                        text => <<"Lines of text (default).">>,
                        circle => <<"A circle, e.g. for an avatar.">>,
                        rect => <<"A rectangle, e.g. for an image.">>,
                        static => <<"No shimmer.">>,
                        done => <<"Hidden: loading has finished.">>},
       methods => [],
       signature => <<"skeleton(Css, Attrs)">>, root => <<"ah-skeleton">>,
       groups => #{variant => {[text, circle, rect], none}},
       flags => [static, done],
       classes => #{text => [], circle => [], rect => [], static => [],
                    done => [<<"ah-skeleton--done">>]},
       options => [lines, width, height, radius, label],
       doc => <<"A shimmering placeholder while content loads.">>}].
