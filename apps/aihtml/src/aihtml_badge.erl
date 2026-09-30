%%%-------------------------------------------------------------------
%%% @doc The badge component (designs/04-components.md): `ah_badge/3'
%%% builds an #ah_badge{} element record (include/aihtml_badge.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_badge).
-behaviour(aihtml_element).

-include("aihtml_badge.hrl").

-export([ah_badge/3, render/1, fields/1, catalog/0]).

-import(aihtml_lib_display, [blank/1, tf/1, num/1, none_for/1, method/3]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(COLORS, aihtml_lib_color:theme_colors()).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Badge: a count, dot or status dot over the corner of `Content'
%% (the anchor). With no anchor (`undefined') the indicator stands alone
%% inline. Options: `count', `max' (99).
-spec ah_badge(html(), css(), attrs()) -> #ah_badge{}.
ah_badge(Content, Css, Attrs) ->
    ?E:build(?MODULE, #ah_badge{body = Content}, Css, Attrs).

%% @doc The field names of #ah_badge{}.
-spec fields(atom()) -> [atom()].
fields(ah_badge) -> record_info(fields, ah_badge).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_badge{}) -> html().
render(#ah_badge{body = Content, variant = Variant, count = Count, max = Max,
                 show_zero = ShowZero} = R) ->
    Cls = ?E:classes(?MODULE, R),
    Dot = Variant =/= standard,
    Invisible = Variant =:= invisible
        orelse (not Dot andalso is_number(Count) andalso Count == 0
                andalso not ShowZero),
    Standalone = blank(Content),
    Indicator = ?H:el(span, case Dot of true -> []; false -> badge_label(Count, Max) end,
                      [<<"ah-badge-indicator">>],
                      [{data_variant, Variant}, {data_color, R#ah_badge.color},
                       {data_dot, tf(Dot)}, {data_invisible, tf(Invisible)},
                       {aria_hidden, not Standalone andalso <<"true">>}]),
    ?H:el(span, [if Standalone -> []; true -> Content end, Indicator],
          [Cls, [<<"ah-badge-root--standalone">> || Standalone]],
          [[{data_overlap, R#ah_badge.overlap},
            {data_anchor_vertical, R#ah_badge.vertical},
            {data_anchor_horizontal, R#ah_badge.horizontal},
            {data_ah, <<"badge">>}, {data_ah_max, Max},
            {data_ah_show_zero, ShowZero andalso <<"true">>}],
           ?E:root_attrs(R, none)]).

badge_label(undefined, _Max) -> <<>>;
badge_label(N, Max) when is_number(N), is_number(Max), N > Max -> [num(Max), <<"+">>];
badge_label(N, _Max) -> N.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => badge, category => media, root => <<"ah-badge-root">>,
      signature => <<"ah_badge(Anchor, Css, Attrs)">>,
      groups => #{variant => {[standard, dot, online, away, busy, offline, invisible], standard},
                  color => {[default | ?COLORS], primary},
                  overlap => {[rect, circular], rect},
                  vertical => {[top, bottom], top},
                  horizontal => {[left, right], right}},
      flags => [show_zero],
      classes => none_for([standard, dot, online, away, busy, offline, invisible, default,
                           rect, circular, top, bottom, left, right, show_zero | ?COLORS]),
      options => [count, max], behavior => <<"badge">>,
      doc => <<"Count, dot or status dot on the corner of Anchor; standalone "
               "when Anchor is undefined. Method setCount(n).">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{count => <<"Number or text in the indicator; numbers above max show \"max+\".">>,
                       max => <<"Largest count shown as is (default 99).">>,
                       show_zero => <<"Keep the indicator visible when count is 0.">>},
      methods => [method(setCount, <<"(Count)">>, <<"Change the count, applying max and show_zero.">>)]}.
