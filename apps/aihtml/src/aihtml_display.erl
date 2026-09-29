%%%-------------------------------------------------------------------
%%% @doc Display components ported from sigil (media, text and data
%%% groups) plus aihtml's own `alert'. See designs/04-components.md.
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged. Many sigil components are
%%% styled through `data-*' attributes (`data-size', `data-color', ...)
%%% rather than classes; for those the catalog maps the modifier to no
%%% class (`classes => #{M => []}') and the function writes the attribute.
%%%
%%% Timestamps given to `time_ago/3' are Unix seconds (as returned by
%%% `erlang:system_time(second)'), a UTC `calendar:datetime()' or an
%%% RFC 3339 binary.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_display).

-export([avatar/3, badge/3, chip/3, aspect_ratio/3, kbd/3, time_ago/3,
         expandable_text/3, progressbar/3, progress_circle/3, meter/3,
         statistic/3, kpi_card/3, timeline/3, ranking_list/3, tag_cloud/3,
         alert/3, catalog/0, examples/0]).

-define(H, aihtml_html).
-define(COLORS, [primary, secondary, success, warning, error, info]).
%% 2 * pi * 45, the progress circle's circumference (r = 45 in a 100 box)
-define(CIRC, 282.74333882308139).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type element() :: aihtml_html:element().

%%%===================================================================
%%% Media
%%%===================================================================

%% @doc Avatar: an image with an initials (or icon) fallback that shows
%% when there is no `src' or the image fails to load. `Content' is the
%% fallback, `"?"' when empty.
-spec avatar(html(), css(), attrs()) -> element().
avatar(Content, Css, Attrs) ->
    {Cls, P, _F, O, Rest} = setup(avatar, Css, Attrs),
    Src = opt(src, O, undefined),
    Alt = opt(alt, O, <<>>),
    Img = case blank(Src) of
              true -> [];
              false -> ?H:void(img, [<<"ah-avatar__image">>], [{src, Src}, {alt, Alt}])
          end,
    Fallback = case blank(Content) of true -> <<"?">>; false -> Content end,
    %% Without an image the fallback is aria-hidden, so name the avatar.
    Named = blank(Src) andalso not blank(Alt),
    ?H:el(span,
          [Img, ?H:el(span, Fallback, [<<"ah-avatar__fallback">>], [{aria_hidden, <<"true">>}])],
          Cls,
          [[{data_size, maps:get(size, P)}, {data_shape, maps:get(shape, P)},
            {data_color, maps:get(color, P)}, {data_ah, <<"avatar">>},
            {role, Named andalso <<"img">>}, {aria_label, Named andalso Alt}],
           Rest]).

%% @doc Badge: a count, dot or status dot over the corner of `Content'
%% (the anchor). With no anchor (`undefined') the indicator stands alone
%% inline. Options: `count', `max' (99).
-spec badge(html(), css(), attrs()) -> element().
badge(Content, Css, Attrs) ->
    {Cls, P, F, O, Rest} = setup(badge, Css, Attrs),
    Variant = maps:get(variant, P),
    Count = opt(count, O, undefined),
    Max = opt(max, O, 99),
    Dot = Variant =/= standard,
    Invisible = Variant =:= invisible
        orelse (not Dot andalso is_number(Count) andalso Count == 0
                andalso not lists:member(show_zero, F)),
    Standalone = blank(Content),
    Indicator = ?H:el(span, case Dot of true -> []; false -> badge_label(Count, Max) end,
                      [<<"ah-badge-indicator">>],
                      [{data_variant, Variant}, {data_color, maps:get(color, P)},
                       {data_dot, tf(Dot)}, {data_invisible, tf(Invisible)},
                       {aria_hidden, not Standalone andalso <<"true">>}]),
    ?H:el(span, [if Standalone -> []; true -> Content end, Indicator],
          [Cls, [<<"ah-badge-root--standalone">> || Standalone]],
          [[{data_overlap, maps:get(overlap, P)},
            {data_anchor_vertical, maps:get(vertical, P)},
            {data_anchor_horizontal, maps:get(horizontal, P)},
            {data_ah, <<"badge">>}, {data_ah_max, Max},
            {data_ah_show_zero, lists:member(show_zero, F) andalso <<"true">>}],
           Rest]).

badge_label(undefined, _Max) -> <<>>;
badge_label(N, Max) when is_number(N), is_number(Max), N > Max -> [num(Max), <<"+">>];
badge_label(N, _Max) -> N.

%% @doc Chip: a compact label with optional avatar, icon and remove
%% button. `removable' chips fire `ah:remove' and then `change' before
%% they remove themselves; `clickable' chips are keyboard focusable.
%% Options: `avatar' (initials), `icon' (html), `value' (data-ah-value,
%% defaults to a binary `Content').
-spec chip(html(), css(), attrs()) -> element().
chip(Content, Css, Attrs) ->
    {Cls, P, F, O, Rest} = setup(chip, Css, Attrs),
    Removable = lists:member(removable, F),
    Clickable = lists:member(clickable, F),
    Disabled = lists:member(disabled, F),
    Avatar = opt(avatar, O, undefined),
    Icon = opt(icon, O, undefined),
    Value = opt(value, O, if is_binary(Content) -> Content; true -> undefined end),
    Children = [[?H:el(span, Avatar, [<<"ah-chip__avatar">>], []) || not blank(Avatar)],
                [?H:el(span, Icon, [<<"ah-chip__icon">>], []) || not blank(Icon)],
                ?H:el(span, Content, [<<"ah-chip__label">>], []),
                [?H:el(button, <<"×"/utf8>>, [<<"ah-chip__delete">>],
                       [{type, button}, {aria_label, <<"Remove">>}, {tabindex, -1}])
                 || Removable]],
    Focusable = (Clickable orelse Removable) andalso not Disabled,
    ?H:el(span, Children, Cls,
          [[{data_variant, maps:get(variant, P)}, {data_color, maps:get(color, P)},
            {data_size, maps:get(size, P)}, {data_disabled, tf(Disabled)},
            {data_clickable, tf(Clickable)}, {data_ah, <<"chip">>},
            {data_ah_value, Value},
            {role, Clickable andalso <<"button">>},
            {tabindex, Focusable andalso 0},
            {aria_disabled, Disabled andalso <<"true">>}],
           Rest]).

%% @doc Fixed aspect ratio box. Option `ratio': `<<"16/9">>' (default),
%% `<<"4:3">>', a number or `{W, H}'. A `style' attribute is kept.
-spec aspect_ratio(html(), css(), attrs()) -> element().
aspect_ratio(Children, Css, Attrs) ->
    {Cls, _P, _F, O, Rest0} = setup(aspect_ratio, Css, Attrs),
    {Styles, Rest} = lists:partition(fun({K, _}) -> K =:= style orelse K =:= <<"style">>;
                                        (_) -> false
                                     end, Rest0),
    Style = [<<"aspect-ratio: ">>, ratio_css(opt(ratio, O, undefined)), <<";">>
             | [[<<" ">>, V] || {_, V} <- Styles]],
    ?H:el('div', Children, Cls, [[{style, iolist_to_binary(Style)}], Rest]).

ratio_css(undefined) -> <<"16 / 9">>;
ratio_css(N) when is_number(N), N > 0 -> num(N);
ratio_css({W, H}) when is_number(W), is_number(H), W > 0, H > 0 ->
    [num(W), <<" / ">>, num(H)];
ratio_css(S) when is_binary(S); is_list(S) ->
    B = unicode:characters_to_binary(S),
    Parts = [string:trim(X) || X <- binary:split(B, [<<"/">>, <<":">>])],
    case [parse_num(X) || X <- Parts] of
        [N] when is_number(N), N > 0 -> num(N);
        [W, H] when is_number(W), is_number(H), W > 0, H > 0 -> [num(W), <<" / ">>, num(H)];
        _ -> error({aihtml, {bad_ratio, S}})
    end;
ratio_css(Other) -> error({aihtml, {bad_ratio, Other}}).

%%%===================================================================
%%% Text
%%%===================================================================

%% @doc Keyboard key. `Keys' is one key (`<<"Esc">>') or a list of keys
%% (`[<<"Ctrl">>, <<"K">>]'), rendered as nested `<kbd>'s joined by
%% the `separator' option (default "+").
-spec kbd(html() | [html()], css(), attrs()) -> element().
kbd(Keys, Css, Attrs) ->
    {[Root | Literal], P, _F, O, Rest} = setup(kbd, Css, Attrs),
    Size = maps:get(size, P),
    case is_list(Keys) andalso Keys =/= [] andalso not io_lib:printable_unicode_list(Keys) of
        false ->
            ?H:el(kbd, Keys, [Root | Literal], [[{data_size, Size}], Rest]);
        true ->
            Sep = ?H:el(span, opt(separator, O, <<"+">>), [<<"ah-kbd-combo__sep">>],
                        [{aria_hidden, <<"true">>}]),
            Ks = [?H:el(kbd, K, [Root], [{data_size, Size}]) || K <- Keys],
            ?H:el(kbd, lists:join(Sep, Ks), [<<"ah-kbd-combo">> | Literal],
                  [[{data_size, Size}], Rest])
    end.

%% @doc Relative time ("3m ago") in sigil's format, rendered on the server
%% and kept current by the browser every 60 s. `Timestamp' is Unix
%% seconds, a UTC `calendar:datetime()' or an RFC 3339 binary.
%% Options: `now' (Unix seconds, for rendering), `labels' (map with
%% just_now, minutes, hours, days, months; "{n}" is the number),
%% `live' (true), `title' (true: absolute time on hover).
-spec time_ago(integer() | calendar:datetime() | binary(), css(), attrs()) -> element().
time_ago(Timestamp, Css, Attrs) ->
    {Cls, _P, _F, O, Rest} = setup(time_ago, Css, Attrs),
    Secs = to_unix(Timestamp),
    Now = opt(now, O, erlang:system_time(second)),
    Custom = opt(labels, O, #{}),
    Labels = maps:merge(default_labels(), Custom),
    Iso = list_to_binary(calendar:system_time_to_rfc3339(Secs, [{offset, "Z"}])),
    Title = case opt(title, O, true) of
                false -> undefined;
                _ -> binary:replace(binary:replace(Iso, <<"T">>, <<" ">>), <<"Z">>, <<" UTC">>)
            end,
    LabelAttrs = [{<<"data-ah-label-", (dash(K))/binary>>, V}
                  || {K, V} <- lists:sort(maps:to_list(Custom))],
    ?H:el(time, format_ago(Now - Secs, Labels), Cls,
          [[{datetime, Iso}, {title, Title}, {data_ah, <<"time-ago">>},
            {data_ah_title, Title =/= undefined andalso <<"true">>},
            {data_ah_live, opt(live, O, true) =:= false andalso <<"false">>},
            LabelAttrs],
           Rest]).

default_labels() ->
    #{just_now => <<"just now">>, minutes => <<"{n}m ago">>, hours => <<"{n}h ago">>,
      days => <<"{n}d ago">>, months => <<"{n}mo ago">>}.

format_ago(Secs, L) ->
    Mins = Secs div 60, Hours = Mins div 60, Days = Hours div 24, Months = Days div 30,
    Sub = fun(K, N) -> binary:replace(to_bin(maps:get(K, L)), <<"{n}">>,
                                      integer_to_binary(N), [global]) end,
    if Secs < 60 -> maps:get(just_now, L);
       Mins < 60 -> Sub(minutes, Mins);
       Hours < 24 -> Sub(hours, Hours);
       Days < 30 -> Sub(days, Days);
       true -> Sub(months, Months)
    end.

to_unix(S) when is_integer(S) -> S;
to_unix({{_, _, _}, {_, _, _}} = DT) ->
    calendar:datetime_to_gregorian_seconds(DT) - 62167219200;
to_unix(B) when is_binary(B) ->
    try calendar:rfc3339_to_system_time(binary_to_list(B))
    catch _:_ -> error({aihtml, {bad_timestamp, B}})
    end;
to_unix(Other) -> error({aihtml, {bad_timestamp, Other}}).

%% @doc Long text cut at `threshold' characters (100) with a toggle.
%% Options: `threshold', `expanded' (false), `expand_label' ("展开"),
%% `collapse_label' ("收起"). Fires `ah:toggle' with the new state.
-spec expandable_text(unicode:chardata(), css(), attrs()) -> element().
expandable_text(Text0, Css, Attrs) ->
    {Cls, _P, _F, O, Rest} = setup(expandable_text, Css, Attrs),
    Text = case Text0 of undefined -> <<>>; _ -> unicode:characters_to_binary(Text0) end,
    Th = opt(threshold, O, 100),
    Exp = opt(expanded, O, false) =:= true,
    Long = string:length(Text) > Th,
    ExpandL = opt(expand_label, O, <<"展开"/utf8>>),
    CollapseL = opt(collapse_label, O, <<"收起"/utf8>>),
    Body = case Long of
               false -> Text;
               true ->
                   [?H:el(span, [string:slice(Text, 0, Th), <<"…"/utf8>>], [],
                          [{data_ah_part, short}, {hidden, Exp}]),
                    ?H:el(span, Text, [], [{data_ah_part, full}, {hidden, not Exp}])]
           end,
    Toggle = [?H:el(button, if Exp -> CollapseL; true -> ExpandL end,
                    [<<"ah-expandable-text__toggle">>],
                    [{type, button}, {aria_expanded, tf(Exp)},
                     {data_ah_expand_label, ExpandL}, {data_ah_collapse_label, CollapseL}])
              || Long],
    ?H:el('div', [?H:el(span, Body, [<<"ah-expandable-text__body">>], []), Toggle], Cls,
          [[{data_expanded, tf(Exp)}, {data_truncated, tf(Long)}, {data_ah, <<"expandable-text">>}],
           Rest]).

%% @doc Alert (aihtml's own): an inline message box. Options: `title',
%% `icon' (true, false or html). `dismissible' adds a close button; the
%% browser fires `ah:dismiss' (cancellable) and removes the alert.
-spec alert(html(), css(), attrs()) -> element().
alert(Children, Css, Attrs) ->
    {Cls, P, F, O, Rest} = setup(alert, Css, Attrs),
    Variant = maps:get(variant, P),
    Icon = case opt(icon, O, true) of
               true -> alert_icon(Variant);
               false -> [];
               Html -> Html
           end,
    Title = opt(title, O, undefined),
    ?H:el('div',
          [[?H:el(span, Icon, [<<"ah-alert-icon">>], [{aria_hidden, <<"true">>}]) || Icon =/= []],
           ?H:el('div', [[?H:el('div', Title, [<<"ah-alert-title">>], []) || not blank(Title)],
                         ?H:el('div', Children, [<<"ah-alert-body">>], [])],
                 [<<"ah-alert-content">>], []),
           [?H:el(button, <<"×"/utf8>>, [<<"ah-alert-close">>],
                  [{type, button}, {aria_label, <<"Close">>}])
            || lists:member(dismissible, F)]],
          Cls, [[{role, <<"alert">>}, {data_ah, <<"alert">>}], Rest]).

alert_icon(Variant) ->
    Shapes = case Variant of
                 info -> [circle(12, 12, 10), line(12, 16, 12, 12), line(12, 8, 12.01, 8)];
                 success -> [circle(12, 12, 10), polyline(<<"9 12 11 14 15 10">>)];
                 warning -> [path(<<"M10.29 3.86 1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 "
                                    "1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z">>),
                             line(12, 9, 12, 13), line(12, 17, 12.01, 17)];
                 error -> [circle(12, 12, 10), line(15, 9, 9, 15), line(9, 9, 15, 15)]
             end,
    svg(Shapes).

%%%===================================================================
%%% Data
%%%===================================================================

%% @doc Linear progress. `Value' is clamped to [min, max]; with the
%% `indeterminate' flag it may be `undefined'. Options: `min' (0),
%% `max' (100), `text' (label instead of the percentage), `color_ranges'
%% (`[{Stop, Color}]', Color a colour atom or a CSS colour).
-spec progressbar(number() | undefined, css(), attrs()) -> element().
progressbar(Value, Css, Attrs) ->
    {Cls, P, F, O, Rest} = setup(progressbar, Css, Attrs),
    Min = opt(min, O, 0),
    Max = opt(max, O, 100),
    Indet = lists:member(indeterminate, F),
    V = clamp(if is_number(Value) -> Value; true -> Min end, Min, Max),
    Pct = pct(V, Min, Max),
    Vert = maps:get(orientation, P) =:= vertical,
    Dim = if Vert -> <<"height">>; true -> <<"width">> end,
    Size = fun(X) -> iolist_to_binary([Dim, <<": ">>, num(X), <<"%;">>]) end,
    Ranges = opt(color_ranges, O, []),
    N = length(Ranges),
    RangeEls = [?H:el('div', [], [<<"ah-progressbar-range">>],
                      [{data_range_index, I - 1}, {data_ah_stop, Stop},
                       {style, iolist_to_binary(
                                 [<<"background-color: ">>, color_css(C),
                                  <<"; z-index: ">>, integer_to_binary(N - I + 1), <<"; ">>,
                                  Size(pct(lists:min([Stop, Max, V]), Min, Max))])}])
                || {I, {Stop, C}} <- lists:enumerate(Ranges)],
    Text = opt(text, O, undefined),
    Label = case Text of undefined -> [integer_to_binary(round(Pct)), <<"%">>]; _ -> Text end,
    ShowText = lists:member(show_text, F) andalso not Indet,
    ?H:el('div',
          [?H:el('div', [], [if Vert -> <<"ah-progressbar-value-vertical">>;
                                true -> <<"ah-progressbar-value">> end],
                 [{style, not Indet andalso Size(Pct)}]),
           RangeEls,
           ?H:el('div', ?H:el(span, Label, [<<"ah-progressbar-text">>],
                              [{style, not ShowText andalso <<"display: none;">>}]),
                 [<<"ah-progressbar-text-host">>], [])],
          Cls,
          [[{role, <<"progressbar">>}, {aria_valuemin, Min}, {aria_valuemax, Max},
            {aria_valuenow, not Indet andalso V},
            {aria_valuetext, not Indet andalso iolist_to_binary(Label)},
            {aria_orientation, maps:get(orientation, P)},
            {aria_busy, Indet andalso <<"true">>},
            {aria_disabled, lists:member(disabled, F) andalso <<"true">>},
            {data_ah, <<"progressbar">>}, {data_ah_value, not Indet andalso V},
            {data_ah_min, Min}, {data_ah_max, Max},
            {data_ah_text, Text =/= undefined andalso <<"custom">>}],
           Rest]).

%% @doc Circular progress (SVG), `Value' 0..100. Options: `label' (text
%% under the ring, also the aria-label), `show_value' (true).
-spec progress_circle(number() | undefined, css(), attrs()) -> element().
progress_circle(Value, Css, Attrs) ->
    {Cls, _P, F, O, Rest} = setup(progress_circle, Css, Attrs),
    Indet = lists:member(indeterminate, F),
    V = trunc(clamp(if is_number(Value) -> Value; true -> 0 end, 0, 100)),
    Label = opt(label, O, undefined),
    Circle = fun(C, Extra) ->
                     ?H:el(circle, [], [C], [{cx, 50}, {cy, 50}, {r, 45} | Extra])
             end,
    Svg = ?H:el(svg,
                [Circle(<<"ah-progress-circle-track">>, []),
                 Circle(<<"ah-progress-circle-fill">>,
                        [{transform, <<"rotate(-90 50 50)">>},
                         {stroke_dasharray, num(?CIRC)},
                         {stroke_dashoffset, num(offset(V))}])],
                [], [{<<"viewBox">>, <<"0 0 100 100">>},
                     {xmlns, <<"http://www.w3.org/2000/svg">>},
                     {aria_hidden, <<"true">>}]),
    ShowValue = opt(show_value, O, true) =/= false andalso not Indet,
    ?H:el('div',
          [?H:el('div', [Svg, [?H:el(span, [integer_to_binary(V), <<"%">>],
                                     [<<"ah-progress-circle-value">>], []) || ShowValue]],
                 [<<"ah-progress-circle-ring">>], []),
           [?H:el(span, Label, [<<"ah-progress-circle-label">>], []) || not blank(Label)]],
          Cls,
          [[{role, <<"progressbar">>}, {aria_valuemin, 0}, {aria_valuemax, 100},
            {aria_valuenow, not Indet andalso V},
            {aria_valuetext, not Indet andalso <<(integer_to_binary(V))/binary, "%">>},
            {aria_busy, Indet andalso <<"true">>},
            {aria_label, not blank(Label) andalso Label},
            {aria_disabled, lists:member(disabled, F) andalso <<"true">>},
            {data_ah, <<"progress-circle">>}, {data_ah_value, not Indet andalso V}],
           Rest]).

offset(V) -> ?CIRC * (1 - V / 100).

%% @doc Meter: a measurement in a known range, coloured low / optimum /
%% high like the native `<meter>'. Options: `min' (0), `max' (100), `low',
%% `high', `optimum', `label', `helper_text', `show_value' (false).
-spec meter(number(), css(), attrs()) -> element().
meter(Value, Css, Attrs) ->
    {Cls, P, _F, O, Rest} = setup(meter, Css, Attrs),
    Min = opt(min, O, 0),
    Max = opt(max, O, 100),
    Label = opt(label, O, undefined),
    ShowValue = opt(show_value, O, false) =:= true,
    Helper = opt(helper_text, O, undefined),
    State = meter_state(Value, Min, Max, opt(low, O, undefined), opt(high, O, undefined),
                        opt(optimum, O, undefined)),
    ?H:el('div',
          [[?H:el('div', [[?H:el(span, Label, [<<"ah-meter__label">>], []) || not blank(Label)],
                          [?H:el(span, Value, [<<"ah-meter__value">>], []) || ShowValue]],
                  [<<"ah-meter__head">>], []) || ShowValue orelse not blank(Label)],
           ?H:el('div', ?H:el('div', [], [<<"ah-meter__fill">>],
                              [{data_state, State},
                               {style, iolist_to_binary([<<"width: ">>,
                                                         num(pct(clamp(Value, Min, Max), Min, Max)),
                                                         <<"%;">>])}]),
                 [<<"ah-meter__track">>],
                 [{role, <<"meter">>}, {aria_valuenow, Value}, {aria_valuemin, Min},
                  {aria_valuemax, Max}, {aria_label, not blank(Label) andalso Label}]),
           [?H:el('div', Helper, [<<"ah-meter__helper">>], []) || not blank(Helper)]],
          Cls, [[{data_size, maps:get(size, P)}], Rest]).

%% sigil's reading of the <meter> thresholds, kept as is.
meter_state(V, Min, Max, Low, High, Opt) ->
    Lo = if Low =:= undefined -> Min; true -> Low end,
    Hi = if High =:= undefined -> Max; true -> High end,
    if Opt =:= undefined ->
           if V < Lo -> low; V > Hi -> high; true -> optimum end;
       Opt > Hi -> if V < Lo -> low; true -> optimum end;
       Opt < Lo -> if V > Hi -> low; true -> optimum end;
       true -> if V < Lo -> low; V > Hi -> high; true -> optimum end
    end.

%% @doc Statistic: title, prefix, number, suffix and a delta arrow.
%% Options: `title', `prefix', `suffix', `precision', `group_separator'
%% (true), `delta'.
-spec statistic(number() | html(), css(), attrs()) -> element().
statistic(Value, Css, Attrs) ->
    {Cls, P, F, O, Rest} = setup(statistic, Css, Attrs),
    Title = opt(title, O, undefined),
    Prefix = opt(prefix, O, undefined),
    Suffix = opt(suffix, O, undefined),
    Prec = opt(precision, O, undefined),
    Group = opt(group_separator, O, true) =/= false,
    Delta = opt(delta, O, undefined),
    Dir = if not is_number(Delta) -> undefined;
             Delta > 0 -> up; Delta < 0 -> down; true -> flat end,
    Arrow = case Dir of up -> <<"▲"/utf8>>; down -> <<"▼"/utf8>>; _ -> <<"—"/utf8>> end,
    ?H:el('div',
          [[?H:el('div', Title, [<<"ah-statistic__title">>], []) || not blank(Title)],
           ?H:el('div', [[?H:el(span, Prefix, [<<"ah-statistic__prefix">>], []) || not blank(Prefix)],
                         ?H:el(span, format_number(Value, Prec, Group),
                               [<<"ah-statistic__number">>], []),
                         [?H:el(span, Suffix, [<<"ah-statistic__suffix">>], []) || not blank(Suffix)]],
                 [<<"ah-statistic__value">>], []),
           [?H:el('div', [?H:el(span, Arrow, [<<"ah-statistic__delta-arrow">>],
                                [{aria_hidden, <<"true">>}]),
                          ?H:el(span, format_number(abs(Delta), Prec, Group), [], [])],
                  [<<"ah-statistic__delta">>], [{data_direction, Dir}])
            || Dir =/= undefined]],
          Cls,
          [[{data_color, maps:get(color, P)},
            {data_loading, tf(lists:member(loading, F))},
            {aria_busy, lists:member(loading, F) andalso <<"true">>}],
           Rest]).

%% @doc KPI card: title, big value and a trend badge. Options: `title',
%% `trend' (percent, > 0 up), `trend_label', `icon' (users, download,
%% install, star, trending_up, trending_down, or html).
-spec kpi_card(html(), css(), attrs()) -> element().
kpi_card(Value, Css, Attrs) ->
    {Cls, _P, _F, O, Rest} = setup(kpi_card, Css, Attrs),
    Title = opt(title, O, undefined),
    Trend = opt(trend, O, undefined),
    TrendL = opt(trend_label, O, undefined),
    Icon = case opt(icon, O, undefined) of
               undefined -> [];
               A when is_atom(A) -> kpi_icon(A);
               Html -> Html
           end,
    TrendCls = case Trend of
                   undefined -> [];
                   T when is_number(T), T > 0 -> <<"ah-kpi-card-trend-up">>;
                   T when is_number(T) -> <<"ah-kpi-card-trend-down">>;
                   T -> error({aihtml, {bad_trend, T}})
               end,
    TrendEl = [?H:el(span,
                     [?H:el(span, [?H:el(span, kpi_icon(if Trend > 0 -> trending_up;
                                                           true -> trending_down end),
                                         [<<"ah-kpi-card-trend-icon">>],
                                         [{aria_hidden, <<"true">>}]),
                                   ?H:el(span, format_trend(Trend),
                                         [<<"ah-kpi-card-trend-value">>], [])],
                           [TrendCls], []),
                      [?H:el(span, TrendL, [<<"ah-kpi-card-trend-label">>], []) || not blank(TrendL)]],
                     [<<"ah-kpi-card-trend">>], [])
               || Trend =/= undefined],
    ?H:el('div',
          ?H:el('div',
                [[?H:el('div', ?H:el(span, Icon, [<<"ah-kpi-card-icon">>], []),
                        [<<"ah-kpi-card-icon-wrapper">>], []) || Icon =/= []],
                 TrendEl,
                 ?H:el('div', ?H:el('div', [[?H:el('div', Title, [<<"ah-kpi-card-title">>], [])
                                             || not blank(Title)],
                                            ?H:el('div', Value, [<<"ah-kpi-card-value">>], [])],
                                    [<<"ah-kpi-card-value-section">>], []),
                       [<<"ah-kpi-card-body">>], [])],
                [<<"ah-kpi-card-content">>], []),
          [Cls, TrendCls], [[{data_ah, <<"kpi-card">>}], Rest]).

format_trend(T) ->
    [if T > 0 -> <<"+">>; true -> <<>> end,
     float_to_binary(float(T), [{decimals, 1}]), <<"%">>].

kpi_icon(trending_up) ->
    svg([polyline(<<"23 6 13.5 15.5 8.5 10.5 1 18">>), polyline(<<"17 6 23 6 23 12">>)]);
kpi_icon(trending_down) ->
    svg([polyline(<<"23 18 13.5 8.5 8.5 13.5 1 6">>), polyline(<<"17 18 23 18 23 12">>)]);
kpi_icon(users) ->
    svg([path(<<"M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2">>), circle(9, 7, 4),
         path(<<"M23 21v-2a4 4 0 0 0-3-3.87">>), path(<<"M16 3.13a4 4 0 0 1 0 7.75">>)]);
kpi_icon(download) ->
    svg([path(<<"M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4">>),
         polyline(<<"7 10 12 15 17 10">>), line(12, 15, 12, 3)]);
kpi_icon(install) ->
    svg([path(<<"M12 2L2 7l10 5 10-5-10-5z">>), path(<<"M2 17l10 5 10-5">>),
         path(<<"M2 12l10 5 10-5">>)]);
kpi_icon(star) ->
    svg([?H:el(polygon, [], [], [{points, <<"12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 "
                                            "12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26 12 2">>}])]);
kpi_icon(Other) -> error({aihtml, {unknown_icon, Other}}).

%% @doc Timeline. `Items' are maps with `date', `title', `subtitle',
%% `icon' (html), `description', `dot' (primary | success | warning |
%% danger) and `expanded'. Items with a description expand on click
%% unless the `collapsible' option is false.
-spec timeline([map()], css(), attrs()) -> element().
timeline(Items, Css, Attrs) ->
    {Cls, P, _F, O, Rest} = setup(timeline, Css, Attrs),
    Position = maps:get(position, P),
    Collapsible = opt(collapsible, O, true) =/= false,
    Rows = [timeline_row(I, Item, Position, Collapsible)
            || {I, Item} <- lists:enumerate(0, Items)],
    ?H:el('div', ?H:el('div', Rows, [<<"ah-timeline-container">>], []),
          [Cls, [<<"ah-collapsible">> || Collapsible]],
          [[{data_ah, <<"timeline">>}], Rest]).

timeline_row(Idx, Item, Position, Collapsible) ->
    Side = case Position of
               both when Idx rem 2 =:= 0 -> far;
               both -> near;
               S -> S
           end,
    Desc = maps:get(description, Item, undefined),
    CanToggle = Collapsible andalso not blank(Desc),
    Expanded = maps:get(expanded, Item, false) =:= true,
    Icon = maps:get(icon, Item, undefined),
    Sub = maps:get(subtitle, Item, undefined),
    Card = ?H:el('div',
                 [?H:el('div', [], [<<"ah-timeline-item-pointer">>], []),
                  ?H:el('div',
                        [?H:el('div',
                               [[?H:el('div', Icon, [<<"ah-timeline-item-icon">>], [])
                                 || not blank(Icon)],
                                ?H:el('div', [?H:el('div', maps:get(title, Item, <<>>),
                                                    [<<"ah-timeline-item-title">>], []),
                                              [?H:el('div', Sub, [<<"ah-timeline-item-subtitle">>], [])
                                               || not blank(Sub)]], [], [])],
                               [<<"ah-timeline-item-header">>], []),
                         [?H:el('div', Desc, [<<"ah-timeline-item-description">>], [])
                          || not blank(Desc)]],
                        [<<"ah-timeline-item-content">>], [])],
                 [<<"ah-timeline-item">>, [<<"ah-timeline-item-expanded">> || Expanded]],
                 [{<<"ah-collapsible">>, CanToggle}, {role, CanToggle andalso <<"button">>},
                  {tabindex, CanToggle andalso 0},
                  {aria_expanded, CanToggle andalso atom_to_binary(Expanded)}]),
    Date = ?H:el('div', maps:get(date, Item, <<>>), [<<"ah-timeline-date">>], []),
    Dot = case maps:get(dot, Item, undefined) of
              undefined -> [];
              D when D =:= primary; D =:= success; D =:= warning; D =:= danger ->
                  <<"ah-timeline-dot-", (atom_to_binary(D))/binary>>;
              D -> error({aihtml, {bad_dot, D}})
          end,
    [?H:el('div', if Side =:= near -> Card; true -> Date end, [<<"ah-timeline-near-cell">>], []),
     ?H:el('div', ?H:el('div', [], [<<"ah-timeline-dot">>, Dot], []),
           [<<"ah-timeline-track-cell">>], []),
     ?H:el('div', if Side =:= far -> Card; true -> Date end, [<<"ah-timeline-far-cell">>], [])].

%% @doc Ranking list. `Items' are maps with `name', `value', and
%% optionally `rank', `secondary', `sub_value', `code' (country code,
%% shown as a flag), `tag', `attrs' (on the row). Options: `title',
%% `max_items', `show_rank' (true), `flag_style' (emoji | flag_icons |
%% none), `tag_colors' (#{Tag => success | warning | error | info}).
%% `clickable' rows fire `ah:item-click' with the row index.
-spec ranking_list([map()], css(), attrs()) -> element().
ranking_list(Items, Css, Attrs) ->
    {Cls, _P, F, O, Rest} = setup(ranking_list, Css, Attrs),
    Title = opt(title, O, undefined),
    Shown = case opt(max_items, O, undefined) of
                N when is_integer(N), N >= 0 -> lists:sublist(Items, N);
                _ -> Items
            end,
    Ctx = #{rank => opt(show_rank, O, true) =/= false,
            flags => opt(flag_style, O, emoji),
            tags => opt(tag_colors, O, #{}),
            click => lists:member(clickable, F)},
    ?H:el('div',
          [[?H:el('div', ?H:el(span, Title, [<<"ah-ranking-list__title">>], []),
                  [<<"ah-ranking-list__header">>], []) || not blank(Title)],
           ?H:el('div', [ranking_item(I, It, Ctx) || {I, It} <- lists:enumerate(0, Shown)],
                 [<<"ah-ranking-list__list">>], [])],
          Cls, [[{data_ah, <<"ranking-list">>}], Rest]).

ranking_item(Idx, It, #{rank := ShowRank, flags := FlagStyle, tags := TagColors,
                        click := Click}) ->
    Rank = maps:get(rank, It, Idx + 1),
    Name = first_of([name, primary, country], It),
    Sec = maps:get(secondary, It, undefined),
    Sub = maps:get(sub_value, It, undefined),
    Tag = maps:get(tag, It, undefined),
    ?H:el('div',
          [[?H:el(span, Rank, [<<"ah-ranking-list__rank">>], [{data_rank, Rank}]) || ShowRank],
           flag(maps:get(code, It, undefined), FlagStyle),
           ?H:el('div', [?H:el(span, Name, [<<"ah-ranking-list__primary">>], []),
                         [?H:el(span, Sec, [<<"ah-ranking-list__secondary">>], []) || not blank(Sec)]],
                 [<<"ah-ranking-list__content">>], []),
           ?H:el('div', [?H:el(span, maps:get(value, It, <<>>), [<<"ah-ranking-list__value">>], []),
                         [?H:el(span, Sub, [<<"ah-ranking-list__sub-value">>], []) || not blank(Sub)]],
                 [<<"ah-ranking-list__values">>], []),
           [?H:el(span, Tag, [<<"ah-ranking-list__tag">>, tag_class(Tag, TagColors)], [])
            || not blank(Tag)]],
          [<<"ah-ranking-list__item">>, [<<"ah-ranking-list__item--clickable">> || Click]],
          [[{data_idx, Idx}, {role, Click andalso <<"button">>}, {tabindex, Click andalso 0}],
           maps:get(attrs, It, [])]).

first_of([K | Ks], M) ->
    case maps:get(K, M, undefined) of undefined -> first_of(Ks, M); V -> V end;
first_of([], _) -> <<>>.

flag(undefined, _) -> [];
flag(_, none) -> [];
flag(Code, flag_icons) ->
    ?H:el(span, ?H:el(span, [], [<<"fi fi-">>, [to_bin(Code)]], []),
          [<<"ah-ranking-list__flag ah-ranking-list__flag--img">>], []);
flag(Code, emoji) ->
    ?H:el(span, emoji_flag(to_bin(Code)),
          [<<"ah-ranking-list__flag ah-ranking-list__flag--emoji">>], [{aria_hidden, <<"true">>}]);
flag(_, Other) -> error({aihtml, {bad_flag_style, Other}}).

%% Regional indicator symbols: "de" -> 🇩🇪. Anything else is shown as is.
emoji_flag(Code) ->
    case string:lowercase(Code) of
        <<A, B>> when A >= $a, A =< $z, B >= $a, B =< $z ->
            unicode:characters_to_binary([16#1F1E6 + A - $a, 16#1F1E6 + B - $a]);
        _ -> Code
    end.

tag_class(Tag, Colors) ->
    Defaults = #{<<"Free">> => success, <<"Paid">> => warning,
                 <<"Progress">> => info, <<"Out of date">> => error},
    Key = to_bin(Tag),
    C = maps:get(Key, Colors, maps:get(Key, Defaults, info)),
    lists:member(C, [success, warning, error, info])
        orelse error({aihtml, {bad_tag_color, C}}),
    <<"ah-ranking-list__tag--", (atom_to_binary(C))/binary>>.

%% @doc Tag cloud, font size weighted by value. `Tags' are maps with
%% `label', `value', `url' (or `{Label, Value}' tuples). Options:
%% `min_font_size' (10), `max_font_size' (24), `font_size_unit' (px),
%% `url_base', `display_value', `sort_by' (none | label | value),
%% `sort_order' (ascending | descending), `text_case' (none | all_lower |
%% all_upper | first_upper | title_case), `text_color', `min_color' and
%% `max_color' (#RRGGBB gradient), `min_value', `max_value',
%% `display_limit', `take_top_weighted'. Fires `ah:tag-click'.
-spec tag_cloud([map() | {html(), number()}], css(), attrs()) -> element().
tag_cloud(Tags0, Css, Attrs) ->
    {Cls, _P, _F, O, Rest} = setup(tag_cloud, Css, Attrs),
    Tags = sort_tags(filter_tags([tag_map(T) || T <- Tags0], O), O),
    Values = [V || #{value := V} <- Tags],
    {Lo, Hi} = case Values of [] -> {0, 0}; _ -> {lists:min(Values), lists:max(Values)} end,
    Range = Hi - Lo,
    MinF = opt(min_font_size, O, 10),
    MaxF = opt(max_font_size, O, 24),
    Unit = unit(opt(font_size_unit, O, px)),
    Grad = {opt(min_color, O, undefined), opt(max_color, O, undefined)},
    Fixed = opt(text_color, O, undefined),
    Base = opt(url_base, O, <<>>),
    Case = opt(text_case, O, none),
    ShowValue = opt(display_value, O, false) =:= true,
    Items = [begin
                 Ratio = if Range == 0 -> 0.5; true -> (V - Lo) / Range end,
                 Font = MinF + (MaxF - MinF) * Ratio,
                 Color = case Grad of
                             {C1, C2} when C1 =/= undefined, C2 =/= undefined -> lerp_color(C1, C2, Ratio);
                             _ when Fixed =/= undefined -> color_css(Fixed);
                             _ -> undefined
                         end,
                 Text = alter_case(if ShowValue -> [to_bin(L), <<" (">>, num(V), <<")">>];
                                      true -> to_bin(L) end, Case),
                 Style = iolist_to_binary([<<"font-size: ">>, num(Font), Unit, <<";">>,
                                           [[<<" color: ">>, Color, <<";">>] || Color =/= undefined]]),
                 ?H:el(li, ?H:el(a, Text, [<<"ah-tagcloud-link">>],
                                 [{style, Style},
                                  {href, Url =/= undefined andalso iolist_to_binary([Base, Url])},
                                  {tabindex, Url =:= undefined andalso 0},
                                  {role, Url =:= undefined andalso <<"button">>},
                                  {data_ah_label, L}, {data_ah_weight, V}]),
                       [<<"ah-tagcloud-item">>], [{data_index, I}])
             end || {I, #{label := L, value := V, url := Url}} <- lists:enumerate(0, Tags)],
    ?H:el('div', ?H:el(ul, Items, [<<"ah-tagcloud">>], []), Cls,
          [[{data_ah, <<"tag-cloud">>}], Rest]).

tag_map({L, V}) -> tag_map(#{label => L, value => V});
tag_map({L, V, U}) -> tag_map(#{label => L, value => V, url => U});
tag_map(#{label := L} = M) ->
    V = maps:get(value, M, 0),
    is_number(V) orelse error({aihtml, {bad_tag_value, V}}),
    #{label => L, value => V, url => maps:get(url, M, undefined)};
tag_map(Other) -> error({aihtml, {bad_tag, Other}}).

filter_tags(Tags, O) ->
    Min = opt(min_value, O, 0),
    Max = opt(max_value, O, 0),
    T1 = [T || #{value := V} = T <- Tags, not (Min > 0) orelse V >= Min],
    T2 = [T || #{value := V} = T <- T1, not (Max > 0) orelse V =< Max],
    case opt(display_limit, O, undefined) of
        N when is_integer(N), N > 0, length(T2) > N ->
            case opt(take_top_weighted, O, false) of
                true ->
                    Top = lists:sublist(lists:sort(fun({_, #{value := A}}, {_, #{value := B}}) ->
                                                           A >= B end,
                                                   lists:enumerate(T2)), N),
                    [T || {_, T} <- lists:keysort(1, Top)];
                _ -> lists:sublist(T2, N)
            end;
        _ -> T2
    end.

sort_tags(Tags, O) ->
    Key = case opt(sort_by, O, none) of
              none -> none;
              label -> fun(#{label := L}) -> string:lowercase(to_bin(L)) end;
              value -> fun(#{value := V}) -> V end;
              Other -> error({aihtml, {bad_sort_by, Other}})
          end,
    case Key of
        none -> Tags;
        _ ->
            Sorted = [T || {_, T} <- lists:keysort(1, [{Key(T), T} || T <- Tags])],
            case opt(sort_order, O, ascending) of
                descending -> lists:reverse(Sorted);
                _ -> Sorted
            end
    end.

alter_case(Text, Mode) ->
    B = iolist_to_binary(Text),
    case Mode of
        none -> B;
        all_lower -> string:lowercase(B);
        all_upper -> string:uppercase(B);
        first_upper -> first_upper(B);
        title_case -> iolist_to_binary(lists:join(<<" ">>, [first_upper(W) || W <- binary:split(B, <<" ">>, [global])]));
        Other -> error({aihtml, {bad_text_case, Other}})
    end.

first_upper(<<>>) -> <<>>;
first_upper(B) ->
    [G | Rest] = string:next_grapheme(B),
    iolist_to_binary([string:uppercase([G]), Rest]).

unit(U) when is_atom(U); is_binary(U) ->
    B = to_bin(U),
    lists:member(B, [<<"px">>, <<"em">>, <<"rem">>, <<"pt">>, <<"%">>])
        orelse error({aihtml, {bad_unit, U}}),
    B.

lerp_color(C1, C2, R) ->
    [R1, G1, B1] = hex(C1),
    [R2, G2, B2] = hex(C2),
    L = fun(A, B) -> integer_to_binary(round(A + (B - A) * R)) end,
    iolist_to_binary([<<"rgb(">>, L(R1, R2), <<",">>, L(G1, G2), <<",">>, L(B1, B2), <<")">>]).

hex(C) ->
    case to_bin(C) of
        <<"#", H:6/binary>> -> hex6(H, C);
        <<H:6/binary>> -> hex6(H, C);
        _ -> error({aihtml, {bad_color, C}})
    end.

hex6(<<R:2/binary, G:2/binary, B:2/binary>>, C) ->
    try [binary_to_integer(X, 16) || X <- [R, G, B]]
    catch error:badarg -> error({aihtml, {bad_color, C}})
    end.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => avatar, category => media, root => <<"ah-avatar">>,
       signature => <<"avatar(Content, Css, Attrs)">>,
       groups => #{size => {[sm, md, lg, xl], md},
                   shape => {[circle, square, rounded], circle},
                   color => {?COLORS, primary}},
       classes => none_for([sm, md, lg, xl, circle, square, rounded | ?COLORS]),
       options => [src, alt], behavior => <<"avatar">>,
       doc => <<"Image avatar with an initials fallback (Content) when src is "
                "missing or fails to load.">>},
     #{name => badge, category => media, root => <<"ah-badge-root">>,
       signature => <<"badge(Anchor, Css, Attrs)">>,
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
                "when Anchor is undefined. Method setCount(n).">>},
     #{name => chip, category => media, root => <<"ah-chip">>,
       signature => <<"chip(Content, Css, Attrs)">>,
       groups => #{variant => {[filled, outlined, soft], filled},
                   color => {[default | ?COLORS], default},
                   size => {[small, medium], medium}},
       flags => [removable, clickable, disabled],
       classes => none_for([filled, outlined, soft, default, small, medium,
                            removable, clickable, disabled | ?COLORS]),
       options => [avatar, icon, value], behavior => <<"chip">>,
       events => [<<"ah:remove">>, <<"change">>, <<"click">>],
       doc => <<"Compact label; removable chips fire ah:remove and change, then "
                "remove themselves.">>},
     #{name => aspect_ratio, category => media, root => <<"ah-aspect-ratio">>,
       signature => <<"aspect_ratio(Children, Css, Attrs)">>,
       options => [ratio],
       doc => <<"Locks Children to a ratio (\"16/9\", \"4:3\", 1.5 or {W, H}).">>},
     #{name => kbd, category => text, root => <<"ah-kbd">>,
       signature => <<"kbd(Keys, Css, Attrs)">>,
       groups => #{size => {[md, lg], md}},
       classes => none_for([md, lg]), options => [separator],
       doc => <<"Keyboard key, or a key combination when Keys is a list.">>},
     #{name => time_ago, category => text, root => <<"ah-time-ago">>,
       signature => <<"time_ago(Timestamp, Css, Attrs)">>,
       options => [now, labels, live, title], behavior => <<"time-ago">>,
       doc => <<"Relative time (\"3m ago\") from Unix seconds or a UTC datetime; "
                "updated every 60 s. Methods setDate(iso), refresh().">>},
     #{name => expandable_text, category => text, root => <<"ah-expandable-text">>,
       signature => <<"expandable_text(Text, Css, Attrs)">>,
       options => [threshold, expanded, expand_label, collapse_label],
       behavior => <<"expandable-text">>, events => [<<"ah:toggle">>],
       doc => <<"Text cut at threshold characters with an expand/collapse toggle. "
                "Methods toggle(), expand(), collapse().">>},
     #{name => alert, category => text, root => <<"ah-alert">>,
       signature => <<"alert(Children, Css, Attrs)">>,
       groups => #{variant => {[info, success, warning, error], info}},
       flags => [dismissible], options => [title, icon], behavior => <<"alert">>,
       events => [<<"ah:dismiss">>],
       doc => <<"Inline message box; dismissible alerts fire ah:dismiss and remove "
                "themselves. Method dismiss().">>},
     #{name => progressbar, category => data, root => <<"ah-progressbar">>,
       signature => <<"progressbar(Value, Css, Attrs)">>,
       groups => #{orientation => {[horizontal, vertical], horizontal},
                   layout => {[normal, reverse], normal},
                   color => {[primary, success, warning, error, info], primary}},
       flags => [show_text, disabled, indeterminate, striped, animated],
       classes => #{normal => [], primary => [], show_text => []},
       options => [min, max, text, color_ranges], behavior => <<"progressbar">>,
       events => [<<"change">>, <<"ah:complete">>],
       doc => <<"Linear progress with optional text, colour ranges, stripes and an "
                "indeterminate state. Methods setValue(v[, text]), getValue().">>},
     #{name => progress_circle, category => data, root => <<"ah-progress-circle">>,
       signature => <<"progress_circle(Value, Css, Attrs)">>,
       groups => #{size => {[sm, md, lg], md},
                   color => {[primary, success, warning, info, error], primary}},
       flags => [disabled, indeterminate],
       classes => maps:merge(
                    maps:from_list([{M, [<<"ah-progress-circle--", (atom_to_binary(M))/binary>>]}
                                    || M <- [sm, md, lg, primary, success, warning, info,
                                             error, indeterminate]]),
                    #{disabled => [<<"ah-progress-circle-disabled">>]}),
       options => [label, show_value], behavior => <<"progress-circle">>,
       events => [<<"change">>, <<"ah:complete">>],
       doc => <<"SVG progress ring, 0-100. Method setValue(v).">>},
     #{name => meter, category => data, root => <<"ah-meter">>,
       signature => <<"meter(Value, Css, Attrs)">>,
       groups => #{size => {[sm, md, lg], md}},
       classes => none_for([sm, md, lg]),
       options => [min, max, low, high, optimum, label, helper_text, show_value],
       doc => <<"Measurement in a range, coloured low / optimum / high.">>},
     #{name => statistic, category => data, root => <<"ah-statistic">>,
       signature => <<"statistic(Value, Css, Attrs)">>,
       groups => #{color => {[default, primary, success, warning, error], default}},
       flags => [loading],
       classes => none_for([default, primary, success, warning, error, loading]),
       options => [title, prefix, suffix, precision, group_separator, delta],
       doc => <<"A number with title, prefix/suffix, grouping and a delta arrow.">>},
     #{name => kpi_card, category => data, root => <<"ah-kpi-card">>,
       signature => <<"kpi_card(Value, Css, Attrs)">>,
       groups => #{color => {[primary, success, warning, info, error], none}},
       flags => [disabled],
       classes => maps:from_list([{M, [<<"ah-kpi-card--", (atom_to_binary(M))/binary>>]}
                                  || M <- [primary, success, warning, info, error]]),
       options => [title, trend, trend_label, icon], behavior => <<"kpi-card">>,
       doc => <<"Metric card with value and trend. Methods setValue(text), "
                "setTrend(pct).">>},
     #{name => timeline, category => data, root => <<"ah-timeline">>,
       signature => <<"timeline(Items, Css, Attrs)">>,
       groups => #{position => {[both, near, far], both}},
       flags => [horizontal, disabled],
       classes => maps:from_list([{M, [<<"ah-timeline-position-", (atom_to_binary(M))/binary>>]}
                                  || M <- [both, near, far]]),
       options => [collapsible], behavior => <<"timeline">>,
       events => [<<"ah:toggle">>],
       doc => <<"Events along an axis, cards alternating (both) or on one side; "
                "cards with a description expand on click.">>},
     #{name => ranking_list, category => data, root => <<"ah-ranking-list">>,
       signature => <<"ranking_list(Items, Css, Attrs)">>,
       flags => [dense, disabled, clickable],
       classes => #{dense => [<<"ah-ranking-list--dense">>],
                    disabled => [<<"ah-ranking-list--disabled">>], clickable => []},
       options => [title, max_items, show_rank, flag_style, tag_colors],
       behavior => <<"ranking-list">>, events => [<<"ah:item-click">>],
       doc => <<"Top N list with rank medals, flags, values and tags.">>},
     #{name => tag_cloud, category => data, root => <<"ah-tagcloud">>,
       signature => <<"tag_cloud(Tags, Css, Attrs)">>,
       flags => [disabled],
       options => [min_font_size, max_font_size, font_size_unit, url_base, display_value,
                   sort_by, sort_order, text_case, text_color, min_color, max_color,
                   min_value, max_value, display_limit, take_top_weighted],
       behavior => <<"tag-cloud">>, events => [<<"ah:tag-click">>],
       doc => <<"Tags sized (and optionally coloured) by weight. Methods "
                "hideItem(i), showItem(i).">>}].

none_for(Mods) -> maps:from_list([{M, []} || M <- Mods]).

%%%===================================================================
%%% Examples
%%%===================================================================

-spec examples() -> [{atom(), binary(), aihtml_html:html()}].
examples() ->
    Row = fun(Items) -> ?H:el('div', Items, [<<"flex flex-wrap items-center gap-4">>], []) end,
    Col = fun(Items) -> ?H:el('div', Items, [<<"flex flex-col gap-3">>], []) end,
    Img = <<"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 40 40'>"
            "<rect width='40' height='40' fill='%2360a5fa'/><circle cx='20' cy='16' r='7' fill='white'/>"
            "<rect x='8' y='26' width='24' height='14' rx='7' fill='white'/></svg>">>,
    Now = erlang:system_time(second),
    Long = <<"aihtml renders every page on the server as Erlang function calls; "
             "jQuery only adds behaviour. This paragraph is long enough to be cut "
             "at the threshold and shows a toggle to read the rest.">>,
    [{avatar, <<"sizes, shapes, colours, image and broken image">>,
      Col([Row([avatar(<<"SG">>, [S], []) || S <- [sm, md, lg, xl]]),
           Row([avatar(<<"AB">>, [square], []), avatar(<<"CD">>, [rounded, success], []),
                avatar(<<"EF">>, [warning], []), avatar(<<"GH">>, [error], []),
                avatar(<<"IJ">>, [info], []), avatar(<<"KL">>, [secondary], []),
                avatar(undefined, [], [])]),
           Row([avatar(<<"IM">>, [lg], [{src, Img}, {alt, <<"User">>}]),
                avatar(<<"BR">>, [lg], [{src, <<"data:image/png;base64,AAAA">>}, {alt, <<"Broken">>}])])])},
     {badge, <<"counts, max, dot, status, corners, standalone">>,
      Row([badge(avatar(<<"A">>, [square], []), [], [{count, 5}]),
           badge(avatar(<<"B">>, [square], []), [error], [{count, 120}]),
           badge(avatar(<<"C">>, [square], []), [success, show_zero], [{count, 0}]),
           badge(avatar(<<"D">>, [square], []), [dot, warning], []),
           badge(avatar(<<"E">>, [], []), [online, circular, bottom], []),
           badge(avatar(<<"F">>, [], []), [busy, circular, bottom], []),
           badge(avatar(<<"G">>, [], []), [away, circular, bottom], []),
           badge(avatar(<<"H">>, [], []), [offline, circular, bottom, left], []),
           badge(avatar(<<"I">>, [square], []), [info, bottom, left], [{count, <<"new">>}]),
           badge(undefined, [secondary], [{count, 42}]),
           badge(undefined, [success], [{count, <<"beta">>}])])},
     {chip, <<"variants × colours, sizes, avatar, removable, clickable, disabled"/utf8>>,
      Col([Row([chip(atom_to_binary(C), [V, C], [])
                || C <- [default, primary, success, warning, error, info]])
           || V <- [filled, outlined, soft]] ++
          [Row([chip(<<"Small">>, [small, primary], []),
                chip(<<"Jane Doe">>, [soft, primary], [{avatar, <<"JD">>}]),
                chip(<<"Removable">>, [removable, outlined, info], [{id, <<"chip-remove">>}]),
                chip(<<"Clickable">>, [clickable, soft, success], []),
                chip(<<"Disabled">>, [disabled, primary], [])])])},
     {aspect_ratio, <<"16/9, 4:3 and 1/1">>,
      Row([?H:el('div', aspect_ratio(?H:el('div', R, [<<"w-full h-full flex items-center justify-center "
                                                          "bg-primary/15 text-primary">>], []),
                                     [], [{ratio, R}]),
                 [<<"w-48">>], []) || R <- [<<"16/9">>, <<"4:3">>, <<"1/1">>]])},
     {kbd, <<"keys, sizes and combinations">>,
      Row([kbd(<<"Esc">>, [], []), kbd(<<"⌘"/utf8>>, [], []), kbd(<<"Enter">>, [lg], []),
           kbd([<<"Ctrl">>, <<"Shift">>, <<"P">>], [], []),
           kbd([<<"⌘"/utf8>>, <<"K">>], [lg], [])])},
     {time_ago, <<"just now, minutes, hours, days, months, custom labels">>,
      Row([time_ago(Now - S, [], [{id, Id}]) || {Id, S} <- [{<<"ta-now">>, 5}, {<<"ta-min">>, 180},
                                                             {<<"ta-h">>, 7200}, {<<"ta-d">>, 3 * 86400},
                                                             {<<"ta-mo">>, 90 * 86400}]] ++
          [time_ago(Now - 600, [], [{labels, #{minutes => <<"{n} 分钟前"/utf8>>}}]),
           time_ago({{2026, 1, 1}, {0, 0, 0}}, [<<"text-muted">>], [{live, false}])])},
     {expandable_text, <<"collapsed, expanded, short">>,
      Col([expandable_text(Long, [], [{threshold, 60}, {id, <<"et-demo">>}]),
           expandable_text(Long, [], [{threshold, 60}, {expanded, true},
                                      {expand_label, <<"Show more">>},
                                      {collapse_label, <<"Show less">>}]),
           expandable_text(<<"Short text is shown as is.">>, [], [])])},
     {progressbar, <<"values, text, colours, ranges, stripes, indeterminate, vertical">>,
      Col([progressbar(35, [show_text], [{id, <<"pb-demo">>}]),
           progressbar(70, [success, striped, animated, show_text], []),
           progressbar(90, [warning, show_text], [{text, <<"9 of 10 files">>}]),
           progressbar(80, [show_text], [{color_ranges, [{30, success}, {60, warning}, {100, error}]}]),
           progressbar(40, [reverse, info], []),
           progressbar(undefined, [indeterminate], [{aria_label, <<"Loading">>}]),
           progressbar(50, [disabled, show_text], []),
           Row([progressbar(V, [vertical, show_text], [{style, <<"height: 120px">>}])
                || V <- [20, 60]] ++
               [progressbar(60, [vertical, reverse, error, show_text],
                            [{style, <<"height: 120px">>}])])])},
     {progress_circle, <<"sizes, colours, label, indeterminate, disabled">>,
      Row([progress_circle(25, [sm], []), progress_circle(50, [], [{id, <<"pc-demo">>}]),
           progress_circle(75, [lg, success], [{label, <<"Uploaded">>}]),
           progress_circle(40, [warning], []), progress_circle(90, [error], []),
           progress_circle(60, [info], [{show_value, false}, {label, <<"No value">>}]),
           progress_circle(undefined, [indeterminate], [{label, <<"Working">>}]),
           progress_circle(30, [disabled], [])])},
     {meter, <<"optimum, low, high, sizes, helper text">>,
      Col([meter(62, [], [{low, 25}, {high, 75}, {label, <<"Usage">>}, {show_value, true}]),
           meter(15, [sm], [{low, 25}, {high, 75}, {label, <<"Battery">>}, {show_value, true},
                            {optimum, 90}, {helper_text, <<"Low: charge soon">>}]),
           meter(88, [lg], [{low, 25}, {high, 75}, {label, <<"CPU">>}, {show_value, true}])])},
     {statistic, <<"prefix, suffix, precision, delta, colours, loading">>,
      Row([statistic(1284500, [primary], [{title, <<"Revenue">>}, {prefix, <<"¥"/utf8>>}]),
           statistic(98.456, [success], [{title, <<"Uptime">>}, {suffix, <<"%">>},
                                         {precision, 2}, {delta, 0.4}]),
           statistic(-3250.5, [error], [{title, <<"Balance">>}, {precision, 1}, {delta, -120}]),
           statistic(42, [], [{title, <<"Tickets">>}, {delta, 0}]),
           statistic(0, [loading], [{title, <<"Loading">>}])])},
     {kpi_card, <<"colours, trends, icons">>,
      ?H:el('div',
            [kpi_card(<<"12,480">>, [], [{title, <<"Active users">>}, {trend, 5.2},
                                         {trend_label, <<"vs last month">>}, {icon, users},
                                         {id, <<"kpi-demo">>}]),
             kpi_card(<<"3,210">>, [success], [{title, <<"Downloads">>}, {trend, 12},
                                               {icon, download}]),
             kpi_card(<<"845">>, [warning], [{title, <<"Installs">>}, {trend, -3.5},
                                             {trend_label, <<"vs last week">>}, {icon, install}]),
             kpi_card(<<"4.8">>, [error], [{title, <<"Rating">>}, {icon, star}]),
             kpi_card(<<"—"/utf8>>, [info, disabled], [{title, <<"Disabled">>}])],
            [<<"grid grid-cols-3 gap-4">>], [])},
     {timeline, <<"both sides, near, horizontal">>,
      Col([timeline(timeline_items(), [], [{id, <<"tl-demo">>}]),
           timeline(lists:sublist(timeline_items(), 2), [near], []),
           timeline(timeline_items(), [horizontal], [{collapsible, false}])])},
     {ranking_list, <<"flags, tags, medals, dense, clickable">>,
      ?H:el('div',
            [ranking_list(ranking_items(), [], [{title, <<"Top countries">>}]),
             ranking_list(ranking_items(), [dense, clickable],
                          [{title, <<"Dense, clickable, max 3">>}, {max_items, 3},
                           {flag_style, none}])],
            [<<"grid grid-cols-2 gap-4">>], [])},
     {tag_cloud, <<"weights, gradient, sorting, values">>,
      Col([tag_cloud(tag_items(), [], [{id, <<"tc-demo">>}]),
           tag_cloud(tag_items(), [], [{min_color, <<"#93c5fd">>}, {max_color, <<"#1e3a8a">>},
                                       {max_font_size, 32}, {sort_by, value},
                                       {sort_order, descending}]),
           tag_cloud(tag_items(), [], [{display_value, true}, {text_case, all_upper},
                                       {display_limit, 4}, {take_top_weighted, true}])])},
     {alert, <<"variants, title, dismissible">>,
      Col([alert(<<"A new version is available.">>, [], [{id, <<"alert-demo">>}]),
           alert(<<"Your changes were saved.">>, [success, dismissible],
                 [{title, <<"Saved">>}, {id, <<"alert-dismiss">>}]),
           alert(<<"Your trial ends in 3 days.">>, [warning, dismissible],
                 [{title, <<"Heads up">>}]),
           alert([<<"Could not reach the server. ">>, ?H:el(a, <<"Retry">>, [<<"underline">>],
                                                            [{href, <<"#">>}])],
                 [error], [{title, <<"Connection failed">>}]),
           alert(<<"No icon, plain message.">>, [], [{icon, false}])])}].

timeline_items() ->
    [#{date => <<"2026-01">>, title => <<"Project start">>, subtitle => <<"Kick-off">>,
       description => <<"Scope agreed, team formed.">>},
     #{date => <<"2026-03">>, title => <<"Alpha">>, dot => success,
       description => <<"First internal release.">>, expanded => true},
     #{date => <<"2026-06">>, title => <<"Beta">>, dot => warning},
     #{date => <<"2026-09">>, title => <<"Launch">>, dot => danger, subtitle => <<"GA">>}].

ranking_items() ->
    [#{name => <<"Germany">>, code => de, value => <<"12,300">>, sub_value => <<"+4%">>,
       tag => <<"Free">>},
     #{name => <<"United States">>, code => us, value => <<"9,870">>, tag => <<"Paid">>},
     #{name => <<"Japan">>, code => jp, value => <<"7,450">>, secondary => <<"Asia">>,
       tag => <<"Progress">>},
     #{name => <<"Brazil">>, code => br, value => <<"3,120">>, tag => <<"Out of date">>},
     #{name => <<"<Other>">>, value => <<"980">>}].

tag_items() ->
    [#{label => <<"Erlang">>, value => 40}, #{label => <<"jQuery">>, value => 25},
     #{label => <<"CSS">>, value => 15}, #{label => <<"sigil">>, value => 30},
     #{label => <<"OTP">>, value => 35, url => <<"#otp">>}, #{label => <<"html">>, value => 8},
     #{label => <<"Tailwind">>, value => 20}].

%%%===================================================================
%%% Internal
%%%===================================================================

%% {Classes, #{Group => Modifier}, Flags, Options, OtherAttrs}
setup(Name, Css, Attrs) ->
    E = aihtml_catalog:entry(?MODULE, Name),
    Classes = aihtml_catalog:classes(E, Css),
    #{groups := Groups} = E,
    Mods = [M || M <- lists:flatten([Css]), is_atom(M)],
    Picks = maps:map(fun(_G, {Ms, Default}) ->
                             case [M || M <- Mods, lists:member(M, Ms)] of
                                 [] -> Default;
                                 L -> lists:last(L)
                             end
                     end, Groups),
    {Opts, Rest} = aihtml_catalog:split_options(E, Attrs),
    {Classes, Picks, aihtml_catalog:flags(E, Css), Opts, Rest}.

opt(K, Opts, Default) -> maps:get(K, Opts, Default).

tf(true) -> <<"true">>;
tf(false) -> <<"false">>.

blank(undefined) -> true;
blank(null) -> true;
blank(<<>>) -> true;
blank([]) -> true;
blank(_) -> false.

clamp(V, Lo, Hi) when is_number(V) -> max(Lo, min(Hi, V));
clamp(V, _, _) -> error({aihtml, {bad_number, V}}).

pct(V, Min, Max) when Max > Min -> 100 * (V - Min) / (Max - Min);
pct(_, _, _) -> 0.

%% A number for CSS or SVG: integers as is, floats with at most four
%% decimals and no trailing zeros.
num(N) when is_integer(N) -> integer_to_binary(N);
num(F) when is_float(F) ->
    case float_to_binary(F, [{decimals, 4}, compact]) of
        <<"-0.0">> -> <<"0">>;
        B -> case binary:split(B, <<".0">>) of
                 [I, <<>>] -> I;
                 _ -> B
             end
    end.

parse_num(B) ->
    case string:to_integer(B) of
        {I, <<>>} -> I;
        _ -> case string:to_float(B) of
                 {F, <<>>} -> F;
                 _ -> error
             end
    end.

to_bin(B) when is_binary(B) -> B;
to_bin(A) when is_atom(A) -> atom_to_binary(A);
to_bin(I) when is_integer(I) -> integer_to_binary(I);
to_bin(F) when is_float(F) -> num(F);
to_bin(L) when is_list(L) -> unicode:characters_to_binary(L).

dash(A) -> binary:replace(to_bin(A), <<"_">>, <<"-">>, [global]).

%% sigil's statistic number: precision (toFixed) and thousands separators.
format_number(V, Prec, Group) when is_number(V) ->
    Fixed = case Prec of
                P when is_integer(P), P >= 0 -> float_to_binary(float(V), [{decimals, P}]);
                _ when is_integer(V) -> integer_to_binary(V);
                _ -> num_js(V)
            end,
    {Sign, Digits} = case Fixed of
                         <<"-", D/binary>> -> {<<"-">>, D};
                         D -> {<<>>, D}
                     end,
    {Int, Dec} = case binary:split(Digits, <<".">>) of
                     [I] -> {I, <<>>};
                     [I, F] -> {I, <<".", F/binary>>}
                 end,
    iolist_to_binary([Sign, if Group -> group3(Int); true -> Int end, Dec]);
format_number(V, _Prec, _Group) -> V.

%% JavaScript's String(number): integral floats print without a fraction.
num_js(F) when F == trunc(F), abs(F) < 1.0e21 -> integer_to_binary(trunc(F));
num_js(F) -> float_to_binary(F, [short]).

group3(Int) ->
    N = byte_size(Int),
    case N =< 3 of
        true -> Int;
        false ->
            Head = N rem 3,
            <<H:Head/binary, Tail/binary>> = Int,
            Chunks = [C || <<C:3/binary>> <= Tail],
            iolist_to_binary(lists:join(<<",">>, [H || H =/= <<>>] ++ Chunks))
    end.

%% A colour for an inline style: a theme colour atom or a plain CSS colour.
color_css(A) when is_atom(A) ->
    lists:member(A, [primary, secondary, success, warning, error, info])
        orelse error({aihtml, {bad_color, A}}),
    <<"var(--ah-color-", (atom_to_binary(A))/binary, ")">>;
color_css(C) ->
    B = to_bin(C),
    Ok = B =/= <<>> andalso
        lists:all(fun(X) -> (X >= $a andalso X =< $z) orelse (X >= $A andalso X =< $Z)
                                orelse (X >= $0 andalso X =< $9)
                                orelse lists:member(X, "#(),.% -") end,
                  binary_to_list(B)),
    Ok orelse error({aihtml, {bad_color, C}}),
    B.

svg(Shapes) ->
    ?H:el(svg, Shapes, [],
          [{xmlns, <<"http://www.w3.org/2000/svg">>}, {width, 20}, {height, 20},
           {<<"viewBox">>, <<"0 0 24 24">>}, {fill, none}, {stroke, <<"currentColor">>},
           {stroke_width, 2}, {stroke_linecap, round}, {stroke_linejoin, round}]).

circle(Cx, Cy, R) -> ?H:el(circle, [], [], [{cx, Cx}, {cy, Cy}, {r, R}]).
line(X1, Y1, X2, Y2) -> ?H:el(line, [], [], [{x1, X1}, {y1, Y1}, {x2, X2}, {y2, Y2}]).
polyline(Points) -> ?H:el(polyline, [], [], [{points, Points}]).
path(D) -> ?H:el(path, [], [], [{d, D}]).
