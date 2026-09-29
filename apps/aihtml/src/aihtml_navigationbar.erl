%%%-------------------------------------------------------------------
%%% @doc The navigation bar, ported from sigil (layout/navigationbar):
%%% collapsible sections under clickable headers (an accordion). DOM and
%%% class names are the ones sigil renders, so the styles in
%%% priv/css/sigil apply unchanged; the behaviour is in
%%% assets/js/components/navigationbar.ts.
%%%
%%% The value (the expanded indexes, "0,2") is in `data-ah-value' on the
%%% root, a `name' renders a hidden input, and user changes fire `change'
%%% on the root.
%%%
%%% navigationbar/4 builds an element record (#ah_navigationbar{},
%%% include/aihtml_navigationbar.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_navigationbar).
-behaviour(aihtml_element).

-include("aihtml_navigationbar.hrl").

-export([navigationbar/4, render/1, fields/1, catalog/0]).

-export_type([header/0, item/0, value/0]).

%% the last clause rejects items outside the declared types at run time
-dialyzer({no_match, [nav_item/1]}).
%% text/1 also accepts ids outside the declared id() type, as before the split
-dialyzer({no_match, [text/1]}).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
%% A header is HTML or #{title, subheader, extra} (a three-column header).
-type header() :: aihtml_html:html()
                | #{title := aihtml_html:html(),
                    subheader => aihtml_html:html(),
                    extra => aihtml_html:html()}.
%% {Header, Content} | {Header, Content, Opts} (Opts: disabled, actions)
%% | #{header, content, actions, disabled}.
-type item() :: {header(), aihtml_html:html()}
              | {header(), aihtml_html:html(), aihtml_html:attrs()}
              | #{header := header(),
                  content => aihtml_html:html(),
                  actions => aihtml_html:html(),
                  disabled => boolean()}.
%% Expanded item indexes (0-based): N, [N], "0,2" or undefined (none).
-type value() :: undefined | non_neg_integer() | [non_neg_integer()] | binary().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Collapsible sections with a clickable header each. `Items' are
%% `{Header, Content}', `{Header, Content, Opts}' (Opts: `disabled',
%% `actions') or maps `#{header, content, actions, disabled}'; a header
%% may be `#{title, subheader, extra}'. `Value' holds the expanded
%% indexes, 0-based: `N', `[N]', `<<"0,2">>' or `undefined'.
-spec navigationbar([item()], value(), css(), attrs()) -> #ah_navigationbar{}.
navigationbar(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_navigationbar{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_navigationbar) -> record_info(fields, ah_navigationbar).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_navigationbar{}) -> aihtml_html:html().
render(#ah_navigationbar{items = Items0, value = Value, name = Name,
                         disabled = Disabled} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),
    #ah_navigationbar{expand_mode = Mode, animation = Anim, toggle_mode = Toggle,
                      arrow_position = ArrowPos} = R,
    check(expand_mode, Mode, [single, single_fit_height, multiple, toggle, none]),
    check(animation, Anim, [slide, fade, none]),
    check(toggle_mode, Toggle, [click, dblclick, none]),
    check(arrow_position, ArrowPos, [left, right]),
    [is_integer(D) andalso D >= 0 orelse error({aihtml, {bad_option, F, D}})
     || {F, D} <- [{expand_duration, R#ah_navigationbar.expand_duration},
                   {collapse_duration, R#ah_navigationbar.collapse_duration}]],
    Items = [nav_item(I) || I <- Items0],
    Expanded = indexes(Value),
    Arrow = fun(Open) -> nav_arrow(R, Open) end,
    Sections =
        [begin
             Open = lists:member(Idx, Expanded),
             HeaderId = <<Id/binary, "-item-", (integer_to_binary(Idx))/binary, "-header">>,
             BodyId = <<Id/binary, "-item-", (integer_to_binary(Idx))/binary, "-content">>,
             ?H:el('div',
                   [?H:el('div',
                          %% sigil puts the arrow first and moves a left one with
                          %% `order'; a right one goes after the text here
                          [?H:el(span, nav_header(Header),
                                 [<<"ah-navigationbar-header-text">>,
                                  [<<"ah-navigationbar-header-text-structured">>
                                   || is_map(Header)]], []),
                           Arrow(Open)],
                          [<<"ah-navigationbar-header">>,
                           [<<"ah-navigationbar-header-expanded">> || Open],
                           [<<"ah-navigationbar-disabled">> || Off],
                           [<<"ah-navigationbar-header-no-toggle">> || Toggle =:= none]],
                          [{id, HeaderId}, {role, button},
                           {tabindex, case Off orelse Disabled of
                                          true -> <<"-1">>;
                                          false -> <<"0">>
                                      end},
                           {aria_expanded, atom_to_binary(Open, utf8)},
                           {aria_controls, BodyId},
                           {aria_disabled, Off andalso <<"true">>}]),
                    ?H:el('div',
                          [?H:el('div', Content, [<<"ah-navigationbar-content">>], []),
                           case Actions of
                               undefined -> [];
                               _ -> ?H:el('div', Actions, [<<"ah-navigationbar-actions">>], [])
                           end],
                          [<<"ah-navigationbar-body">>],
                          [{id, BodyId}, {role, region}, {aria_labelledby, HeaderId},
                           {style, case Open of true -> undefined; false -> <<"display:none;">> end}])],
                   [<<"ah-navigationbar-item">>], [])
         end || {Idx, {Header, Content, Actions, Off}}
                    <- lists:zip(lists:seq(0, length(Items) - 1), Items)],
    Cur = join([integer_to_binary(I) || I <- Expanded]),
    Style = [[<<"width:">>, css_size(W), $;] || W <- [R#ah_navigationbar.width], W =/= undefined]
        ++ [[<<"height:">>, css_size(Hh), $;] || Hh <- [R#ah_navigationbar.height], Hh =/= undefined],
    ?H:el('div',
          %% the hidden input goes first: items rely on :last-child
          [hidden_input(Name, Cur) | Sections],
          [Classes, <<"ah-navigationbar-vertical">>,
           case Mode of
               single -> <<"ah-navigationbar-expand-single">>;
               multiple -> <<"ah-navigationbar-expand-multiple">>;
               _ -> []
           end,
           case Anim of
               slide -> <<"ah-navigationbar-animate-slide">>;
               fade -> <<"ah-navigationbar-animate-fade">>;
               none -> []
           end,
           [<<"ah-navigationbar-disabled">> || Disabled]],
          [[{id, Id}, {style, case Style of [] -> undefined; _ -> iolist_to_binary(Style) end},
            {data_ah, <<"navigationbar">>}, {data_ah_value, Cur},
            {data_expand_mode, Mode}, {data_animation, Anim}, {data_toggle_mode, Toggle},
            {data_expand_duration, R#ah_navigationbar.expand_duration},
            {data_collapse_duration, R#ah_navigationbar.collapse_duration},
            {data_fit, Mode =:= single_fit_height andalso R#ah_navigationbar.height =/= undefined},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

nav_item(#{header := H} = M) ->
    {H, maps:get(content, M, []), maps:get(actions, M, undefined),
     maps:get(disabled, M, false) =:= true};
nav_item({H, C}) -> {H, C, undefined, false};
nav_item({H, C, Opts}) ->
    O = flat(Opts),
    {H, C, proplists:get_value(actions, O), proplists:get_value(disabled, O, false) =:= true};
nav_item(Other) -> error({aihtml, {bad_navigationbar_item, Other}}).

nav_header(#{title := T} = M) ->
    [?H:el(span, T, [<<"ah-navigationbar-header-title">>], []),
     [?H:el(span, S, [<<"ah-navigationbar-header-subheader">>], [])
      || S <- [maps:get(subheader, M, undefined)], S =/= undefined],
     [?H:el(span, X, [<<"ah-navigationbar-header-extra">>], [])
      || X <- [maps:get(extra, M, undefined)], X =/= undefined]];
nav_header(H) when is_map(H) -> error({aihtml, {bad_navigationbar_header, H}});
nav_header(H) -> H.

%% sigil's render-arrow: one icon that turns, or two that swap.
nav_arrow(#ah_navigationbar{no_arrow = true}, _) -> [];
nav_arrow(#ah_navigationbar{arrow_position = Pos, expand_icon = Ex, collapse_icon = Co}, Open) ->
    Dual = Ex =/= undefined andalso Co =/= undefined,
    Cls = [<<"ah-navigationbar-arrow">>,
           [<<"ah-navigationbar-arrow-left">> || Pos =:= left],
           [<<"ah-navigationbar-arrow-up">> || Open],
           [<<"ah-navigationbar-arrow-dual">> || Dual]],
    Primary = case Ex of undefined -> <<"▼"/utf8>>; _ -> Ex end,
    case Dual of
        true ->
            ?H:el(span, [?H:el(span, Primary, [<<"ah-navigationbar-icon">>,
                                               <<"ah-navigationbar-icon-expand">>], []),
                         ?H:el(span, Co, [<<"ah-navigationbar-icon">>,
                                          <<"ah-navigationbar-icon-collapse">>], [])],
                  Cls, [{aria_hidden, <<"true">>}]);
        false ->
            ?H:el(span, Primary, Cls, [{aria_hidden, <<"true">>}])
    end.

indexes(undefined) -> [];
indexes(I) when is_integer(I), I >= 0 -> [I];
indexes(B) when is_binary(B) ->
    lists:usort([try binary_to_integer(string:trim(P))
                 catch error:badarg -> error({aihtml, {bad_value, B}})
                 end || P <- binary:split(B, <<",">>, [global, trim_all])]);
indexes(L) when is_list(L) ->
    [is_integer(I) andalso I >= 0 orelse error({aihtml, {bad_value, L}}) || I <- L],
    lists:usort(L);
indexes(Other) -> error({aihtml, {bad_value, Other}}).

css_size(N) when is_integer(N) -> [integer_to_binary(N), <<"px">>];
css_size(S) -> S.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => navigationbar, category => layout,
       signature => <<"navigationbar(Items, Value, Css, Attrs)">>,
       root => <<"ah-navigationbar">>,
       flags => [square, disable_gutters, no_arrow],
       classes => #{disable_gutters => [<<"ah-navigationbar-no-gutters">>], no_arrow => []},
       options => [expand_mode, animation, toggle_mode, arrow_position, expand_icon,
                   collapse_icon, expand_duration, collapse_duration, width, height],
       behavior => <<"navigationbar">>,
       events => [<<"change">>, <<"ah:expand">>, <<"ah:collapse">>],
       doc => <<"Collapsible sections under clickable headers (an accordion); "
                "the value is the expanded indexes, e.g. \"0,2\".">>,
       option_docs =>
           #{square => <<"No rounded corners.">>,
             disable_gutters => <<"Compact: no outer border or side padding, only dividers.">>,
             no_arrow => <<"Hide the expand arrow.">>,
             expand_mode => <<"single_fit_height (default; with a height the open section "
                              "fills it), single (one open, cannot be closed), toggle (at most "
                              "one open), multiple, or none (the user cannot toggle).">>,
             animation => <<"slide (default), fade or none.">>,
             toggle_mode => <<"What opens a section: click (default), dblclick or none; "
                              "Enter and Space always work unless none.">>,
             arrow_position => <<"right (default) or left of the header text.">>,
             expand_icon => <<"HTML of the arrow (default a small triangle that turns).">>,
             collapse_icon => <<"HTML shown while expanded; with expand_icon the two swap "
                                "(plus / minus style).">>,
             expand_duration => <<"Expand animation in ms (default 250).">>,
             collapse_duration => <<"Collapse animation in ms (default 250).">>,
             width => <<"Width: px as an integer or a CSS length.">>,
             height => <<"Height: px as an integer or a CSS length.">>},
       methods => [#{name => expand, args => <<"(Index)">>, doc => <<"Expand a section.">>},
                   #{name => collapse, args => <<"(Index)">>, doc => <<"Collapse a section.">>},
                   #{name => toggle, args => <<"(Index)">>, doc => <<"Expand or collapse a section.">>},
                   #{name => setValue, args => <<"(Indexes)">>,
                     doc => <<"Expand exactly these sections (a list or \"0,2\").">>},
                   #{name => getValue, args => <<"()">>,
                     doc => <<"Return the expanded indexes as an array.">>},
                   #{name => enable, args => <<"(Index)">>, doc => <<"Enable a section.">>},
                   #{name => disable, args => <<"(Index)">>, doc => <<"Disable a section.">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

check(Field, V, Allowed) ->
    lists:member(V, Allowed) orelse error({aihtml, {bad_option, Field, V}}).

%% The parts refer to each other by id, so a root without one gets one.
ensure_id(R) ->
    Id = case element(3, R) of
             undefined -> <<"ah-b", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, setelement(3, R, Id)}.

flat(M) when is_map(M) -> maps:to_list(M);
flat(L) when is_list(L) -> L.

hidden_input(undefined, _) -> [];
hidden_input(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

join(Vs) -> iolist_to_binary(lists:join(<<",">>, Vs)).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(A) when is_atom(A) -> atom_to_binary(A, utf8);
text(I) when is_integer(I) -> integer_to_binary(I);
text(F) when is_float(F) -> float_to_binary(F, [short]);
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_value, L}})
    end;
text(Other) -> error({aihtml, {bad_value, Other}}).
