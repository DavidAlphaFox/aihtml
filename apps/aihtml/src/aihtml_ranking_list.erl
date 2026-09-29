%%%-------------------------------------------------------------------
%%% @doc The ranking_list component (designs/04-components.md): `ranking_list/3'
%%% builds an #ah_ranking_list{} element record (include/aihtml_ranking_list.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_ranking_list).
-behaviour(aihtml_element).

-include("aihtml_ranking_list.hrl").

-export([ranking_list/3, render/1, fields/1, catalog/0]).

-export_type([item/0]).

-import(aihtml_lib_display, [blank/1, to_bin/1]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% Records may hold any value, whatever their field types say, so these
%% keep rejecting values outside the types at render time.
-dialyzer({no_match, [flag/2]}).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
%% #{name, value, rank, secondary, sub_value, code, tag, attrs}
-type item() :: #{atom() => term()}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Ranking list. `Items' are maps with `name', `value', and
%% optionally `rank', `secondary', `sub_value', `code' (country code,
%% shown as a flag), `tag', `attrs' (on the row). Options: `title',
%% `max_items', `show_rank' (true), `flag_style' (emoji | flag_icons |
%% none), `tag_colors' (#{Tag => success | warning | error | info}).
%% `clickable' rows fire `ah:item-click' with the row index.
-spec ranking_list([map()], css(), attrs()) -> #ah_ranking_list{}.
ranking_list(Items, Css, Attrs) ->
    ?E:build(?MODULE, #ah_ranking_list{items = Items}, Css, Attrs).

%% @doc The field names of #ah_ranking_list{}.
-spec fields(atom()) -> [atom()].
fields(ah_ranking_list) -> record_info(fields, ah_ranking_list).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_ranking_list{}) -> html().
render(#ah_ranking_list{items = Items, title = Title} = R) ->
    Cls = ?E:classes(?MODULE, R),
    Shown = case R#ah_ranking_list.max_items of
                N when is_integer(N), N >= 0 -> lists:sublist(Items, N);
                _ -> Items
            end,
    Ctx = #{rank => R#ah_ranking_list.show_rank =/= false,
            flags => R#ah_ranking_list.flag_style,
            tags => R#ah_ranking_list.tag_colors,
            click => R#ah_ranking_list.clickable},
    ?H:el('div',
          [[?H:el('div', ?H:el(span, Title, [<<"ah-ranking-list__title">>], []),
                  [<<"ah-ranking-list__header">>], []) || not blank(Title)],
           ?H:el('div', [ranking_item(I, It, Ctx) || {I, It} <- lists:enumerate(0, Shown)],
                 [<<"ah-ranking-list__list">>], [])],
          Cls, [[{data_ah, <<"ranking-list">>}], ?E:root_attrs(R, 'ah:item-click')]).

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

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => ranking_list, category => data, root => <<"ah-ranking-list">>,
      signature => <<"ranking_list(Items, Css, Attrs)">>,
      flags => [dense, disabled, clickable],
      classes => #{dense => [<<"ah-ranking-list--dense">>],
                   disabled => [<<"ah-ranking-list--disabled">>], clickable => []},
      options => [title, max_items, show_rank, flag_style, tag_colors],
      behavior => <<"ranking-list">>, events => [<<"ah:item-click">>],
      doc => <<"Top N list with rank medals, flags, values and tags.">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{title => <<"Heading above the list.">>,
                       max_items => <<"Show only the first N rows.">>,
                       show_rank => <<"Rank circles, gold / silver / bronze for 1-3 (default true).">>,
                       flag_style => <<"emoji (default), flag_icons (needs the flag-icons CSS) or none.">>,
                       tag_colors => <<"#{Tag => success | warning | error | info}.">>,
                       dense => <<"Tighter rows.">>,
                       disabled => <<"Dimmed and inert.">>,
                       clickable => <<"Rows are buttons firing ah:item-click with {index}.">>},
      methods => []}.
