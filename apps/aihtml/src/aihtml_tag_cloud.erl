%%%-------------------------------------------------------------------
%%% @doc The tag_cloud component (designs/04-components.md): `tag_cloud/3'
%%% builds an #ah_tag_cloud{} element record (include/aihtml_tag_cloud.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_tag_cloud).
-behaviour(aihtml_element).

-include("aihtml_tag_cloud.hrl").

-export([tag_cloud/3, render/1, fields/1, catalog/0]).

-export_type([tag/0]).

-import(aihtml_lib_display, [num/1, to_bin/1, method/3]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% Records may hold any value, whatever their field types say, so these
%% keep rejecting values outside the types at render time.
-dialyzer({no_match, [sort_tags/2, alter_case/2]}).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
%% #{label, value, url} | {Label, Value} | {Label, Value, Url}
-type tag() :: #{atom() => term()} | {aihtml_html:html(), number()}
             | {aihtml_html:html(), number(), iodata()}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Tag cloud, font size weighted by value. `Tags' are maps with
%% `label', `value', `url' (or `{Label, Value}' tuples). Options:
%% `min_font_size' (10), `max_font_size' (24), `font_size_unit' (px),
%% `url_base', `display_value', `sort_by' (none | label | value),
%% `sort_order' (ascending | descending), `text_case' (none | all_lower |
%% all_upper | first_upper | title_case), `text_color', `min_color' and
%% `max_color' (#RRGGBB gradient), `min_value', `max_value',
%% `display_limit', `take_top_weighted'. Fires `ah:tag-click'.
-spec tag_cloud([map() | {html(), number()}], css(), attrs()) -> #ah_tag_cloud{}.
tag_cloud(Tags, Css, Attrs) ->
    ?E:build(?MODULE, #ah_tag_cloud{items = Tags}, Css, Attrs).

%% @doc The field names of #ah_tag_cloud{}.
-spec fields(atom()) -> [atom()].
fields(ah_tag_cloud) -> record_info(fields, ah_tag_cloud).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_tag_cloud{}) -> html().
render(#ah_tag_cloud{items = Tags0} = R) ->
    Cls = ?E:classes(?MODULE, R),
    Tags = sort_tags(filter_tags([tag_map(T) || T <- Tags0], R), R),
    Values = [V || #{value := V} <- Tags],
    {Lo, Hi} = case Values of [] -> {0, 0}; _ -> {lists:min(Values), lists:max(Values)} end,
    Range = Hi - Lo,
    MinF = R#ah_tag_cloud.min_font_size,
    MaxF = R#ah_tag_cloud.max_font_size,
    Unit = unit(R#ah_tag_cloud.font_size_unit),
    Grad = {R#ah_tag_cloud.min_color, R#ah_tag_cloud.max_color},
    Fixed = R#ah_tag_cloud.text_color,
    Base = R#ah_tag_cloud.url_base,
    Case = R#ah_tag_cloud.text_case,
    ShowValue = R#ah_tag_cloud.display_value =:= true,
    Items = [begin
                 Ratio = if Range == 0 -> 0.5; true -> (V - Lo) / Range end,
                 Font = MinF + (MaxF - MinF) * Ratio,
                 Color = case Grad of
                             {C1, C2} when C1 =/= undefined, C2 =/= undefined -> lerp_color(C1, C2, Ratio);
                             _ when Fixed =/= undefined -> aihtml_lib_color:css(Fixed);
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
          [[{data_ah, <<"tag-cloud">>}], ?E:root_attrs(R, 'ah:tag-click')]).

tag_map({L, V}) -> tag_map(#{label => L, value => V});
tag_map({L, V, U}) -> tag_map(#{label => L, value => V, url => U});
tag_map(#{label := L} = M) ->
    V = maps:get(value, M, 0),
    is_number(V) orelse error({aihtml, {bad_tag_value, V}}),
    #{label => L, value => V, url => maps:get(url, M, undefined)};
tag_map(Other) -> error({aihtml, {bad_tag, Other}}).

filter_tags(Tags, R) ->
    Min = R#ah_tag_cloud.min_value,
    Max = R#ah_tag_cloud.max_value,
    T1 = [T || #{value := V} = T <- Tags, not (Min > 0) orelse V >= Min],
    T2 = [T || #{value := V} = T <- T1, not (Max > 0) orelse V =< Max],
    case R#ah_tag_cloud.display_limit of
        N when is_integer(N), N > 0, length(T2) > N ->
            case R#ah_tag_cloud.take_top_weighted of
                true ->
                    Top = lists:sublist(lists:sort(fun({_, #{value := A}}, {_, #{value := B}}) ->
                                                           A >= B end,
                                                   lists:enumerate(T2)), N),
                    [T || {_, T} <- lists:keysort(1, Top)];
                _ -> lists:sublist(T2, N)
            end;
        _ -> T2
    end.

sort_tags(Tags, R) ->
    Key = case R#ah_tag_cloud.sort_by of
              none -> none;
              label -> fun(#{label := L}) -> string:lowercase(to_bin(L)) end;
              value -> fun(#{value := V}) -> V end;
              Other -> error({aihtml, {bad_sort_by, Other}})
          end,
    case Key of
        none -> Tags;
        _ ->
            Sorted = [T || {_, T} <- lists:keysort(1, [{Key(T), T} || T <- Tags])],
            case R#ah_tag_cloud.sort_order of
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
    [maps:merge(entry(), api())].

entry() ->
    #{name => tag_cloud, category => data, root => <<"ah-tagcloud">>,
      signature => <<"tag_cloud(Tags, Css, Attrs)">>,
      flags => [disabled],
      options => [min_font_size, max_font_size, font_size_unit, url_base, display_value,
                  sort_by, sort_order, text_case, text_color, min_color, max_color,
                  min_value, max_value, display_limit, take_top_weighted],
      behavior => <<"tag-cloud">>, events => [<<"ah:tag-click">>],
      doc => <<"Tags sized (and optionally coloured) by weight. Methods "
               "hideItem(i), showItem(i).">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{min_font_size => <<"Size of the lightest tag (default 10).">>,
                       max_font_size => <<"Size of the heaviest tag (default 24).">>,
                       font_size_unit => <<"px (default), em, rem, pt or %.">>,
                       url_base => <<"Prefix for each tag's url.">>,
                       display_value => <<"Append \" (value)\" to each label.">>,
                       sort_by => <<"none (default), label or value.">>,
                       sort_order => <<"ascending (default) or descending.">>,
                       text_case => <<"none, all_lower, all_upper, first_upper or title_case.">>,
                       text_color => <<"One colour for all tags.">>,
                       min_color => <<"#RRGGBB of the lightest tag (with max_color: a gradient).">>,
                       max_color => <<"#RRGGBB of the heaviest tag.">>,
                       min_value => <<"Hide tags below this value (0: no limit).">>,
                       max_value => <<"Hide tags above this value (0: no limit).">>,
                       display_limit => <<"Show at most N tags.">>,
                       take_top_weighted => <<"With display_limit, keep the heaviest tags.">>,
                       disabled => <<"Dimmed and inert.">>},
      methods => [method(hideItem, <<"(Index)">>, <<"Hide a tag.">>),
                  method(showItem, <<"(Index)">>, <<"Show a hidden tag.">>)]}.
