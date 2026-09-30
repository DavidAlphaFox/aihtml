%%%-------------------------------------------------------------------
%%% @doc The expandable_text component (designs/04-components.md): `ah_expandable_text/3'
%%% builds an #ah_expandable_text{} element record (include/aihtml_expandable_text.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_expandable_text).
-behaviour(aihtml_element).

-include("aihtml_expandable_text.hrl").

-export([ah_expandable_text/3, render/1, fields/1, catalog/0]).

-import(aihtml_lib_display, [tf/1, method/3]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Long text cut at `threshold' characters (100) with a toggle.
%% Options: `threshold', `expanded' (false), `expand_label' ("展开"),
%% `collapse_label' ("收起"). Fires `ah:toggle' with the new state.
-spec ah_expandable_text(unicode:chardata(), css(), attrs()) -> #ah_expandable_text{}.
ah_expandable_text(Text, Css, Attrs) ->
    ?E:build(?MODULE, #ah_expandable_text{text = Text}, Css, Attrs).

%% @doc The field names of #ah_expandable_text{}.
-spec fields(atom()) -> [atom()].
fields(ah_expandable_text) -> record_info(fields, ah_expandable_text).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_expandable_text{}) -> html().
render(#ah_expandable_text{text = Text0, threshold = Th, expand_label = ExpandL,
                           collapse_label = CollapseL} = R) ->
    Cls = ?E:classes(?MODULE, R),
    Text = case Text0 of undefined -> <<>>; _ -> unicode:characters_to_binary(Text0) end,
    Exp = R#ah_expandable_text.expanded =:= true,
    Long = string:length(Text) > Th,
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
           ?E:root_attrs(R, 'ah:toggle')]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => expandable_text, category => text, root => <<"ah-expandable-text">>,
      signature => <<"ah_expandable_text(Text, Css, Attrs)">>,
      options => [threshold, expanded, expand_label, collapse_label],
      behavior => <<"expandable-text">>, events => [<<"ah:toggle">>],
      doc => <<"Text cut at threshold characters with an expand/collapse toggle. "
               "Methods toggle(), expand(), collapse().">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{threshold => <<"Characters shown before the cut (default 100).">>,
                       expanded => <<"Start expanded.">>,
                       expand_label => <<"Toggle text when collapsed (default 展开)."/utf8>>,
                       collapse_label => <<"Toggle text when expanded (default 收起)."/utf8>>},
      methods => [method(toggle, <<"()">>, <<"Expand or collapse; fires ah:toggle.">>),
                  method(expand, <<"()">>, <<"Show the full text.">>),
                  method(collapse, <<"()">>, <<"Show the cut text.">>)]}.
