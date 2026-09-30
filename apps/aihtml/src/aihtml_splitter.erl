%%%-------------------------------------------------------------------
%%% @doc Two panes separated by a draggable bar, ported from sigil's
%%% splitter (DOM and class names are sigil's, so the ported styles under
%%% priv/css/sigil/components apply unchanged). The behaviour is in
%%% assets/js/components/splitter.ts, aihtml's additions in
%%% priv/css/extra/splitter.css.
%%%
%%% ah_splitter/3 builds an #ah_splitter{} record (include/aihtml_splitter.hrl)
%%% and render/1 turns it into HTML (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_splitter).
-behaviour(aihtml_element).

-include("aihtml_splitter.hrl").

-export([ah_splitter/3]).
-export([render/1, fields/1, catalog/0]).

-export_type([pane/0]).

-import(aihtml_lib_nav, [hidden_input/2, px/1, num/1]).

%% A splitter pane: its content, or a map with its initial size
%% (<<"30%">> or pixels) and minimum size in pixels.
-type pane() :: #{content => aihtml_html:html(), size => binary() | number(),
                  min => non_neg_integer()}
              | aihtml_html:html().

-define(EL, aihtml_element).

%% @doc Two panes separated by a draggable bar (sigil splitter). Css:
%% `vertical' (default; side by side, the bar is vertical) or `horizontal'
%% (stacked); flag `disabled'. Drag, arrow keys (Shift: larger steps),
%% Home/End and Enter (collapse the first pane) resize it; `input' fires
%% while dragging and `change' at the end with `data-ah-value' set to the
%% two sizes in percent ("30,70"). Options: `splitbar_size' (px, default
%% 5), `resizable' (default true), `step' (px, default 10), `name'.
-spec ah_splitter([pane()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_splitter{}.
ah_splitter(Panes, Css, Attrs) ->
    ?EL:build(?MODULE, #ah_splitter{panes = Panes}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(ah_splitter) -> [atom()].
fields(ah_splitter) -> record_info(fields, ah_splitter).

-spec render(#ah_splitter{}) -> aihtml_html:html().
render(#ah_splitter{panes = Panes, splitbar_size = Bar} = R) ->
    Classes = ?EL:classes(?MODULE, R),
    Horiz = R#ah_splitter.orientation =:= horizontal,
    [P0, P1] = case [pane(P) || P <- Panes] of
                   [A, B] -> [A, B];
                   [A] -> [A, pane([])];
                   _ -> error({aihtml, {splitter_needs_two_panes, length(Panes)}})
               end,
    {Basis, Pct} = case maps:get(size, P0, <<"50%">>) of
                       S when is_number(S) -> {[px(S)], undefined};
                       S -> F = percent(S),
                            {[<<"calc((100% - ">>, px(Bar), <<") * ">>, num(F / 100), $)],
                             F}
                   end,
    MinProp = case Horiz of true -> <<"min-height:">>; false -> <<"min-width:">> end,
    Min = fun(P) -> [MinProp, px(maps:get(min, P, 0)), $;] end,
    Value = case Pct of undefined -> undefined;
                        _ -> iolist_to_binary([num(Pct), $,, num(100 - Pct)])
            end,
    Panel = fun(N, P, Style) ->
                    aihtml_html:el('div', maps:get(content, P, []),
                                   [<<"ah-splitter-panel">>],
                                   [{data_panel, N}, {style, Style}])
            end,
    Splitbar = aihtml_html:el('div',
                   aihtml_html:el('div', [], [<<"ah-splitter-collapse-btn">>],
                                  [{aria_hidden, <<"true">>}]),
                   [<<"ah-splitter-splitbar">>],
                   [{role, separator}, {tabindex, 0},
                    {aria_orientation, case Horiz of true -> horizontal; false -> vertical end},
                    {aria_valuemin, 0}, {aria_valuemax, 100},
                    {aria_valuenow, case Pct of undefined -> 50; _ -> round(Pct) end},
                    {style, [case Horiz of true -> <<"height:">>; false -> <<"width:">> end,
                             px(Bar)]}]),
    aihtml_html:el('div',
        [Panel(0, P0, [<<"flex:0 0 ">>, Basis, $;, Min(P0)]),
         Splitbar,
         Panel(1, P1, [<<"flex:1 1 0;">>, Min(P1)]),
         hidden_input(R#ah_splitter.name, Value)],
        Classes,
        [[{data_ah, <<"splitter">>}, {data_ah_value, Value},
          {data_ah_min, iolist_to_binary([integer_to_binary(maps:get(min, P0, 0)), $,,
                                          integer_to_binary(maps:get(min, P1, 0))])},
          {data_ah_resizable, R#ah_splitter.resizable =:= false andalso <<"false">>},
          {data_ah_step, R#ah_splitter.step}],
         ?EL:root_attrs(R, change)]).

pane(#{} = P) -> P;
pane(Html) -> #{content => Html}.

percent(B) when is_binary(B) ->
    N = binary:replace(B, <<"%">>, <<>>),
    try binary_to_float(N) catch error:badarg -> float(binary_to_integer(N)) end;
percent(L) when is_list(L) -> percent(list_to_binary(L)).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => splitter, category => layout,
       signature => <<"ah_splitter(Panes, Css, Attrs)">>,
       root => <<"ah-splitter">>,
       groups => #{orientation => {[vertical, horizontal], vertical}},
       flags => [disabled],
       options => [splitbar_size, resizable, step, name],
       behavior => <<"splitter">>, events => [<<"input">>, <<"change">>],
       option_docs => #{vertical => <<"Panes side by side, vertical bar (default).">>,
                       horizontal => <<"Panes stacked, horizontal bar.">>,
                       disabled => <<"No resizing.">>,
                       splitbar_size => <<"Bar thickness, default 5 (px).">>,
                       resizable => <<"false: no dragging or keys, default true.">>,
                       step => <<"Arrow key step, default 10 (px); Shift moves 5 steps.">>,
                       name => <<"Name of a hidden input holding the sizes.">>},
       methods => [#{name => setSizes, args => <<"(Percent)">>, doc => <<"Set the first pane to a percentage, without firing change.">>},
                   #{name => getSizes, args => <<"()">>, doc => <<"The two pane sizes in px.">>},
                   #{name => collapse, args => <<"()">>, doc => <<"Collapse the first pane.">>},
                   #{name => expand, args => <<"()">>, doc => <<"Restore the collapsed pane.">>}],
       doc => <<"Two panes with a draggable, keyboard-operable split bar; value is "
                "the pane sizes in percent.">>}].
