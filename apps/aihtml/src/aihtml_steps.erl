%%%-------------------------------------------------------------------
%%% @doc A step indicator (sigil's steps), optionally with panels and prev /
%%% next buttons. The value, the current index, is in `data-ah-value' on
%%% the root and a user change fires `change' there. The indicators use
%%% the shared template steps_indicator, which steps.ts re-renders.
%%%
%%% steps/4 builds an element record (#ah_steps{}, defined in
%%% include/aihtml_steps.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_steps).
-behaviour(aihtml_element).

-include("aihtml_steps.hrl").

-export([steps/4, render/1, fields/1, catalog/0]).
-export_type([step/0]).

%% Title | {Title, Description} | #{title, description, content, status, disabled}
-type step() :: aihtml_html:html() | {aihtml_html:html(), aihtml_html:html()} | map().

%% Shared with the browser (see aihtml_tpl), compiled to AH.tpl.steps_indicator.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_steps_indicator, "../templates/steps_indicator.mustache"}).

-import(aihtml_html, [el/4]).
-import(aihtml_lib_layout, [maybe_el/3, bool/2, hidden/2, bin/1]).

-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% @doc A step indicator (wizard). `Steps' are `Title', `{Title, Description}'
%% or `#{title, description, content, status, disabled}' (status is one of
%% completed | active | error | disabled | pending; by default it follows
%% `Current', a 0-based index). When any step has content, the panels and
%% prev/next buttons are rendered too.
%% Options: clickable (default true), show_nav, prev_label, next_label, name.
%% Value: the current index.
-spec steps([html() | {html(), html()} | map()], non_neg_integer(), css(), attrs()) ->
          #ah_steps{}.
steps(Steps, Current, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_steps{items = Steps, value = Current}, Css, Attrs).

%% @doc The field names of #ah_steps{}.
-spec fields(ah_steps) -> [atom()].
fields(ah_steps) -> record_info(fields, ah_steps).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_steps{}) -> html().
render(#ah_steps{items = Steps, value = Current} = R) ->
    Classes = aihtml_element:classes(?MODULE, R),
    Norm = [norm_step(S) || S <- Steps],
    N = length(Norm),
    Cur = min(max(0, Current), max(0, N - 1)),
    Clickable = bool(clickable, R#ah_steps.clickable),
    HasContent = lists:any(fun(M) -> maps:get(content, M, undefined) =/= undefined end, Norm),
    ShowNav = case R#ah_steps.show_nav of
                  undefined -> HasContent;
                  B -> bool(show_nav, B)
              end,
    Indexed = lists:zip(lists:seq(0, N - 1), Norm),
    Items = [step_item(I, M, Cur, Clickable) || {I, M} <- Indexed],
    Panels = [el('div', [el('div', maps:get(content, M, []),
                            [<<"ah-steps-panel">>, [<<"ah-steps-panel-active">> || I =:= Cur]],
                            [{data_index, I}]) || {I, M} <- Indexed],
                 [<<"ah-steps-panels">>], []) || HasContent],
    Nav = [el('div', [step_btn(prev, R#ah_steps.prev_label, Clickable andalso Cur > 0),
                      step_btn(next, R#ah_steps.next_label, Clickable andalso Cur < N - 1)],
              [<<"ah-steps-nav">>], []) || ShowNav],
    el('div', [el('div', Items, [<<"ah-steps-header">>], []), Panels, Nav,
               hidden(R#ah_steps.name, Cur)],
       Classes,
       [[{data_ah, <<"steps">>}, {data_ah_value, Cur},
         {data_clickable, (not Clickable) andalso <<"false">>}], ?E:root_attrs(R, change)]).

norm_step(#{} = M) -> M;
norm_step({T, D}) -> #{title => T, description => D};
norm_step(T) -> #{title => T}.

step_status(I, M, Cur) ->
    case {maps:get(status, M, undefined), maps:get(disabled, M, false)} of
        {undefined, true} -> disabled;
        {undefined, _} when I < Cur -> completed;
        {undefined, _} when I =:= Cur -> active;
        {undefined, _} -> pending;
        {S, _} -> binary_to_existing_atom(bin(S), utf8)
    end.

step_item(I, M, Cur, Clickable) ->
    Status = step_status(I, M, Cur),
    Click = Clickable andalso Status =/= disabled,
    Indicator = aihtml_tpl:safe(tpl_steps_indicator(
                                  #{check => Status =:= completed, error => Status =:= error,
                                    plain => Status =/= completed andalso Status =/= error,
                                    number => I + 1})),
    el('div', [el('div', Indicator, [<<"ah-steps-indicator">>], [{aria_hidden, <<"true">>}]),
               el('div', [], [<<"ah-steps-connector">>,
                              [<<"ah-steps-connector-done">> || Status =:= completed]], []),
               el('div', [el('div', maps:get(title, M, <<>>), [<<"ah-steps-title">>], []),
                          maybe_el('div', maps:get(description, M, undefined),
                                   <<"ah-steps-description">>)],
                  [<<"ah-steps-content">>], [])],
       [<<"ah-steps-item">>, <<"ah-steps-item-", (atom_to_binary(Status, utf8))/binary>>,
        [<<"ah-steps-item-clickable">> || Click], [<<"ah-steps-item-selected">> || I =:= Cur]],
       [{data_index, I}, {role, Click andalso button},
        {tabindex, Click andalso case I =:= Cur of true -> 0; false -> -1 end},
        {aria_current, I =:= Cur andalso step},
        {aria_disabled, Status =:= disabled andalso <<"true">>}]).

step_btn(Action, Label, Enabled) ->
    el(button, Label, [<<"ah-steps-btn">>, [<<"ah-steps-btn-disabled">> || not Enabled]],
       [{type, button}, {data_action, Action}, {disabled, not Enabled}]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => steps, category => layout,
       option_docs => #{clickable => <<"Steps can be clicked and reached by arrow keys (default true).">>,
                        show_nav => <<"Previous and next buttons (default: when a step has content).">>,
                        prev_label => <<"Text of the previous button.">>,
                        next_label => <<"Text of the next button.">>,
                        name => <<"Submit the step index as a hidden input.">>,
                        horizontal => <<"Steps in a row (default).">>,
                        vertical => <<"Steps in a column, panels beside them.">>,
                        disabled => <<"Dim and ignore clicks.">>},
       methods => [#{name => select, args => <<"(Index)">>, doc => <<"Go to a step without firing change.">>},
                   #{name => next, args => <<"()">>, doc => <<"Next enabled step.">>},
                   #{name => prev, args => <<"()">>, doc => <<"Previous enabled step.">>},
                   #{name => first, args => <<"()">>, doc => <<"First step.">>},
                   #{name => last, args => <<"()">>, doc => <<"Last step.">>},
                   #{name => setStatus, args => <<"(Index, Status)">>, doc => <<"Set completed, active, error, disabled or pending.">>}],
       signature => <<"steps(Steps, Current, Css, Attrs)">>, root => <<"ah-steps">>,
       groups => #{orientation => {[horizontal, vertical], horizontal}},
       flags => [disabled],
       options => [clickable, show_nav, prev_label, next_label, name],
       behavior => <<"steps">>, events => [<<"change">>],
       doc => <<"A step indicator with optional panels; value is the 0-based step. "
                "Methods: select(i), next, prev, first, last, setStatus(i, status).">>}].
