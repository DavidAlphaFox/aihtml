%%%-------------------------------------------------------------------
%%% @doc Horizontal toolbar with an overflow popup, ported from sigil's
%%% toolbar (DOM and class names are sigil's, so the ported styles under
%%% priv/css/sigil/components apply unchanged). The behaviour is in
%%% assets/js/components/toolbar.ts, aihtml's additions in
%%% priv/css/extra/toolbar.css.
%%%
%%% toolbar/3 builds an #ah_toolbar{} record (include/aihtml_toolbar.hrl)
%%% and render/1 turns it into HTML (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_toolbar).
-behaviour(aihtml_element).

-include("aihtml_toolbar.hrl").

-export([toolbar/3]).
-export([render/1, fields/1, catalog/0]).

-export_type([tool/0]).

-import(aihtml_lib_nav, [key_attr/1, icon/2]).

%% A toolbar tool: a button (map), a separator, or any other html (custom).
-type tool() :: #{key => aihtml_lib_nav:key(), label => aihtml_html:html(),
                  icon => aihtml_lib_nav:icon(), title => binary(), disabled => boolean(),
                  toggle => boolean(), pressed => boolean(),
                  minimizable => boolean()}
              | separator | {custom, aihtml_html:html()} | aihtml_html:html().

-define(EL, aihtml_element).

%% @doc Horizontal toolbar (sigil toolbar). Tools that do not fit move into
%% an overflow popup behind a "☰" button. Clicking a button tool with a key
%% sets `data-ah-value' to that key and fires `change'; `toggle' tools also
%% flip `aria-pressed'. Flag `disabled'. Options: `popup_width' (default
%% 200).
-spec toolbar([tool()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_toolbar{}.
toolbar(Tools, Css, Attrs) ->
    ?EL:build(?MODULE, #ah_toolbar{tools = Tools}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(ah_toolbar) -> [atom()].
fields(ah_toolbar) -> record_info(fields, ah_toolbar).

-spec render(#ah_toolbar{}) -> aihtml_html:html().
render(#ah_toolbar{tools = Tools} = R) ->
    Classes = ?EL:classes(?MODULE, R),
    Runs = tool_runs(Tools),
    Els = lists:join(aihtml_html:el('div', [], [<<"ah-toolbar-separator">>],
                                    [{role, separator}, {aria_orientation, vertical}]),
                     [tool_run(Run, Last) || {Run, Last} <- mark_last(Runs)]),
    MinBtn = aihtml_html:el('div', <<"\x{2630}"/utf8>>, [<<"ah-toolbar-minimize-btn">>],
                            [{role, button}, {tabindex, 0}, {aria_label, <<"More tools">>},
                             {aria_haspopup, menu}, {aria_expanded, <<"false">>}]),
    aihtml_html:el('div', [Els, MinBtn], Classes,
                   [[{data_ah, <<"toolbar">>}, {role, toolbar},
                     {aria_orientation, horizontal},
                     {data_ah_popup_width, R#ah_toolbar.popup_width}],
                    ?EL:root_attrs(R, change)]).

%% Tools split into runs at separators.
tool_runs(Tools) ->
    {Runs, Cur} = lists:foldl(fun(separator, {Acc, []}) -> {Acc, []};
                                 (separator, {Acc, Cur}) -> {[lists:reverse(Cur) | Acc], []};
                                 (T, {Acc, Cur}) -> {Acc, [T | Cur]}
                              end, {[], []}, Tools),
    lists:reverse(case Cur of [] -> Runs; _ -> [lists:reverse(Cur) | Runs] end).

mark_last([]) -> [];
mark_last([R]) -> [{R, true}];
mark_last([R | Rs]) -> [{R, false} | mark_last(Rs)].

%% One run; adjacent buttons share corners (first / inner / last).
tool_run(Run, LastRun) ->
    N = length(Run),
    Btn = [is_map(T) || T <- Run],
    [begin
         IsBtn = lists:nth(I, Btn),
         Prev = I > 1 andalso lists:nth(I - 1, Btn),
         Next = I < N andalso lists:nth(I + 1, Btn),
         Pos = case {IsBtn, Prev, Next} of
                   {true, true, true} -> <<"ah-toolbar-tool-inner">>;
                   {true, false, true} -> <<"ah-toolbar-tool-first">>;
                   {true, true, false} -> <<"ah-toolbar-tool-last">>;
                   _ -> []
               end,
         SepAfter = I =:= N andalso not LastRun,
         tool(T, [Pos, [<<"ah-toolbar-tool-separator-after">> || SepAfter]])
     end || {I, T} <- lists:enumerate(Run)].

tool(#{} = T, Cls) ->
    Toggle = maps:get(toggle, T, false),
    Pressed = Toggle andalso maps:get(pressed, T, false),
    Label = maps:get(label, T, undefined),
    Title = maps:get(title, T, undefined),
    Btn = aihtml_html:el(button,
              [icon(<<"ah-toolbar-icon">>, maps:get(icon, T, undefined)),
               case Label of undefined -> []; _ -> aihtml_html:el(span, Label, [], []) end],
              [<<"ah-btn">>, <<"ah-btn-sm">>, <<"ah-toolbar-tool-el">>,
               [<<"ah-btn-toggled">> || Pressed]],
              [{type, button}, {data_key, key_attr(T)},
               {title, Title},
               {aria_label, Label =:= undefined andalso Title},
               {aria_pressed, Toggle andalso atom_to_binary(Pressed)},
               {data_ah_toggle, Toggle},
               {disabled, maps:get(disabled, T, false)}]),
    aihtml_html:el('div', Btn, [<<"ah-toolbar-tool">>, Cls],
                   [{data_ah_minimizable, maps:get(minimizable, T, true) =:= false
                                              andalso <<"false">>}]);
tool({custom, Html}, Cls) ->
    tool_custom(Html, Cls);
tool(Html, Cls) ->
    tool_custom(Html, Cls).

tool_custom(Html, Cls) ->
    aihtml_html:el('div', aihtml_html:el('div', Html, [<<"ah-toolbar-tool-el">>], []),
                   [<<"ah-toolbar-tool">>, Cls], []).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => toolbar, category => layout,
       signature => <<"toolbar(Tools, Css, Attrs)">>,
       root => <<"ah-toolbar">>,
       flags => [disabled],
       options => [popup_width],
       behavior => <<"toolbar">>, events => [<<"change">>, <<"click">>],
       option_docs => #{disabled => <<"Disable the whole toolbar.">>,
                       popup_width => <<"Width of the overflow popup, default 200 (px).">>},
       methods => [#{name => layout, args => <<"()">>, doc => <<"Recompute which tools overflow.">>},
                   #{name => open, args => <<"()">>, doc => <<"Open the overflow popup.">>},
                   #{name => close, args => <<"()">>, doc => <<"Close the overflow popup.">>},
                   #{name => disableTool, args => <<"(Key, Disabled)">>, doc => <<"Disable or enable a tool.">>},
                   #{name => setPressed, args => <<"(Key, Pressed)">>, doc => <<"Set a toggle tool without firing change.">>}],
       doc => <<"Row of tool buttons and custom controls; tools that do not fit "
                "move into an overflow popup.">>}].
