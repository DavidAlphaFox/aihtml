-module(aihtml_tooltip_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_tooltip.hrl").

-define(M, aihtml_tooltip).

r(H) -> aihtml_html:render_binary(H).

has(Html, Part) ->
    case binary:match(r(Html), Part) of
        nomatch -> ?assertEqual({missing, Part}, r(Html));
        _ -> ok
    end.

lacks(Html, Part) ->
    ?assertEqual(nomatch, binary:match(r(Html), Part)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([tooltip], [N || #{name := N} <- ?M:catalog()]).

catalog_entries_are_complete_test() ->
    [begin
         ?assert(is_binary(S)), ?assert(is_binary(R)),
         ?assertEqual(overlay, C),
         ?assert(lists:member(<<"ah:open">>, maps:get(events, E)))
     end || #{signature := S, root := R, category := C} = E <- ?M:catalog()].

every_entry_documents_options_and_methods_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         [?assert(maps:is_key(O, Docs)) || O <- maps:get(options, E, [])],
         ?assert(is_list(maps:get(methods, E)))
     end || E <- ?M:catalog()].

%%%===================================================================
%%% Tooltip
%%%===================================================================

tooltip_wraps_trigger_test() ->
    H = ?M:ah_tooltip(<<"Hint <b>">>, aihtml_html:el(button, <<"B">>, [], []), [top], [{id, <<"t">>}]),
    has(H, <<"<span class=\"ah-tooltip-host\" data-ah=\"tooltip\" data-ah-tip-position=\"top\" id=\"t\">">>),
    has(H, <<"<button>B</button>">>),
    has(H, <<"class=\"ah-tooltip ah-tooltip-top\" role=\"tooltip\"">>),
    has(H, <<"Hint &lt;b&gt;">>).

tooltip_options_and_mouse_test() ->
    H = ?M:ah_tooltip(<<"x">>, <<"y">>, [mouse, no_arrow, <<"max-w-xs">>],
                      [{trigger, click}, {auto_hide, false}, {show_delay, 0}, {width, 200}]),
    has(H, <<"data-ah-tip-position=\"mouse\"">>),
    has(H, <<"data-ah-tip-trigger=\"click\"">>),
    has(H, <<"data-ah-tip-auto-hide=\"false\"">>),
    has(H, <<"data-ah-tip-delay=\"0\"">>),
    has(H, <<"ah-tooltip ah-tooltip-bottom ah-tooltip-no-arrow max-w-xs">>),
    has(H, <<"style=\"width:200px\"">>),
    %% options are not written as HTML attributes
    lacks(H, <<" trigger=">>).

tooltip_bad_modifier_test() ->
    ?assertError({aihtml, {unknown_modifier, tooltip, primary, _}},
                 ?M:ah_tooltip(<<"x">>, <<"y">>, [primary], [])),
    ?assertError({aihtml, {conflicting_modifiers, tooltip, position, _}},
                 ?M:ah_tooltip(<<"x">>, <<"y">>, [top, left], [])).

tooltip_attrs_test() ->
    B = aihtml_html:el(button, <<"b">>, [], ?M:tooltip_attrs(<<"Save \"now\"">>,
                                                           #{position => left, arrow => false})),
    has(B, <<"data-ah-tooltip=\"Save &quot;now&quot;\"">>),
    has(B, <<"data-ah-tip-arrow=\"false\"">>),
    has(B, <<"data-ah-tip-position=\"left\"">>),
    lacks(aihtml_html:el(i, [], [], ?M:tooltip_attrs(<<"t">>, #{})), <<"arrow">>).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_tooltip(<<"Hint">>, <<"B">>, [left, no_arrow, <<"max-w-xs">>],
                                 [{id, t}, {trigger, click}, {width, 200}, {title, <<"x">>}])),
                 r(#ah_tooltip{body = <<"Hint">>, anchor = <<"B">>, position = left,
                               no_arrow = true, css = [<<"max-w-xs">>], id = t,
                               trigger = click, width = 200, attrs = [{title, <<"x">>}]})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_tooltip{body = <<"b">>, anchor = <<"a">>, position = mouse},
                 ?M:ah_tooltip(<<"b">>, <<"a">>, [mouse], [])).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_tooltip}},
                 r(#ah_tooltip{body = <<"x">>, postback = closed})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, tooltip, position, middle, _}},
                 r(#ah_tooltip{position = middle})),
    ?assertError({aihtml, {bad_option, trigger, hovering}},
                 r(#ah_tooltip{trigger = hovering})).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(default(Tag))))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].

default(ah_tooltip) -> #ah_tooltip{}.
