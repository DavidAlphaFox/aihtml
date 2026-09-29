-module(aihtml_toast_tests).

-include_lib("eunit/include/eunit.hrl").

-define(M, aihtml_toast).

r(H) -> aihtml_html:render_binary(H).

has(Html, Part) ->
    case binary:match(r(Html), Part) of
        nomatch -> ?assertEqual({missing, Part}, r(Html));
        _ -> ok
    end.

%% Operations an action body would send, JSON round-tripped like the wire.
ops(Fun) ->
    Ops = aihtml_action:render_ops(Fun),
    json:decode(iolist_to_binary(json:encode(Ops))).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([toast], [N || #{name := N} <- ?M:catalog()]).

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
%%% Toast
%%%===================================================================

shows_toast_test() ->
    B = aihtml_html:el(button, <<"b">>, [],
                       ?M:shows_toast(<<"Done">>, #{variant => success, duration => 0,
                                                    position => bottom_right,
                                                    description => <<"<x>">>})),
    has(B, <<"data-ah-toast=\"Done\" data-ah-toast-description=\"&lt;x&gt;\" "
             "data-ah-toast-variant=\"success\" data-ah-toast-duration=\"0\" "
             "data-ah-toast-position=\"bottom-right\"">>).

toast_op_test() ->
    [Op] = ops(fun(Ctx) ->
                       ?M:toast(Ctx, <<"Saved <b>">>, #{variant => success, duration => 0,
                                                         position => bottom_left,
                                                         close_on_click => false,
                                                         description => "multi word"})
               end),
    #{<<"op">> := <<"call">>, <<"method">> := <<"notify">>,
      <<"args">> := [#{<<"card">> := Card, <<"position">> := <<"bottom-left">>,
                       <<"duration">> := 0} = Args]} = Op,
    ?assertEqual(3, map_size(Args)),
    ?assertMatch({_, _}, binary:match(Card, <<"<div class=\"ah-notify ah-notify-success\" role=\"alert\">">>)),
    ?assertMatch({_, _}, binary:match(Card, <<"<div class=\"ah-toast__title\">Saved &lt;b&gt;</div>"
                                              "<div class=\"ah-toast__description\">multi word</div>">>)),
    %% the default duration of a toast
    [#{<<"args">> := [#{<<"duration">> := 4000, <<"position">> := <<"top-right">>}]}] =
        ops(fun(Ctx) -> ?M:toast(Ctx, <<"t">>, #{}) end).

facade_extras_test() ->
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()].
