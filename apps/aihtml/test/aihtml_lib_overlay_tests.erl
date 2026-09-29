-module(aihtml_lib_overlay_tests).

-include_lib("eunit/include/eunit.hrl").

-define(M, aihtml_lib_overlay).

r(H) -> aihtml_html:render_binary(H).

has(Html, Part) ->
    case binary:match(r(Html), Part) of
        nomatch -> ?assertEqual({missing, Part}, r(Html));
        _ -> ok
    end.

lacks(Html, Part) ->
    ?assertEqual(nomatch, binary:match(r(Html), Part)).

%% Operations an action body would send, JSON round-tripped like the wire.
ops(Fun) ->
    Ops = aihtml_action:render_ops(Fun),
    json:decode(iolist_to_binary(json:encode(Ops))).

%%%===================================================================
%%% Declarative triggers
%%%===================================================================

triggers_test() ->
    has(aihtml_html:el(button, <<"o">>, [], ?M:opens({id, <<"d">>})),
        <<"data-ah-open=\"#d\" aria-haspopup=\"dialog\" aria-controls=\"d\"">>),
    has(aihtml_html:el(button, <<"t">>, [], ?M:toggles(<<".x">>)), <<"data-ah-toggle=\".x\"">>),
    lacks(aihtml_html:el(button, <<"t">>, [], ?M:toggles(<<".x">>)), <<"aria-controls">>),
    has(aihtml_html:el(button, <<"c">>, [], ?M:closes()), <<"data-ah-close=\"\"">>),
    has(aihtml_html:el(button, <<"c">>, [], ?M:closes({id, w})), <<"data-ah-close=\"#w\"">>),
    has(aihtml_html:el(button, <<"c">>, [], ?M:closes(closest, ok)),
        <<"data-ah-close=\"\" data-ah-result=\"ok\"">>),
    has(aihtml_html:el(button, <<"c">>, [], ?M:closes(<<"#w">>, <<"cancel">>)),
        <<"data-ah-close=\"#w\" data-ah-result=\"cancel\"">>).

%%%===================================================================
%%% Server-driven helpers
%%%===================================================================

open_close_toggle_ops_test() ->
    ?assertEqual([#{<<"op">> => <<"call">>, <<"id">> => <<"cart">>,
                    <<"method">> => <<"open">>, <<"args">> => []},
                  #{<<"op">> => <<"call">>, <<"sel">> => <<"#cart">>,
                    <<"method">> => <<"close">>, <<"args">> => []},
                  #{<<"op">> => <<"call">>, <<"id">> => <<"cart">>,
                    <<"method">> => <<"toggle">>, <<"args">> => []}],
                 ops(fun(Ctx) ->
                             ?M:open(Ctx, {id, <<"cart">>}),
                             ?M:close(Ctx, <<"#cart">>),
                             ?M:toggle(Ctx, {id, cart})
                     end)).
