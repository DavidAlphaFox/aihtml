-module(aihtml_notification_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_notification.hrl").

-define(M, aihtml_notification).

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
%%% Catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([notification], [N || #{name := N} <- ?M:catalog()]).

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
%%% Notification
%%%===================================================================

notification_template_test() ->
    H = ?M:ah_notification(<<"Saved <ok>">>, [success, bottom_left],
                           [{id, <<"n">>}, {auto_close, false}, {width, 300},
                            {close_on_click, false}]),
    has(H, <<"<div class=\"ah-notify-tpl\" data-ah=\"notification\" hidden "
             "data-ah-position=\"bottom-left\" data-ah-duration=\"0\" id=\"n\">"
             "<div class=\"ah-notify ah-notify-success\" role=\"alert\" style=\"width:300px\">">>),
    has(H, <<"<div class=\"ah-notify-content\">Saved &lt;ok&gt;</div>">>),
    has(H, <<"class=\"ah-notify-close\"">>),
    D = ?M:ah_notification(<<"x">>, [], [{closable, false}]),
    has(D, <<"data-ah-position=\"top-right\" data-ah-duration=\"3000\">">>),
    has(D, <<"ah-notify ah-notify-info ah-notify-clickable">>),
    has(D, <<"<circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"16\"">>),
    lacks(D, <<"ah-notify-close">>).

notify_op_renders_and_escapes_content_test() ->
    [#{<<"method">> := <<"notify">>, <<"args">> := [#{<<"card">> := Card, <<"duration">> := 3000}]}] =
        ops(fun(Ctx) ->
                    ?M:notify(Ctx, #{content => [aihtml_html:el(b, <<"Hi">>, [], []),
                                                 <<" <script>">>],
                                     variant => error, closable => false})
            end),
    ?assertMatch({_, _}, binary:match(Card, <<"<div class=\"ah-notify-content\"><b>Hi</b> &lt;script&gt;</div>">>)),
    ?assertMatch({_, _}, binary:match(Card, <<"ah-notify ah-notify-error ah-notify-clickable">>)),
    ?assertEqual(nomatch, binary:match(Card, <<"ah-notify-close">>)).

%% The card the server renders is byte-identical to the template output the
%% browser produces for the same view (see also aihtml_tpl_tests).
card_matches_template_test() ->
    {safe, B} = aihtml_tpl:safe(aihtml_lib_overlay:tpl_notification(
                                  #{variant => <<"info">>, info => true, success => false,
                                    warning => false, error => false, clickable => true,
                                    closable => true, width => null, content => <<"x">>})),
    has(?M:ah_notification(<<"x">>, [], []), B).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_notification(<<"N">>, [error, bottom_left],
                                      [{id, <<"n">>}, {auto_close, false}, {width, 320}])),
                 r(#ah_notification{body = <<"N">>, variant = error, position = bottom_left,
                                    id = <<"n">>, auto_close = false, width = 320})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"(ah:[a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ah, Ev, Tok] = binary:split(T, <<":">>, [global]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {<<Ah/binary, ":", Ev/binary>>, Ref}
            end,
    Close = <<"ah:close">>,
    ?assertEqual({Close, {?MODULE, gone, 2}},
                 Token(#ah_notification{postback = {gone, 2}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, notification, variant, danger, _}},
                 r(#ah_notification{variant = danger})),
    ?assertError({aihtml, {bad_option, delay, -1}},
                 r(#ah_notification{delay = -1})).

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

default(ah_notification) -> #ah_notification{}.
