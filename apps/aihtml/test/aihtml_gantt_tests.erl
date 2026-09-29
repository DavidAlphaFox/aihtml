%% Tests for aihtml_gantt.
-module(aihtml_gantt_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_gantt.hrl").

-define(M, aihtml_gantt).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%%%===================================================================
%%% gantt
%%%===================================================================

tasks() ->
    [#{id => t1, name => <<"A">>, start => <<"2026-09-01">>, 'end' => <<"2026-09-05">>,
       progress => 50},
     #{id => t2, name => <<"B">>, start => <<"2026-09-05">>, 'end' => <<"2026-09-09">>,
       dependencies => [t1], color => <<"#f00">>}].

gantt_default_rows_test() ->
    H = r(?M:gantt(tasks(), [<<"w-full">>], [{id, g}, {today, <<"2026-09-03">>}])),
    ?assert(has(<<"<div class=\"ah-gantt w-full\" id=\"g\" data-ah=\"gantt\" style=\"height:500px;\"">>, H)),
    %% a week before September to a week after it
    ?assert(has(<<"data-origin=\"2026-08-25\"">>, H)),
    ?assert(has(<<"<div class=\"ah-gantt-month-cell\" style=\"width:420px;\">Aug 2026</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-gantt-month-cell\" style=\"width:1800px;\">Sep 2026</div>">>, H)),
    ?assertEqual(7 + 30 + 7, count(<<"class=\"ah-gantt-day-cell">>, H)),
    ?assert(has(<<"ah-gantt-day-cell ah-gantt-day-today\" style=\"width:60px;\">3<">>, H)),
    %% one row per task, labelled with its name
    ?assert(has(<<"data-rowid=\"t1\" data-level=\"0\" role=\"treeitem\" aria-level=\"1\" tabindex=\"0\"">>, H)),
    ?assert(has(<<"<span class=\"ah-gantt-sidebar-label\">B</span>">>, H)),
    %% bars: 7 days after the origin, 4 days wide, second row
    ?assert(has(<<"data-taskid=\"t1\" data-rowid=\"t1\" data-start=\"2026-09-01\" data-end=\"2026-09-05\"">>, H)),
    ?assert(has(<<"left:420px;top:4px;width:240px;height:32px;background:var(--ah-color-primary);">>, H)),
    ?assert(has(<<"left:660px;top:44px;width:240px;height:32px;background:#f00;">>, H)),
    ?assert(has(<<"<div class=\"ah-gantt-task-progress\" style=\"width:50%;\"></div>">>, H)),
    ?assert(has(<<"aria-label=\"A: 2026-09-01 – 2026-09-05, 50%\""/utf8>>, H)),
    ?assert(has(<<"data-deps=\"t1\"">>, H)),
    %% not editable: no resize handles
    ?assertEqual(0, count(<<"ah-gantt-resize-handle">>, H)),
    %% a finish-to-start curve from row 0 to row 1
    ?assert(has(<<"<path class=\"ah-gantt-dep-line\" d=\"M660,20 C690,20 630,60 660,60\"">>, H)),
    %% today at noon of 3 September: 9.5 days
    ?assert(has(<<"<div class=\"ah-gantt-today-marker\" style=\"display:block;left:570px;\">">>, H)).

gantt_rows_test() ->
    Rows = [#{id => p, label => <<"Phase">>}, #{id => c1, label => <<"One">>, parent => p},
            #{id => c2, label => <<"Two">>, parent => p}, #{id => z, label => <<"Z">>}],
    Tasks = [#{id => a, row => c1, start => {2026, 9, 1}, 'end' => {2026, 9, 3}},
             #{id => b, row => c2, start => {{2026, 9, 2}, {12, 0, 0}}, 'end' => <<"2026-09-04T12:00">>},
             #{id => x, row => nowhere, start => <<"2026-09-01">>, 'end' => <<"2026-09-02">>}],
    H = r(?M:gantt(Tasks, [editable], [{rows, Rows}, {collapsed, [p]}, {id, g},
                                      {labels, #{tasks => <<"{n} jobs">>}}])),
    %% parent collapsed: children hidden, summary bar shown on its row
    ?assert(has(<<"data-rowid=\"p\" data-level=\"0\" role=\"treeitem\" aria-level=\"1\" "
                  "aria-expanded=\"false\"">>, H)),
    ?assert(has(<<"data-rowid=\"c1\" data-parent=\"p\" data-level=\"1\" role=\"treeitem\" "
                  "aria-level=\"2\" tabindex=\"-1\" hidden style=\"height:40px;padding-left:36px;\"">>, H)),
    ?assert(has(<<"<span class=\"ah-gantt-expand-icon\" aria-hidden=\"true\">">>, H)),
    ?assert(has(<<"<div class=\"ah-gantt-summary-label\">2 jobs</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-gantt-summary-bar\" data-rowid=\"p\" data-count=\"2\" "
                  "style=\"position:absolute;left:420px;top:4px;width:210px;">>, H)),
    ?assert(has(<<"data-start=\"2026-09-02T12:00\" data-end=\"2026-09-04T12:00\"">>, H)),
    %% the task of an unknown row is not drawn
    ?assertNot(has_quiet(<<"data-taskid=\"x\"">>, H)),
    %% visible rows: p and z; the layer is two rows high
    ?assert(has(<<"<div class=\"ah-gantt-tasks-layer\" style=\"width:2640px;height:80px;\">">>, H)),
    ?assertEqual(4, count(<<"ah-gantt-resize-handle ah-gantt-resize-">>, H)),
    ?assert(has(<<"data-collapsed=\"p\"">>, H)),
    ?assert(has(<<"data-editable">>, H)),
    %% both ends hidden: the path is empty
    ?assertEqual(0, count(<<"ah-gantt-dep-line">>, H)).

gantt_misc_test() ->
    %% no tasks: the current month
    H = r(#ah_gantt{no_dependencies = true, today = {2026, 2, 10}, height = undefined}),
    ?assert(has(<<"data-origin=\"2026-02-01\"">>, H)),
    ?assertEqual(28, count(<<"class=\"ah-gantt-day-cell">>, H)),
    ?assertNot(has_quiet(<<"<svg">>, H)),
    ?assert(has(<<"data-ah=\"gantt\" data-origin">>, H)),
    ?assertError({aihtml, {bad_date, <<"soon">>}},
                 r(?M:gantt([#{id => a, start => <<"soon">>, 'end' => <<"2026-01-01">>}], [], []))),
    ?assertError({aihtml, {bad_gantt_progress, 120}},
                 r(?M:gantt([(hd(tasks()))#{progress => 120}], [], []))),
    ?assertError({aihtml, {bad_color, <<"red;x">>}},
                 r(?M:gantt([(hd(tasks()))#{color => <<"red;x">>}], [], []))),
    ?assertError({aihtml, {bad_option, column_width, 0}},
                 r(?M:gantt([], [], [{column_width, 0}]))).


gantt_update_test() ->
    G = ?M:gantt(tasks(), [], [{collapsed, [x]}]),
    [#{op := html, id := <<"g1">>, swap := morph, html := H}] =
        aihtml_action:render_ops(
          fun(Ctx) ->
                  ?M:gantt_update(Ctx, #{id => <<"g1">>, data => #{<<"collapsed">> => <<"a,b">>}}, G)
          end),
    ?assert(has(<<"id=\"g1\"">>, H)),
    ?assert(has(<<"data-collapsed=\"a,b\"">>, H)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := gantt}] = ?M:catalog(),
    ?assertEqual([{gantt_update, 3}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
        aihtml_catalog:entry(?M, gantt),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    [_ | _] = aihtml_catalog:classes(E, Fl),
    [?assert(is_binary(D)) || #{doc := D} <- Ms].

catalog_docs_test() ->
    [begin
         ?assert(byte_size(maps:get(doc, E)) > 0),
         [?assert(byte_size(maps:get(K, maps:get(option_docs, E))) > 0)
          || K <- maps:get(options, E, []) ++ maps:get(flags, E, [])]
     end || E <- ?M:catalog()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:gantt(tasks(), [editable, <<"x">>],
                            [{id, g}, {rows, [#{id => t1}, #{id => t2}]}, {today, {2026, 9, 3}},
                             {height, 300}, {title, <<"t">>}])),
                 r(#ah_gantt{items = tasks(), editable = true, css = [<<"x">>], id = g,
                             rows = [#{id => t1}, #{id => t2}], today = {2026, 9, 3},
                             height = 300, attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_gantt{items = [], no_dependencies = true, column_width = 30,
                           attrs = [{role, x}]},
                 ?M:gantt([], [no_dependencies], [{column_width, 30}, {role, x}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z:-]+:[^\" ]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Tok | Rev] = lists:reverse(binary:split(T, <<":">>, [global])),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {iolist_to_binary(lists:join(<<":">>, lists:reverse(Rev))), Ref}
            end,
    ?assertEqual({<<"ah:task-change">>, {?MODULE, moved, #{}}},
                 Token(#ah_gantt{postback = moved})).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, gantt, editable, yes}}, r(#ah_gantt{editable = yes})),
    ?assertError({aihtml, {bad_label, gantt, x}}, r(#ah_gantt{labels = #{x => <<"y">>}})),
    ?assertError({aihtml, {unknown_modifier, gantt, big, _}}, ?M:gantt([], [big], [])).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(default(Tag))))),
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].

default(ah_gantt) -> #ah_gantt{}.

generated_id_test() ->
    ?assertNotEqual(r(#ah_gantt{}), r(#ah_gantt{})).

%% A value containing a comma is escaped in data-ah-value (aihtml_value).
vhas(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

comma_collapsed_test() ->
    G = ?M:gantt(tasks(), [], []),
    [#{op := html, html := H}] =
        aihtml_action:render_ops(
          fun(Ctx) ->
                  ?M:gantt_update(Ctx, #{id => <<"g1">>, data => #{<<"collapsed">> => <<"a\\,b,c">>}}, G)
          end),
    ?assert(vhas(<<"data-collapsed=\"a\\,b,c\"">>, H)).
