%% Tests for aihtml_upload.
-module(aihtml_upload_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_upload.hrl").

-define(M, aihtml_upload).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

%%%===================================================================
%%% upload
%%%===================================================================

url_mode_test() ->
    Files = [<<"notes.txt">>, #{name => <<"p.png">>, size => 2048, type => <<"image/png">>,
                                id => 3}],
    H = r(?M:upload(Files, [<<"w-96">>],
                    [{id, up}, {name, docs}, {url, <<"/upload">>}, {accept, <<"image/*">>},
                     {max_size, 1000}, {max_count, 3}, {hint, <<"PNG only">>},
                     {title, <<"t">>}])),
    ?assertMatch({match, _}, re:run(H, <<"^<div class=\"ah-upload w-96\" data-ah=\"upload\" ">>)),
    Json = <<"[&quot;notes.txt&quot;,{&quot;id&quot;:3,&quot;name&quot;:&quot;p.png&quot;,"
             "&quot;size&quot;:2048,&quot;type&quot;:&quot;image/png&quot;}]">>,
    ?assert(has(<<"data-ah-value=\"", Json/binary, "\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"docs\" value=\"", Json/binary, "\">">>, H)),
    ?assert(has(<<"data-ah-url=\"/upload\" data-ah-field=\"file\" data-ah-max-size=\"1000\" "
                  "data-ah-max-count=\"3\"">>, H)),
    ?assert(has(<<" id=\"up\"">>, H)),
    ?assert(has(<<" title=\"t\"">>, H)),
    %% the file input picks, it does not submit
    ?assert(has(<<"<input class=\"ah-upload-input\" type=\"file\" tabindex=\"-1\" "
                  "aria-hidden=\"true\" accept=\"image/*\" multiple>">>, H)),
    ?assert(has(<<"<div class=\"ah-upload-dragger\" role=\"button\" tabindex=\"0\">">>, H)),
    ?assert(has(<<"<span>Drag files here, or</span> <span class=\"ah-upload-browse\">"
                  "click to upload</span>">>, H)),
    ?assert(has(<<"<div class=\"ah-upload-hint\">PNG only</div>">>, H)),
    %% rows of the uploaded files, from the shared template
    ?assert(has(<<"<div class=\"ah-upload-item ah-upload-item-success\" data-file-id=\"s0\">"
                  "<div class=\"ah-upload-item-icon ah-upload-item-icon-file\"">>, H)),
    ?assert(has(<<"data-file-id=\"s1\"><div class=\"ah-upload-item-icon "
                  "ah-upload-item-icon-image\"">>, H)),
    ?assert(has(<<"<span class=\"ah-upload-item-size\">2.0 KB</span>">>, H)),
    ?assert(has(<<"aria-label=\"Remove p.png\">&times;</button>">>, H)),
    ?assertNot(has_quiet(<<"data-ah-manual">>, H)),
    ?assertNot(has_quiet(<<"ah-upload-item-progress">>, H)).

native_mode_test() ->
    H = r(?M:upload([], [], [{name, attachment}, {multiple, false}])),
    ?assert(has(<<"<input class=\"ah-upload-input\" type=\"file\" tabindex=\"-1\" "
                  "aria-hidden=\"true\" name=\"attachment\">">>, H)),
    ?assertNot(has_quiet(<<"type=\"hidden\"">>, H)),
    ?assertNot(has_quiet(<<"data-ah-url">>, H)),
    ?assert(has(<<"data-ah-value=\"[]\"">>, H)),
    ?assert(has(<<"<div class=\"ah-upload-list\" aria-live=\"polite\"></div>">>, H)).

options_test() ->
    H = r(?M:upload([<<"a">>], [disabled],
                    [{url, "/u"}, {field_name, doc}, {auto_upload, false}, {show_file_list, false},
                     {headers, #{<<"x-token">> => <<"k">>}}, {extra_data, #{folder => 7}},
                     {with_credentials, true}, {labels, #{remove => <<"删除"/utf8>>}},
                     {drag_text, <<"拖到这里"/utf8>>}, {browse_text, <<"选择"/utf8>>}])),
    ?assert(has(<<"class=\"ah-upload ah-upload-disabled\"">>, H)),
    ?assert(has(<<"data-ah-field=\"doc\"">>, H)),
    ?assert(has(<<"data-ah-manual">>, H)),
    ?assert(has(<<"data-ah-credentials">>, H)),
    ?assert(has(<<"data-ah-headers=\"{&quot;x-token&quot;:&quot;k&quot;}\"">>, H)),
    ?assert(has(<<"data-ah-extra=\"{&quot;folder&quot;:7}\"">>, H)),
    ?assert(has(<<"&quot;remove&quot;:&quot;删除&quot;"/utf8>>, H)),
    ?assert(has(<<"&quot;too_large&quot;:&quot;File too large&quot;">>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"role=\"button\" tabindex=\"-1\" aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"type=\"file\" tabindex=\"-1\" aria-hidden=\"true\" multiple disabled>">>, H)),
    ?assert(has(<<"拖到这里"/utf8>>, H)),
    ?assertNot(has_quiet(<<"ah-upload-list">>, H)).

format_size_test() ->
    ?assertEqual(<<>>, ?M:format_size(undefined)),
    ?assertEqual(<<"0 B">>, ?M:format_size(0)),
    ?assertEqual(<<"1023 B">>, ?M:format_size(1023)),
    ?assertEqual(<<"1.0 KB">>, ?M:format_size(1024)),
    ?assertEqual(<<"1.5 KB">>, ?M:format_size(1536)),
    ?assertEqual(<<"2.0 MB">>, ?M:format_size(2 * 1024 * 1024)).

uploaded_files_test() ->
    ?assertEqual([], ?M:uploaded_files(<<>>)),
    ?assertEqual([], ?M:uploaded_files(#{value => null})),
    ?assertEqual([#{<<"name">> => <<"a.png">>, <<"size">> => 3}, <<"b">>],
                 ?M:uploaded_files(#{value => <<"[{\"name\":\"a.png\",\"size\":3},\"b\"]">>})),
    ?assertError({aihtml, {bad_upload_value, _}}, ?M:uploaded_files(<<"{}">>)),
    %% what the component writes, it reads back
    Files = [#{name => <<"x">>, id => 1}],
    {match, [V]} = re:run(r(?M:upload(Files, [], [])), <<"data-ah-value=\"([^\"]*)\"">>,
                          [{capture, all_but_first, binary}]),
    Unescaped = binary:replace(V, <<"&quot;">>, <<"\"">>, [global]),
    ?assertEqual([#{<<"name">> => <<"x">>, <<"id">> => 1}], ?M:uploaded_files(Unescaped)).

escaping_test() ->
    H = r(?M:upload([#{name => <<"<b>&.txt">>}], [], [{hint, <<"<i>">>}])),
    ?assert(has(<<"&lt;b&gt;&amp;.txt</span>">>, H)),
    ?assert(has(<<"&lt;i&gt;">>, H)),
    ?assertNot(has_quiet(<<"<b>">>, H)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := upload}] = ?M:catalog(),
    ?assert(erlang:function_exported(?M, upload, 3)),
    ?assertEqual([{uploaded_files, 1}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms, category := form,
      behavior := <<"upload">>} = aihtml_catalog:entry(?M, upload),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    [?assert(is_binary(D) andalso D =/= <<>>) || D <- maps:values(Docs)],
    [#{name := _, args := <<"(", _/binary>>, doc := _} = X || X <- Ms],
    ?assert(lists:member(setValue, [N || #{name := N} <- Ms])).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    Files = [#{name => <<"a.pdf">>, size => 10}],
    ?assertEqual(r(?M:upload(Files, [disabled, <<"w-80">>],
                             [{id, u}, {name, n}, {url, <<"/up">>}, {accept, <<".pdf">>},
                              {max_count, 2}, {hint, <<"h">>}, {labels, #{too_many => <<"!">>}},
                              {title, <<"t">>}])),
                 r(#ah_upload{value = Files, disabled = true, css = [<<"w-80">>], id = u,
                              name = n, url = <<"/up">>, accept = <<".pdf">>, max_count = 2,
                              hint = <<"h">>, labels = #{too_many => <<"!">>},
                              attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    U = ?M:upload([], [<<"x">>], [{multiple, false}, {max_size, 5}, {field_name, f},
                                  {extra_data, #{a => 1}}, {data_x, 1}]),
    ?assertMatch(#ah_upload{value = [], multiple = false, max_size = 5, field_name = f,
                            extra_data = #{a := 1}, css = [<<"x">>], attrs = [{data_x, 1}],
                            auto_upload = true, show_file_list = true}, U),
    ?assertError({aihtml, {record_only_field, ah_upload, postback}},
                 ?M:upload([], [], [{postback, x}])).

postback_test() ->
    H = r(#ah_upload{id = u, postback = {got, #{k => 1}}}),
    {match, [T]} = re:run(H, <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    ?assertEqual(<<"change">>, Ev),
    ?assertEqual({ok, {?MODULE, got, #{k => 1}}}, aihtml_action:unsign(Tok)),
    %% root_attrs follow the component's own attributes
    ?assertMatch({match, _}, re:run(H, <<"^<div class=\"ah-upload\" data-ah=\"upload\"[^>]* "
                                         "id=\"u\" data-ah-on=\"change:">>)).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, max_size, 0}}, r(#ah_upload{max_size = 0})),
    ?assertError({aihtml, {bad_option, max_count, x}}, r(#ah_upload{max_count = x})),
    ?assertError({aihtml, {bad_option, multiple, yes}}, r(#ah_upload{multiple = yes})),
    ?assertError({aihtml, {bad_option, url, <<>>}}, r(#ah_upload{url = <<>>})),
    ?assertError({aihtml, {bad_option, headers, []}}, r(#ah_upload{headers = []})),
    ?assertError({aihtml, {bad_upload_label, remov}}, r(#ah_upload{labels = #{remov => <<"x">>}})),
    ?assertError({aihtml, {bad_option, value, [{1}]}}, r(#ah_upload{value = [{1}]})),
    ?assertError({aihtml, {bad_upload_file, 7}}, r(#ah_upload{value = [7]})),
    ?assertError({aihtml, {bad_option, value, x}}, r(#ah_upload{value = x})),
    ?assertError({aihtml, {bad_flag, upload, disabled, 1}}, r(#ah_upload{disabled = 1})),
    ?assertError({aihtml, {unknown_modifier, upload, big, _}}, ?M:upload([], [big], [])).

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

default(ah_upload) -> #ah_upload{}.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.
