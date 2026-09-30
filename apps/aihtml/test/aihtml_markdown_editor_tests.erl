%% Tests for aihtml_markdown_editor.
-module(aihtml_markdown_editor_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_markdown_editor.hrl").

-define(M, aihtml_markdown_editor).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

markdown_editor_basic_test() ->
    H = r(?M:ah_markdown_editor(<<"# Title\n\nSome **bold**.">>, [<<"w-full">>],
                                [{id, ed}, {name, body}, {title, <<"t">>}])),
    ?assert(has(<<"<div class=\"ah-md-editor w-full\" id=\"ed\" data-ah=\"markdown-editor\"">>, H)),
    ?assert(has(<<"data-ah-value=\"# Title\n\nSome **bold**.\"">>, H)),
    ?assert(has(<<"data-ah-placeholder=\"Start typing...\"">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    %% the textarea: the fallback and the form field
    ?assert(has(<<"<textarea class=\"ah-md-editor-source\" id=\"ed-source\" name=\"body\" "
                  "aria-label=\"Markdown editor\" placeholder=\"Start typing...\" rows=\"10\" "
                  "spellcheck=\"false\"># Title\n\nSome **bold**.</textarea>">>, H)),
    ?assertEqual(1, count(<<"name=">>, H)),
    %% server-rendered UI: block handle, drag indicator, caret, slash menu
    ?assert(has(<<"<div class=\"ah-pm-content\">">>, H)),
    ?assert(has(<<"class=\"ah-pm-block-handle\"">>, H)),
    ?assert(has(<<"class=\"ah-pm-block-handle-add\" type=\"button\" tabindex=\"-1\" "
                  "title=\"Add Block\"">>, H)),
    ?assert(has(<<"class=\"ah-pm-block-handle-drag\"">>, H)),
    ?assert(has(<<"class=\"ah-pm-drag-indicator\"">>, H)),
    ?assert(has(<<"class=\"ah-md-editor-caret\"">>, H)),
    ?assert(has(<<"<div class=\"ah-pm-slash-menu\" id=\"ed-menu\">">>, H)),
    ?assert(has(<<"id=\"ed-menu-list\" role=\"listbox\"">>, H)),
    ?assertEqual(12, count(<<"role=\"option\"">>, H)),
    ?assert(has(<<"<div class=\"ah-pm-slash-menu-item selected\" id=\"ed-menu-0\" role=\"option\" "
                  "data-type=\"paragraph\" aria-selected=\"true\">">>, H)),
    ?assert(has(<<"id=\"ed-menu-2\" role=\"option\" data-type=\"heading\" data-level=\"2\"">>, H)),
    ?assert(has(<<"data-type=\"table\"">>, H)),
    ?assert(has(<<"<button class=\"ah-pm-slash-menu-tab active\" type=\"button\" tabindex=\"-1\" "
                  "data-group=\"Text\">Heading</button>">>, H)),
    ?assert(has(<<"role=\"group\" aria-labelledby=\"ed-menu-List\"">>, H)),
    ?assert(has(<<"<svg">>, H)),
    %% no footer without show_stats / max_chars
    ?assertNot(has_quiet(<<"ah-pm-stats">>, H)),
    %% labels the behaviour needs
    ?assert(has(<<"data-ah-labels=\"{&quot;editor&quot;:&quot;Markdown editor&quot;,"
                  "&quot;enter_image_url&quot;:&quot;Image URL:&quot;,"
                  "&quot;enter_url&quot;:&quot;Enter URL:&quot;}\"">>, H)).

markdown_editor_empty_test() ->
    H = r(?M:ah_markdown_editor(undefined, [], [])),
    ?assert(has(<<"data-ah-value=\"\"">>, H)),
    ?assert(has(<<"id=\"ah-md">>, H)),                  % an id is generated
    ?assert(has(<<"spellcheck=\"false\"></textarea>">>, H)),
    ?assertEqual(r(?M:ah_markdown_editor(<<>>, [], [{id, x}])),
                 r(?M:ah_markdown_editor(undefined, [], [{id, x}]))),
    %% a string value
    ?assert(has(<<"data-ah-value=\"héllo\""/utf8>>,
                r(?M:ah_markdown_editor("héllo", [], [])))).

%% The Markdown travels in an attribute and a textarea, escaped: no
%% </textarea>, </script> or quote in it can break out.
escaping_test() ->
    Md = <<"</textarea><script>alert(1)</script> & \"q\" 'a' <!-- x -->">>,
    H = r(?M:ah_markdown_editor(Md, [], [{id, e}])),
    ?assertNot(has_quiet(<<"<script">>, H)),
    ?assertNot(has_quiet(<<"<!--">>, H)),
    ?assertEqual(1, count(<<"</textarea>">>, H)),
    ?assert(has(<<"data-ah-value=\"&lt;/textarea&gt;&lt;script&gt;alert(1)&lt;/script&gt; "
                  "&amp; &quot;q&quot;">>, H)),
    ?assert(has(<<">&lt;/textarea&gt;&lt;script&gt;alert(1)&lt;/script&gt; &amp; ">>, H)).

%% The HTML parser drops a newline right after <textarea>: one is added so
%% that a value starting with a newline survives.
leading_newline_test() ->
    H = r(?M:ah_markdown_editor(<<"\n\nText">>, [], [{id, n}])),
    ?assert(has(<<"spellcheck=\"false\">\n\n\nText</textarea>">>, H)),
    ?assert(has(<<"data-ah-value=\"\n\nText\"">>, H)),
    ?assert(has(<<"spellcheck=\"false\">Text</textarea>">>,
                r(?M:ah_markdown_editor(<<"Text">>, [], [{id, n}])))).

markdown_editor_options_test() ->
    H = r(?M:ah_markdown_editor(<<"x">>, [],
                                [{id, o}, {placeholder, <<"写点什么"/utf8>>}, {show_stats, true},
                                 {max_chars, 500}, {height, 320}])),
    ?assert(has(<<"data-ah-placeholder=\"写点什么\""/utf8>>, H)),
    ?assert(has(<<"placeholder=\"写点什么\""/utf8>>, H)),
    ?assert(has(<<"data-ah-max-chars=\"500\"">>, H)),
    ?assert(has(<<"style=\"height:320px\"">>, H)),
    ?assert(has(<<"<div class=\"ah-pm-stats\">">>, H)),
    ?assert(has(<<"<span class=\"ah-pm-stats__label\">Chars</span>"
                  "<span class=\"ah-pm-stats__value\" data-stat=\"chars\">–</span>"/utf8>>, H)),
    ?assert(has(<<"data-stat=\"words\"">>, H)),
    ?assert(has(<<"data-stat=\"paragraphs\"">>, H)),
    ?assert(has(<<"data-stat=\"limit\">–/500</span>"/utf8>>, H)),
    ?assert(has(<<"<span class=\"ah-pm-stats__warning\" hidden>Limit exceeded</span>">>, H)),
    %% show_stats alone: no limit
    S = r(?M:ah_markdown_editor(<<>>, [], [{show_stats, true}])),
    ?assert(has(<<"data-stat=\"paragraphs\"">>, S)),
    ?assertNot(has_quiet(<<"data-stat=\"limit\"">>, S)),
    ?assertNot(has_quiet(<<"ah-pm-stats__warning">>, S)),
    %% a CSS length
    ?assert(has(<<"style=\"height:60vh\"">>, r(?M:ah_markdown_editor(<<>>, [], [{height, <<"60vh">>}])))),
    ?assert(has(<<"style=\"height:calc(100vh - 80px)\"">>,
                r(?M:ah_markdown_editor(<<>>, [], [{height, "calc(100vh - 80px)"}])))),
    ?assertError({aihtml, {bad_height, <<"1px;color:red">>}},
                 r(?M:ah_markdown_editor(<<>>, [], [{height, <<"1px;color:red">>}]))),
    ?assertError({aihtml, {bad_max_chars, 0}}, r(?M:ah_markdown_editor(<<>>, [], [{max_chars, 0}]))),
    ?assertError({aihtml, {unknown_modifier, markdown_editor, big, _}},
                 ?M:ah_markdown_editor(<<>>, [big], [])).

readonly_disabled_test() ->
    Ro = r(?M:ah_markdown_editor(<<"x">>, [readonly], [{id, ro}, {name, n}])),
    ?assert(has(<<"class=\"ah-md-editor ah-md-editor-readonly\"">>, Ro)),
    ?assert(has(<<"data-ah-readonly">>, Ro)),
    ?assert(has(<<" readonly>x</textarea>">>, Ro)),
    %% nothing to edit with
    ?assertNot(has_quiet(<<"ah-pm-slash-menu">>, Ro)),
    ?assertNot(has_quiet(<<"ah-pm-block-handle">>, Ro)),
    Di = r(?M:ah_markdown_editor(<<"x">>, [disabled], [{id, di}])),
    ?assert(has(<<"class=\"ah-md-editor ah-md-editor-disabled\"">>, Di)),
    ?assert(has(<<"aria-disabled=\"true\"">>, Di)),
    ?assert(has(<<" disabled>x</textarea>">>, Di)),
    ?assertNot(has_quiet(<<"ah-pm-slash-menu">>, Di)).

labels_test() ->
    H = r(?M:ah_markdown_editor(<<>>, [], [{id, l}, {show_stats, true},
                                           {labels, #{heading_1 => <<"标题 1"/utf8>>,
                                                      group_list => "列表",
                                                      chars => <<"字符"/utf8>>,
                                                      editor => <<"正文"/utf8>>}}])),
    ?assert(has(<<"<span class=\"ah-pm-slash-menu-item-label\">标题 1</span>"/utf8>>, H)),
    ?assert(has(<<"data-group=\"List\">列表</button>"/utf8>>, H)),
    ?assert(has(<<"<span class=\"ah-pm-stats__label\">字符</span>"/utf8>>, H)),
    ?assert(has(<<"aria-label=\"正文\""/utf8>>, H)),
    ?assert(has(<<"&quot;editor&quot;:&quot;正文&quot;"/utf8>>, H)),
    %% label texts are escaped
    E = r(?M:ah_markdown_editor(<<>>, [], [{labels, #{paragraph => <<"<b>P</b>">>}}])),
    ?assert(has(<<"&lt;b&gt;P&lt;/b&gt;">>, E)),
    ?assertError({aihtml, {bad_markdown_editor_label, nope}},
                 r(?M:ah_markdown_editor(<<>>, [], [{labels, #{nope => <<"x">>}}]))).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := markdown_editor}] = ?M:catalog(),
    E = aihtml_catalog:entry(?M, markdown_editor),
    #{flags := Flags} = E,
    [_ | _] = aihtml_catalog:classes(E, Flags),
    %% every option and flag is documented, every behaviour method listed
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E,
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    Names = [Name || #{name := Name} <- Ms],
    [?assert(lists:member(N, Names))
     || N <- [getValue, setValue, getHtml, focus, blur, exec]],
    ?assert(lists:member(aihtml_markdown_editor, aihtml_catalog:modules())).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    Labels = #{chars => <<"C">>},
    ?assertEqual(r(?M:ah_markdown_editor(<<"# Hi">>, [readonly, <<"h-96">>],
                                         [{id, md}, {name, body}, {placeholder, <<"P">>},
                                          {show_stats, true}, {max_chars, 50}, {height, 400},
                                          {labels, Labels}, {title, <<"t">>}])),
                 r(#ah_markdown_editor{value = <<"# Hi">>, readonly = true, css = [<<"h-96">>],
                                       id = md, name = body, placeholder = <<"P">>,
                                       show_stats = true, max_chars = 50, height = 400,
                                       labels = Labels, attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    D = ?M:ah_markdown_editor(<<"x">>, [disabled, <<"c">>],
                              [{id, d}, {max_chars, 10}, {height, <<"50vh">>}, {title, <<"t">>}]),
    ?assertMatch(#ah_markdown_editor{value = <<"x">>, disabled = true, readonly = false, id = d,
                                     max_chars = 10, height = <<"50vh">>, show_stats = false,
                                     css = [<<"c">>], attrs = [{title, <<"t">>}]}, D).

generated_id_test() ->
    R = #ah_markdown_editor{},
    ?assertNotEqual(r(R), r(R)).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, saved, #{id => 7}}},
                 Token(#ah_markdown_editor{postback = {saved, #{id => 7}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_max_chars, -1}}, r(#ah_markdown_editor{max_chars = -1})),
    ?assertError({aihtml, {bad_show_stats, yes}}, r(#ah_markdown_editor{show_stats = yes})),
    ?assertError({aihtml, {bad_markdown_editor_labels, []}}, r(#ah_markdown_editor{labels = []})),
    ?assertError({aihtml, {bad_height, -5}}, r(#ah_markdown_editor{height = -5})),
    ?assertError({aihtml, {bad_flag, markdown_editor, readonly, yes}},
                 r(#ah_markdown_editor{readonly = yes})).

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

default(ah_markdown_editor) -> #ah_markdown_editor{}.
