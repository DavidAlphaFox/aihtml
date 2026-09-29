%% aihtml_lib_markdown renders Markdown to the same HTML as markdown-it
%% (the parser of the markdown editor): every case of
%% aihtml_lib_markdown.fixtures.txt is rendered here and by
%% scripts/render-markdown.mjs through node, and the bytes must match.
%%
%% The node script configures markdown-it as the editor does and adds the
%% view's two rules of its own (the URL policy and the task list HTML), so
%% nothing is normalised except one thing: markdown-it does not escape
%% the apostrophe, aihtml escapes it everywhere (&#39;), so the script
%% writes &#39; for every ' in markdown-it's output (markdown-it never
%% writes one of its own: each is text or an attribute value).
-module(aihtml_lib_markdown_tests).

-include_lib("eunit/include/eunit.hrl").

-define(M, aihtml_lib_markdown).

r(Md) -> ?M:render(Md).

%%%===================================================================
%%% Same HTML as markdown-it
%%%===================================================================

markdown_it_fixtures_test_() ->
    {Root, File} = fixtures(),
    Cases = read_fixtures(File),
    Out = os:cmd("cd " ++ Root ++ " && node scripts/render-markdown.mjs " ++ File),
    Js = json:decode(unicode:characters_to_binary(Out)),
    [?_assertEqual(length(Cases), length(Js))
     | [{binary_to_list(Name), ?_assertEqual({Name, Html}, {Name, r(Src)})}
        || {{Name, Src}, #{<<"name">> := Name, <<"html">> := Html}} <- lists:zip(Cases, Js)]].

%% The fixtures file and the project root (the directory holding
%% scripts/render-markdown.mjs), found from this source file or the app.
fixtures() ->
    Starts = [filename:dirname(filename:absname(?FILE)), code:lib_dir(aihtml)],
    [Root | _] = [R || S <- Starts, is_list(S), R <- [project_root(S)], R =/= none],
    {Root, filename:join([Root, "apps", "aihtml", "test", "aihtml_lib_markdown.fixtures.txt"])}.

project_root(Dir) ->
    case filelib:is_regular(filename:join([Dir, "scripts", "render-markdown.mjs"])) of
        true -> Dir;
        false ->
            case filename:dirname(Dir) of
                Dir -> none;
                Up -> project_root(Up)
            end
    end.

%% The format of scripts/render-markdown.mjs's readFixtures: a case per
%% "%%%% name" line, its lines each ending in \n (not with " [nonl]").
read_fixtures(File) ->
    {ok, Bin} = file:read_file(File),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    cases(Lines, none, []).

cases([], Cur, Acc) -> lists:reverse(finish(Cur, Acc));
cases([<<"%%%% ", Head/binary>> | Rest], Cur, Acc) ->
    cases(Rest, {Head, []}, finish(Cur, Acc));
cases([Line | Rest], {Head, Ls}, Acc) -> cases(Rest, {Head, [Line | Ls]}, Acc);
cases([_ | Rest], none, Acc) -> cases(Rest, none, Acc).

finish(none, Acc) -> Acc;
finish({Head, Ls}, Acc) ->
    Src0 = iolist_to_binary(lists:join(<<"\n">>, lists:reverse(Ls))),
    Src = case binary:last(<<" ", Src0/binary>>) of
              $\n -> binary:part(Src0, 0, byte_size(Src0) - 1);
              _ -> Src0
          end,
    case binary:split(Head, <<" [nonl]">>) of
        [Name, <<>>] -> [{Name, Src} | Acc];
        _ -> [{Head, <<Src/binary, "\n">>} | Acc]
    end.

%%%===================================================================
%%% Safety
%%%===================================================================

raw_html_is_text_test() ->
    ?assertEqual(<<"<p>&lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt;</p>\n">>,
                 r(<<"<script>alert(\"x\")</script>">>)),
    ?assertEqual(<<"<p>a &lt;img src=x onerror=alert(1)&gt; b</p>\n">>,
                 r(<<"a <img src=x onerror=alert(1)> b">>)).

unsafe_urls_are_not_links_test() ->
    [?assertEqual(nomatch, binary:match(r(Md), [<<"<a">>, <<"<img">>])) || Md <- [
        <<"[x](javascript:alert(1))">>, <<"[x](JavaScript:alert(1))">>,
        <<"[x](&#106;avascript:alert(1))">>, <<"[x](data:text/html,x)">>,
        <<"![x](data:image/png;base64,AAAA)">>, <<"<javascript:alert(1)>">>,
        <<"[x](vbscript:x)">>, <<"[x](ftp://example.com)">>,
        <<"[x][r]\n\n[r]: javascript:alert(1)">>]].

safe_urls_are_links_test() ->
    ?assertEqual(<<"<p><a href=\"https://example.com/a%20b?q=1&amp;r=%E4%B8%AD\">x</a> "
                   "<a href=\"/p\">y</a> <a href=\"#f\">z</a> "
                   "<a href=\"mailto:a@b.c\">m</a></p>\n">>,
                 r(<<"[x](<https://example.com/a b?q=1&r=中>) [y](/p) [z](#f) [m](mailto:a@b.c)"/utf8>>)).

quotes_are_escaped_in_attributes_test() ->
    ?assertEqual(<<"<p><img src=\"/a%22b\" alt=\"it&#39;s &quot;q&quot;\" "
                   "title=\"t&quot;&#39;\"></p>\n">>,
                 r(<<"![it's \"q\"](/a\"b \"t\\\"'\")">>)).

strings_and_bad_input_test() ->
    ?assertEqual(<<"<p><em>a</em></p>\n">>, r("*a*")),
    ?assertEqual(<<"<p>中</p>\n"/utf8>>, r([20013])),
    ?assertError({aihtml, {bad_markdown, not_utf8}}, r(<<16#FF>>)).

task_list_html_test() ->
    ?assertEqual(<<"<ul class=\"contains-task-list\">\n"
                   "<li class=\"task-list-item\"><input type=\"checkbox\" "
                   "class=\"task-list-item-checkbox\" disabled>todo</li>\n"
                   "<li class=\"task-list-item\"><input type=\"checkbox\" "
                   "class=\"task-list-item-checkbox\" disabled checked>done</li>\n</ul>\n">>,
                 r(<<"- [ ] todo\n- [x] done\n">>)).

deep_nesting_is_bounded_test() ->
    Deep = binary:copy(<<">">>, 5000),
    ?assert(is_binary(r(<<Deep/binary, " x">>))),
    ?assert(is_binary(r(binary:copy(<<"[">>, 5000)))),
    ?assert(is_binary(r(binary:copy(<<"*a ">>, 5000)))).
