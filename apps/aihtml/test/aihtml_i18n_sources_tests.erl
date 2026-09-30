%% Checks on the sources that keep the texts in the catalogs
%% (designs/07-i18n.md): the browser's English fallbacks are en.json's,
%% and no UI text is written into the code.
-module(aihtml_i18n_sources_tests).

-include_lib("eunit/include/eunit.hrl").

%% Texts allowed in the code: {File (relative to apps/aihtml), Snippet},
%% each with its reason. Keep it short.
-define(ALLOW, []).

%% apps/aihtml, from the compile info (assets/ and templates/ are not
%% under _build).
root() ->
    Src = proplists:get_value(source, aihtml_i18n:module_info(compile)),
    filename:dirname(filename:dirname(Src)).

read(F) ->
    {ok, B} = file:read_file(F),
    B.

%% A TypeScript file without its comments (block comments, and lines that
%% are comments), so examples in them are not taken for code.
code(F) ->
    NoBlocks = re:replace(read(F), <<"/\\*.*?\\*/">>, <<>>, [global, dotall, {return, binary}]),
    re:replace(NoBlocks, <<"^\\s*//.*$">>, <<>>, [global, multiline, {return, binary}]).

en() -> json:decode(read(filename:join([code:priv_dir(aihtml), "i18n", "en.json"]))).

ts_files() ->
    [F || F <- filelib:wildcard(filename:join(root(), "assets/js/**/*.ts")),
          not lists:suffix(".d.ts", F)].

rel(F) -> list_to_binary(string:prefix(F, root() ++ "/")).

matches(Bin, Re) ->
    case re:run(Bin, Re, [global, {capture, all_but_first, binary}, unicode]) of
        {match, Ms} -> Ms;
        nomatch -> []
    end.

unescape(S) -> binary:replace(S, <<"\\\"">>, <<"\"">>, [global]).

%% Every AH.t(Scope, Key, English) in the browser code gives en.json's text,
%% for a scope the pages carry ("client"), so the English texts have one
%% source and a page in another language has every text the browser asks for.
browser_texts_match_en_test() ->
    En = en(),
    Msgs = maps:get(<<"messages">>, En),
    Client = maps:get(<<"client">>, En),
    Calls = [{rel(F), S, K, unescape(T)}
             || F <- ts_files(),
                [S, K, T] <- matches(code(F), <<"AH\\.t\\(\\s*\"([^\"]+)\",\\s*\"([^\"]+)\",\\s*"
                                               "\"((?:[^\"\\\\]|\\\\.)*)\"">>)],
    ?assert(length(Calls) > 100),
    [?assertEqual({F, S, K, T}, {F, S, K, maps:get(K, maps:get(S, Msgs, #{}), missing)})
     || {F, S, K, T} <- Calls],
    [?assertEqual({F, S, client}, {F, S, lists:member(S, Client) andalso client})
     || {F, S, _, _} <- Calls].

%% AH.format(Key, English) likewise gives en.json's setting.
browser_formats_match_en_test() ->
    Format = maps:get(<<"format">>, en()),
    Calls = [{rel(F), K, json:decode(re:replace(V, <<"\\s+">>, <<" ">>, [global, {return, binary}]))}
             || F <- ts_files(),
                [K, V] <- matches(code(F), <<"AH\\.format(?:<[^>]*>)?\\(\"([^\"]+)\",\\s*"
                                               "(\\[[^\\]]*\\]|\"[^\"]*\")\\)">>)],
    ?assert(length(Calls) >= 10),
    [?assertEqual({F, K, V}, {F, K, maps:get(K, Format, missing)}) || {F, K, V} <- Calls].

%% No UI text written into the code: the places where components put text
%% for people (accessible names, titles, placeholders, text nodes) take it
%% from the catalog (aihtml_i18n:text/2,3, AH.t, a template variable).
no_hard_coded_texts_test() ->
    Erl = [{F, <<"\\{(aria_label|aria_description|title|placeholder), <<\"[A-Z][^\"]*\"">>}
           || F <- filelib:wildcard(filename:join(root(), "src/*.erl"))],
    Tpl = [{F, <<"((aria-label|title|placeholder)=\"[A-Z][^\"{]*\"|>[A-Z][a-z]+[^<{]*<)">>}
           || F <- filelib:wildcard(filename:join(root(), "templates/*.mustache"))],
    Ts = [{F, <<"(setAttribute\\(\"(aria-label|title|placeholder)\", \"[A-Z][^\"]*\""
                "|(aria-label|title|placeholder)=\\\\?\"[A-Z][^\"]*\""
                "|\"aria-label\": \"[A-Z][^\"]*\""
                "|textContent = \"[A-Z][^\"]*\""
                "|announce\\([^,]+, \"[A-Z][^\"]*\""
                "|label: \"[A-Z][^\"]*\""
                "|empty_text: \"[A-Z][^\"]*\")">>}
          || F <- ts_files()],
    Found = [{rel(F), M} || {F, Re} <- Erl ++ Tpl ++ Ts,
                            [M | _] <- matches(read(F), Re),
                            not lists:member({rel(F), M}, ?ALLOW)],
    ?assertEqual([], Found).
