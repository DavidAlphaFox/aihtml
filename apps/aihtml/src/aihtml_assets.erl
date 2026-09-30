%%%-------------------------------------------------------------------
%%% @doc The prebuilt browser assets in priv/static, whose file names carry
%%% content hashes (designs/06-bundling.md):
%%%
%%%   js/    Vite's bundle; its manifest names the entry and the chunks it
%%%          imports statically (worth a modulepreload)
%%%   css/   aihtml-<hash>.css, named by scripts/hash-css.mjs, with a
%%%          manifest mapping "aihtml.css" to it
%%%
%%% Each manifest is cached and read again when the file changes, so a
%%% rebuild during development shows up without a restart.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_assets).

-export([entry/0, css/0]).

-export_type([entry/0]).

%% Paths relative to priv/static/js.
-type entry() :: #{file := binary(), imports := [binary()]}.

%% @doc The entry chunk and the chunks it imports statically.
-spec entry() -> entry().
entry() ->
    cached(filename:join(["js", ".vite", "manifest.json"]), fun read/1,
           "run `npm run js` to build the runtime").

%% @doc The stylesheet's file name, relative to priv/static/css.
-spec css() -> binary().
css() ->
    cached(filename:join("css", "manifest.json"),
           fun(F) -> {ok, B} = file:read_file(F), maps:get(<<"aihtml.css">>, json:decode(B)) end,
           "run `npm run css:lib` to build the stylesheet").

%% Parse(File) of a manifest under priv/static, kept until the file changes.
cached(Rel, Parse, Hint) ->
    File = filename:join([code:priv_dir(aihtml), "static", Rel]),
    Mtime = case file:read_file_info(File, [{time, posix}]) of
                {ok, Info} -> element(6, Info);
                {error, _} -> error({aihtml, {no_bundle, File, Hint}})
            end,
    Key = {?MODULE, Rel},
    case persistent_term:get(Key, undefined) of
        {Mtime, Value} -> Value;
        _ ->
            Value = Parse(File),
            persistent_term:put(Key, {Mtime, Value}),
            Value
    end.

read(File) ->
    {ok, Bin} = file:read_file(File),
    Manifest = json:decode(Bin),
    [Main] = [M || _ := #{<<"isEntry">> := true} = M <- Manifest],
    #{file => maps:get(<<"file">>, Main),
      imports => [maps:get(<<"file">>, maps:get(K, Manifest))
                  || K <- maps:get(<<"imports">>, Main, [])]}.
