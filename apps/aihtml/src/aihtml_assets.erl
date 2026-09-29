%%%-------------------------------------------------------------------
%%% @doc The bundled browser runtime (designs/06-bundling.md): Vite writes
%%% priv/static/js with content-hashed file names and a manifest; this
%%% module reads the manifest to find the entry and the chunks it imports
%%% statically (worth a modulepreload). The manifest is cached and read
%%% again when the file changes, so a rebuild during development shows up
%%% without a restart.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_assets).

-export([entry/0]).

-export_type([entry/0]).

%% Paths relative to priv/static/js.
-type entry() :: #{file := binary(), imports := [binary()]}.

-define(KEY, {?MODULE, manifest}).

%% @doc The entry chunk and the chunks it imports statically.
-spec entry() -> entry().
entry() ->
    File = manifest_file(),
    Mtime = case file:read_file_info(File, [{time, posix}]) of
                {ok, Info} -> element(6, Info);
                {error, _} -> error({aihtml, {no_bundle, File,
                                              "run `npm run js` to build the runtime"}})
            end,
    case persistent_term:get(?KEY, undefined) of
        {Mtime, Entry} -> Entry;
        _ ->
            Entry = read(File),
            persistent_term:put(?KEY, {Mtime, Entry}),
            Entry
    end.

manifest_file() ->
    filename:join([code:priv_dir(aihtml), "static", "js", ".vite", "manifest.json"]).

read(File) ->
    {ok, Bin} = file:read_file(File),
    Manifest = json:decode(Bin),
    [Main] = [M || _ := #{<<"isEntry">> := true} = M <- Manifest],
    #{file => maps:get(<<"file">>, Main),
      imports => [maps:get(<<"file">>, maps:get(K, Manifest))
                  || K <- maps:get(<<"imports">>, Main, [])]}.
