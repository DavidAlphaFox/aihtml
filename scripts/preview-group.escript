#!/usr/bin/env escript
%%! -noshell
%% Renders a demo module's demos/0 (aihtml_example_demo_<name>) into
%% OutDir/index.html together
%% with the assets it needs (Tailwind CSS built from the current sources,
%% aihtml.js, jQuery), so the page opens from file:// in any browser.
%%
%%   escript scripts/preview-group.escript aihtml_example_demo_button OutDir [ExtraEbinDir]
%%
%% ExtraEbinDir goes first in the code path (freshly compiled modules).
main([Mod | [OutDir | Rest]]) ->
    Root = filename:dirname(filename:dirname(filename:absname(escript:script_name()))),
    [code:add_patha(D) || D <- filelib:wildcard(filename:join(Root, "_build/default/lib/*/ebin"))],
    [code:add_patha(D) || D <- Rest],
    M = list_to_atom(Mod),
    {module, M} = code:ensure_loaded(M),
    ok = filelib:ensure_path(OutDir),
    Sections = [aihtml:el(section,
                          [aihtml:el(h3, [atom_to_binary(Name), <<" · "/utf8>> | [Title]],
                                     [<<"text-sm font-bold text-muted mb-3">>], []),
                           M:Fun()],
                          [<<"border-b border-line py-6">>], [{id, atom_to_binary(Fun)}])
                || #{component := Name, demos := Demos} <- M:demos(), {Title, Fun} <- Demos],
    Body = aihtml:el('div', [aihtml:el(h1, Mod, [<<"text-xl font-bold mb-2">>], []),
                             aihtml:theme_switcher([], []), Sections],
                     [<<"max-w-5xl mx-auto p-6">>], []),
    Page = aihtml:page(Body, #{title => list_to_binary(Mod),
                               css => [<<"aihtml.css">>],
                               jquery => <<"jquery.min.js">>,
                               runtime => <<"aihtml.js">>,
                               persist => false}),
    ok = file:write_file(filename:join(OutDir, "index.html"), Page),
    Sh = fun(Cmd) -> io:put_chars(unicode:characters_to_binary(os:cmd("cd " ++ Root ++ " && " ++ Cmd))) end,
    Sh("node scripts/build-js.mjs " ++ filename:absname(filename:join(OutDir, "aihtml.js"))),
    %% inside the project, so that "tailwindcss" resolves from node_modules
    Entry = filename:join([Root, "_build", "preview", Mod ++ ".src.css"]),
    ok = filelib:ensure_dir(Entry),
    ok = file:write_file(Entry,
             ["@import \"tailwindcss\" source(none);\n",
              "@import \"", Root, "/apps/aihtml/priv/css/aihtml.css\";\n",
              "@source \"", Root, "/apps/aihtml/src\";\n",
              "@source \"", Root, "/apps/aihtml_example/src\";\n",
              "@source \"", Root, "/scripts\";\n"]),
    Sh("npx tailwindcss -i " ++ Entry ++ " -o "
       ++ filename:absname(filename:join(OutDir, "aihtml.css")) ++ " 2>&1 | tail -1"),
    {ok, _} = file:copy(filename:join(Root, "apps/aihtml/priv/static/vendor/jquery.min.js"),
                        filename:join(OutDir, "jquery.min.js")),
    %% AH.vendor loads echarts, xlsx, jspdf from vendor/ next to aihtml.js
    Vendor = filename:join(OutDir, "vendor"),
    _ = file:delete(Vendor),
    ok = file:make_symlink(filename:join(Root, "apps/aihtml/priv/static/vendor"), Vendor),
    io:format("~s~n", [filename:absname(filename:join(OutDir, "index.html"))]);
main(_) ->
    io:format("usage: preview-group.escript Module OutDir [ExtraEbinDir]~n"),
    halt(1).
