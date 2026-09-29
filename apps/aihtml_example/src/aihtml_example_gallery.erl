%% @doc GET /components: every ported component with its signature,
%% description and examples, straight from the group modules' catalog/0
%% and examples/0.
-module(aihtml_example_gallery).

-export([init/2]).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, State) ->
    {ok, aihtml_cowboy:reply(Req, page(), #{title => <<"aihtml components">>,
                                            css => [<<"/static/example.css">>]}),
     State}.

page() ->
    Groups = [{M, M:catalog(), examples(M)} || M <- aihtml_catalog:groups(),
                                              M =/= aihtml_theme,
                                              code:ensure_loaded(M) =:= {module, M}],
    Nav = [aihtml:el('div',
                     [aihtml:el(p, group_name(M), [<<"text-xs font-bold uppercase text-muted mt-4 mb-1">>], []),
                      [aihtml:el(a, atom_to_binary(N), [<<"block py-0.5 text-sm hover:text-primary">>],
                                 [{href, <<"#", (atom_to_binary(N))/binary>>}])
                       || #{name := N} <- Cat]], [], [])
           || {M, Cat, _} <- Groups],
    Sections = [section(E, [Ex || {N, _, _} = Ex <- Exs, N =:= maps:get(name, E)])
                || {_, Cat, Exs} <- Groups, E <- Cat],
    aihtml:el('div',
              [aihtml:el(header,
                         aihtml:el('div', [aihtml:el(h1, <<"aihtml components">>,
                                                     [<<"text-xl font-bold">>], []),
                                           aihtml:el(a, <<"back to the demo">>,
                                                     [<<"text-sm underline text-muted">>],
                                                     [{href, <<"/">>}]),
                                           aihtml:theme_switcher([], [])],
                                   [<<"max-w-7xl mx-auto px-4 py-4 flex flex-wrap items-end gap-6">>], []),
                         [<<"bg-surface border-b border-line">>], []),
               aihtml:el('div',
                         [aihtml:el(nav, Nav, [<<"hidden md:block w-48 shrink-0 sticky top-4 self-start"
                                                 " max-h-screen overflow-y-auto pb-8">>], []),
                          aihtml:el(main, Sections, [<<"flex-1 min-w-0">>], [])],
                         [<<"max-w-7xl mx-auto px-4 py-6 flex gap-8">>], [])],
              [<<"min-h-screen">>], []).

section(#{name := N, signature := Sig} = E, Examples) ->
    aihtml:el(section,
              [aihtml:el(h2, atom_to_binary(N), [<<"text-lg font-bold">>], []),
               aihtml:el(code, Sig, [<<"text-sm text-muted font-mono">>], []),
               aihtml:el(p, maps:get(doc, E, <<>>), [<<"text-sm mt-1 mb-4">>], []),
               [aihtml:el('div', [aihtml:el(p, Title, [<<"text-xs text-muted mb-2">>], []), Html],
                          [<<"mb-4 p-4 rounded-surface border border-line bg-surface">>], [])
                || {_, Title, Html} <- Examples]],
              [<<"py-6 border-b border-line scroll-mt-4">>], [{id, atom_to_binary(N)}]).

examples(M) ->
    case erlang:function_exported(M, examples, 0) of
        true -> M:examples();
        false -> []
    end.

group_name(M) ->
    <<"aihtml_", Name/binary>> = atom_to_binary(M),
    binary:replace(Name, <<"_">>, <<" ">>, [global]).
