%% @doc Registry of the component demos. Each aihtml_example_demo_<group>
%% module exports demos() -> [#{component, title, summary, demos}]:
%%   component  the catalog name (button)
%%   title      the name shown on the site (<<"RadioButton">>), optional
%%   summary    one line for the home page cards, in Chinese
%%   demos      [{Title, Fun}]: every Fun is a zero-arity function of that
%%              module rendering one example, written the way an
%%              application writes it (the docs page shows its source)
%% All of this is demo-site content; the library keeps only the catalog.
-module(aihtml_example_demos).

-export([modules/0, all/0, for/1, info/1]).

-export_type([demo/0]).

-type demo() :: {Title :: binary(), module(), Fun :: atom()}.
-type info() :: #{component := atom(), title => binary(), summary => binary(),
                  demos := [{binary(), atom()}], module := module()}.

-spec modules() -> [module()].
modules() ->
    [M || G <- aihtml_catalog:groups(), G =/= aihtml_theme,
          <<"aihtml_", Rest/binary>> <- [atom_to_binary(G)],
          M <- [binary_to_atom(<<"aihtml_example_demo_", Rest/binary>>)],
          code:ensure_loaded(M) =:= {module, M}].

%% @doc Every component with its demos, in catalog order.
-spec all() -> [info()].
all() ->
    [D#{module => M} || M <- modules(), D <- M:demos()].

%% @doc The demos of one component, ready to render.
-spec for(atom()) -> [demo()].
for(Component) ->
    case info(Component) of
        #{demos := Ds, module := M} -> [{T, M, F} || {T, F} <- Ds];
        undefined -> []
    end.

%% @doc The demo-site entry of one component, if it has one.
-spec info(atom()) -> info() | undefined.
info(Component) ->
    case [D || #{component := C} = D <- all(), C =:= Component] of
        [D | _] -> D;
        [] -> undefined
    end.
