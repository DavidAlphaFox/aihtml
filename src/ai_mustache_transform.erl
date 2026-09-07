%%%-------------------------------------------------------------------
%%% @doc parse_transform for the three mustache source forms.
%%%
%%% ```
%%% -module(my_views).
%%% -compile({parse_transform, ai_mustache_transform}).
%%%
%%% -mustache_tag(my_i18n).                              % (a)
%%% -mustache_template({index, "views/index.mustache"}). % (c)
%%%
%%% greet(Name) ->                                       % (b)
%%%     ai_mustache:inline(~"Hello {{name}}!", #{name => Name}).
%%% '''
%%%
%%% <ul>
%%%   <li><b>(a) `-mustache_tag'</b> declares and validates an extension
%%%       module. It does not assemble one: a parse_transform sees a single
%%%       module, so a declaration here says nothing about how the plugin
%%%       compiles `.mustache' files -- that is what
%%%       `{mustache_opts, [{extensions, [...]}]}' is for. The declaration
%%%       <em>is</em> used for forms (b) and (c) in this same module, which
%%%       compile right here. The attribute is consumed and does not reach the
%%%       beam.</li>
%%%   <li><b>(b) `ai_mustache:inline/2'</b> with a binary literal is expanded
%%%       in place into the iolist the template builds. Without this transform
%%%       the very same call still works, interpreted at runtime, with
%%%       identical output -- just slower. A non-literal first argument is left
%%%       alone and warns.</li>
%%%   <li><b>(c) `-mustache_template'</b> compiles a template file into
%%%       `Name/1' and `Name_iolist/1' in this module and exports them. Unlike
%%%       (a) and (b) this one hard-fails without the transform: the functions
%%%       simply are not there. The attribute is deliberately <em>kept</em> in
%%%       the beam -- see inject_all/2.</li>
%%% </ul>
%%%
%%% == Erlang has no user-defined sigils ==
%%%
%%% OTP's sigils are a closed set (`~"..."', `~b', `~B', `~s', `~S') and an
%%% unknown one is a lexical error, so `~mustache"..."' never reaches a
%%% parse_transform at all. Form (b) therefore keys off an ordinary remote
%%% call with a literal argument. `~"Hello {{x}}"' works because it is the
%%% standard string sigil and produces a plain binary literal, exactly like
%%% `<<"Hello {{x}}">>'.
%%%
%%% == Extension tags are not available on the fallback path ==
%%%
%%% `compile_tag/4' returns an abstract expression, which is meaningless at
%%% runtime. An inline template that uses a custom marker therefore requires
%%% this transform; interpreted, the same template raises
%%% `{unknown_marker, Char}'.
%%%
%%% == Known limitation: rebar3 does not see template files ==
%%%
%%% A parse_transform cannot register extra file dependencies with rebar3 --
%%% only `-include' is tracked. Editing `views/index.mustache' therefore does
%%% not on its own rebuild the module carrying `-mustache_template'. The rebar3
%%% plugin closes this by scanning for the attribute (via
%%% {@link ai_mustache_path:scan/1}) and touching the `.erl' when the
%%% template's hash changes. Without the plugin, form (c) is
%%% plugin-recommended rather than plugin-free: `make clean' or a manual touch
%%% is the fallback.
%%%
%%% == The three passes ==
%%%
%%% <ol>
%%%   <li>collect: read attributes, validate, build state; touches no form;</li>
%%%   <li>expand: rewrite in place -- expand inline calls, drop consumed
%%%       attributes;</li>
%%%   <li>inject: append generated functions and their export.</li>
%%% </ol>
%%%
%%% All three run to completion before a result is decided, so one compilation
%%% reports every problem instead of only the first.
%%%
%%% See designs/06-parse-transform.md and tasks/T22.md -- T25.md.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_transform).

-export([parse_transform/2, format_error/1]).

%% Stand-in for the context expression while the expansion is alpha-renamed.
%% Substituted afterwards so that the user's own variables never get suffixed.
-define(CTX_VAR, 'MustacheCtx__').

-record(st, {file           :: file:filename_all(),
             module         :: module() | undefined,
             opts           :: [term()],
             %% {Marker, Module, Anno} -- one entry per claimed character
             ext_claims = [] :: [{char(), module(), erl_anno:anno()}],
             ext_mods   = [] :: [module()],
             %% {Name, Path, Anno}
             templates  = [] :: [{atom(), string(), erl_anno:anno()}],
             defined    = [] :: [{atom(), arity()}],
             counter    = 0  :: non_neg_integer(),
             errors     = [] :: [diag()],
             warnings   = [] :: [diag()]}).

-type diag()  :: {file:filename_all(), erl_anno:location(), term()}.
-type forms() :: [erl_parse:abstract_form() | {eof, erl_anno:anno()}].

%%%===================================================================
%%% Entry point
%%%===================================================================

-spec parse_transform(forms(), [term()]) ->
          forms()
        | {warning, forms(), [{file:filename_all(), [tuple()]}]}
        | {error, [{file:filename_all(), [tuple()]}],
                  [{file:filename_all(), [tuple()]}]}.
parse_transform(Forms, Opts) ->
    File = file_of(Forms),
    St0 = #st{file = File, module = module_of(Forms), opts = Opts},
    %% pass 1: collect -mustache_tag / -mustache_template, validate extensions
    St1 = lists:foldl(fun collect/2, St0, Forms),
    St2 = check_assembly(St1#st{file = File}),
    %% pass 2: expand ai_mustache:inline/2, drop consumed attributes
    {Forms1, St3} = lists:mapfoldl(fun expand/2, St2#st{file = File}, Forms),
    Forms2 = [F || F <- Forms1, F =/= drop],
    %% pass 3: append the functions -mustache_template asks for
    {Forms3, St4} = inject_all(Forms2, St3#st{file = File}),
    result(Forms3, St4).

-spec result(forms(), #st{}) -> term().
result(Forms, #st{errors = [], warnings = []}) ->
    Forms;
result(Forms, #st{errors = [], warnings = Ws}) ->
    {warning, Forms, group(Ws)};
result(_Forms, #st{errors = Es, warnings = Ws}) ->
    {error, group(Es), group(Ws)}.

%% erlc wants diagnostics grouped by file; putting two files' problems into one
%% group makes it print the wrong path for half of them.
-spec group([diag()]) -> [{file:filename_all(), [tuple()]}].
group(Diags0) ->
    Diags = lists:reverse(Diags0),
    Files = lists:usort([F || {F, _, _} <- Diags]),
    [{F, lists:keysort(1, [{Loc, ?MODULE, R} || {F1, Loc, R} <- Diags, F1 =:= F])}
     || F <- Files].

%%%===================================================================
%%% Pass 1: collect
%%%===================================================================

-spec collect(term(), #st{}) -> #st{}.
collect({attribute, _, file, {F, _}}, St) ->
    St#st{file = F};
collect({attribute, _, module, M}, St) ->
    St#st{module = M};
collect({attribute, A, mustache_tag, Term}, St) ->
    collect_tag(A, Term, St);
collect({attribute, A, mustache_template, Term}, St) ->
    collect_template(A, Term, St);
collect({function, _, Name, Arity, _}, St) ->
    St#st{defined = [{Name, Arity} | St#st.defined]};
collect(_, St) ->
    St.

%%%-------------------------------------------------------------------
%%% (a) -mustache_tag
%%%-------------------------------------------------------------------

%% Three shapes, and only three:
%%   -mustache_tag(my_i18n).                    every marker my_i18n claims
%%   -mustache_tag({$@, my_i18n}).              just $@
%%   -mustache_tag({$@, {my_i18n, compile_at}}) just $@, custom callback name
%% The bare-module shape is the one to prefer: the marker set then has exactly
%% one definition, in the extension's own markers/0, and cannot fall out of
%% step with the attribute.
-spec collect_tag(erl_anno:anno(), term(), #st{}) -> #st{}.
collect_tag(A, Mod, St) when is_atom(Mod) ->
    add_tag(A, Mod, all, St);
collect_tag(A, {C, Mod}, St) when is_integer(C), is_atom(Mod) ->
    add_tag(A, Mod, {one, C, compile_tag}, St);
collect_tag(A, {C, {Mod, Fun}}, St)
  when is_integer(C), is_atom(Mod), is_atom(Fun) ->
    add_tag(A, Mod, {one, C, Fun}, St);
collect_tag(A, Term, St) ->
    error_at(A, {bad_mustache_tag, Term}, St).

-spec add_tag(erl_anno:anno(), module(), all | {one, char(), atom()}, #st{}) -> #st{}.
add_tag(A, Mod, Which, St0) ->
    St1 = St0#st{ext_mods = St0#st.ext_mods ++ [Mod]},
    case pre_check_marker(A, Mod, Which, St1) of
        {error, St2} -> St2;
        ok ->
            case ai_mustache_ext:describe(Mod, vopts(St1)) of
                {error, Reason} -> error_at(A, Reason, St1);
                {ok, #{markers := Declared}} ->
                    claim(A, Mod, Which, Declared, St1)
            end
    end.

%% The attribute's own character is checked before the module is even looked
%% at, so `-mustache_tag({$#, whatever})' says "reserved marker" rather than
%% "no such module".
-spec pre_check_marker(erl_anno:anno(), module(), all | {one, char(), atom()},
                       #st{}) -> ok | {error, #st{}}.
pre_check_marker(_A, _Mod, all, _St) ->
    ok;
pre_check_marker(A, Mod, {one, C, _Fun}, St) ->
    case {ai_mustache_ext:is_valid_marker(C), ai_mustache_ext:is_reserved(C)} of
        {false, _}   -> {error, error_at(A, {invalid_marker, C, Mod}, St)};
        {true, true} -> {error, error_at(A, {marker_reserved, C, Mod}, St)};
        _            -> ok
    end.

-spec claim(erl_anno:anno(), module(), all | {one, char(), atom()}, [char()],
            #st{}) -> #st{}.
claim(A, Mod, all, Declared, St) ->
    lists:foldl(fun(C, Acc) -> add_claim(A, C, Mod, Acc) end, St, Declared);
claim(A, Mod, {one, C, Fun}, Declared, St0) ->
    St1 = check_callback_name(A, Mod, Fun, St0),
    case Declared =:= [] orelse lists:member(C, Declared) of
        false -> error_at(A, {marker_not_declared, C, Mod, Declared}, St1);
        true  -> add_claim(A, C, Mod, St1)
    end.

%% designs/06 fixes the entry point at compile_tag/4; the {Mod, Fun} shape
%% survives only so an older attribute still compiles. Dispatch ignores Fun, so
%% say so rather than silently calling something else.
-spec check_callback_name(erl_anno:anno(), module(), atom(), #st{}) -> #st{}.
check_callback_name(_A, _Mod, compile_tag, St) ->
    St;
check_callback_name(A, Mod, Fun, St) ->
    warn_at(A, {ext_callback_name_ignored, Mod, Fun}, St).

-spec add_claim(erl_anno:anno(), char(), module(), #st{}) -> #st{}.
add_claim(A, C, Mod, St) ->
    case [M || {C1, M, _} <- St#st.ext_claims, C1 =:= C, M =/= Mod] of
        [] ->
            St#st{ext_claims = [{C, Mod, A} | St#st.ext_claims]};
        Others ->
            error_at(A, {marker_conflict, C, lists:usort([Mod | Others])}, St)
    end.

%% A -mustache_tag that names a module the build does not assemble is the
%% mistake this attribute exists to catch, so say so as soon as the options are
%% explicit enough to prove it. Only when `extensions' is actually configured:
%% a module that uses forms (b)/(c) alone needs no such option.
-spec check_assembly(#st{}) -> #st{}.
check_assembly(#st{ext_mods = []} = St) ->
    St;
check_assembly(St) ->
    case proplists:get_value(extensions, mustache_opts(St), undefined) of
        undefined -> St;
        Exts ->
            Missing = [{M, A} || {_C, M, A} <- lists:reverse(St#st.ext_claims),
                                 not lists:member(M, Exts)],
            lists:foldl(fun({M, A}, Acc) ->
                                warn_at(A, {ext_not_assembled, M}, Acc)
                        end, St, lists:usort(Missing))
    end.

%%%-------------------------------------------------------------------
%%% (c) -mustache_template
%%%-------------------------------------------------------------------

-spec collect_template(erl_anno:anno(), term(), #st{}) -> #st{}.
collect_template(A, Term, St) ->
    case ai_mustache_path:template_spec(Term) of
        error ->
            error_at(A, {bad_mustache_template, Term}, St);
        {ok, {Name, Path}} ->
            case lists:keymember(Name, 1, St#st.templates) of
                true  -> error_at(A, {duplicate_template_name, Name}, St);
                false -> St#st{templates = St#st.templates ++ [{Name, Path, A}]}
            end
    end.

%%%===================================================================
%%% Pass 2: expand
%%%===================================================================

-spec expand(term(), #st{}) -> {term(), #st{}}.
expand({attribute, _, file, {F, _}} = Form, St) ->
    {Form, St#st{file = F}};
expand({attribute, _, mustache_tag, _}, St) ->
    %% Consumed. Leaving it behind would pollute module_info(attributes) with
    %% something that means nothing at runtime.
    {drop, St};
expand({function, A, Name, Arity, Clauses}, St) ->
    {Clauses1, St1} = expr(Clauses, St),
    {{function, A, Name, Arity, Clauses1}, St1};
expand(Form, St) ->
    {Form, St}.

%%%-------------------------------------------------------------------
%%% Expression traversal
%%%-------------------------------------------------------------------

%% The walk has to reach every expression position -- a missed one silently
%% degrades an inline call to the runtime path, which is the hardest kind of
%% bug to notice. Rather than enumerate the node types (and forget one when OTP
%% adds a construct), the default clause descends through every tuple and list.
%% The clauses above it exist for the positions where blind descent would be
%% wrong: a clause's patterns and guards, and a comprehension generator's
%% pattern, are not expression positions, and rewriting there would turn
%% invalid code into differently invalid code.
-spec expr(term(), #st{}) -> {term(), #st{}}.
expr({call, A, {remote, _, {atom, _, ai_mustache}, {atom, _, inline}},
      [Tpl, Ctx]} = Call, St) ->
    inline_call(A, Tpl, Ctx, Call, St);

expr({clause, A, Patterns, Guards, Body}, St) ->
    {Body1, St1} = expr(Body, St),
    {{clause, A, Patterns, Guards, Body1}, St1};

expr({Gen, A, Pattern, Source}, St)
  when Gen =:= generate; Gen =:= generate_strict;
       Gen =:= b_generate; Gen =:= b_generate_strict;
       Gen =:= m_generate; Gen =:= m_generate_strict ->
    {Source1, St1} = expr(Source, St),
    {{Gen, A, Pattern, Source1}, St1};

expr({'fun', _, {function, _, _}} = F, St) ->
    {F, St};

expr(T, St) when is_tuple(T) ->
    {L, St1} = expr(tuple_to_list(T), St),
    {list_to_tuple(L), St1};
expr([H | T], St) ->
    {H1, St1} = expr(H, St),
    {T1, St2} = expr(T, St1),
    {[H1 | T1], St2};
expr(Other, St) ->
    {Other, St}.

%%%-------------------------------------------------------------------
%%% (b) ai_mustache:inline/2
%%%-------------------------------------------------------------------

%% Only the full remote call with exactly two arguments is matched. A local
%% inline/2 is somebody else's function and any other arity is not this one.
-spec inline_call(erl_anno:anno(), term(), term(), term(), #st{}) ->
          {term(), #st{}}.
inline_call(A, Tpl, Ctx, Call, St0) ->
    %% Both arguments are walked first: either may itself contain an inline
    %% call, and the context expression is spliced into the expansion as is.
    {Tpl1, St1} = expr(Tpl, St0),
    {Ctx1, St2} = expr(Ctx, St1),
    case literal_binary(Tpl1) of
        error ->
            {rebuild(Call, Tpl1, Ctx1), warn_not_literal(A, St2)};
        {ok, Bin} ->
            do_expand(A, Bin, Ctx1, rebuild(Call, Tpl1, Ctx1), St2)
    end.

-spec rebuild(term(), term(), term()) -> term().
rebuild({call, A, Remote, _}, Tpl, Ctx) -> {call, A, Remote, [Tpl, Ctx]}.

%% `<<"a">>', `~"a"' and `<<"a", "b">>' are all literal binaries; anything with
%% a variable, a size or a type specifier is not. erl_parse:normalise/1 decides
%% by trying to evaluate the construction, so no case has to be enumerated.
-spec literal_binary(term()) -> {ok, binary()} | error.
literal_binary({bin, _, _} = Expr) ->
    try erl_parse:normalise(Expr) of
        Bin when is_binary(Bin) -> {ok, Bin};
        _                       -> error
    catch
        _:_ -> error
    end;
literal_binary(_) ->
    error.

-spec warn_not_literal(erl_anno:anno(), #st{}) -> #st{}.
warn_not_literal(A, St) ->
    case lists:member(nowarn_mustache_inline, St#st.opts) of
        true  -> St;
        false -> warn_at(A, inline_not_literal, St)
    end.

-spec do_expand(erl_anno:anno(), binary(), term(), term(), #st{}) ->
          {term(), #st{}}.
do_expand(A, Bin, CtxExpr, Call, St) ->
    Opts = inline_opts(St),
    case ai_mustache_parser:parse(Bin, Opts) of
        {error, {_F, TplLine, Reason}} ->
            {Call, error_at(shift(A, TplLine), Reason, St)};
        {ok, Nodes} ->
            case partials_of(Nodes) of
                [{Loc, Name} | _] ->
                    {Call, error_at(shift(A, element(1, Loc)),
                                    {inline_partial_unsupported, Name}, St)};
                [] ->
                    build_inline(A, Nodes, CtxExpr, Call, Opts, St)
            end
    end.

-spec build_inline(erl_anno:anno(), list(), term(), term(), map(), #st{}) ->
          {term(), #st{}}.
build_inline(A, Nodes, CtxExpr, Call, Opts, St) ->
    N = St#st.counter + 1,
    Placeholder = {var, erl_anno:new(0), ?CTX_VAR},
    case ai_mustache_compiler:compile_inline(Nodes, Placeholder, Opts) of
        {error, {_F, TplLine, Reason}} ->
            {Call, error_at(shift(A, TplLine), Reason, St)};
        {ok, Expr0} ->
            %% Annotations first, substitution second: mapping annotations over
            %% the finished expression would overwrite the line numbers of the
            %% user's own context expression and send its stack frames to the
            %% wrong line.
            Expr1 = erl_parse:map_anno(fun(_) -> A end, Expr0),
            Expr = subst(Expr1, N, CtxExpr),
            {Expr, St#st{counter = N}}
    end.

%% ai_mustache_compiler always names the stack `S' and the indent `I'. Two
%% inline calls in one function would then bind `S' twice -- the second match
%% would test against the first's value instead of binding -- and either would
%% clash with a user variable of the same name. Every generated variable is
%% therefore suffixed with the call's serial number. Only names the compiler
%% produced are touched: the context expression is substituted afterwards, so
%% the user's own variables never pass through here.
-spec subst(term(), non_neg_integer(), term()) -> term().
subst({var, _, ?CTX_VAR}, _N, CtxExpr) ->
    CtxExpr;
subst({var, A, '_'}, _N, _CtxExpr) ->
    {var, A, '_'};
subst({var, A, V}, N, _CtxExpr) ->
    {var, A, list_to_atom(atom_to_list(V) ++ "__" ++ integer_to_list(N))};
subst(T, N, CtxExpr) when is_tuple(T) ->
    list_to_tuple(subst(tuple_to_list(T), N, CtxExpr));
subst([H | T], N, CtxExpr) ->
    [subst(H, N, CtxExpr) | subst(T, N, CtxExpr)];
subst(Other, _N, _CtxExpr) ->
    Other.

%% An inline template is a literal inside the .erl, so a problem on its line 3
%% belongs on the line of the call plus two. Editors can then jump to it.
-spec shift(erl_anno:anno(), term()) -> erl_anno:location().
shift(A, TplLine) ->
    erl_anno:line(A) + line_of(TplLine) - 1.

-spec line_of(term()) -> integer().
line_of(L) when is_integer(L) -> L;
line_of(L)                    -> erl_anno:line(L).

-spec partials_of(list()) -> [{tuple(), term()}].
partials_of(Nodes) ->
    lists:append([partial_of(N) || N <- Nodes]).

-spec partial_of(tuple()) -> [{tuple(), term()}].
partial_of({partial, Loc, Name, _})  -> [{Loc, Name}];
partial_of({section, _, _, B})       -> partials_of(B);
partial_of({inverted, _, _, B})      -> partials_of(B);
partial_of({has, _, _, B, _})        -> partials_of(B);
partial_of({ext, _, _, _, B})        -> partials_of(B);
partial_of(_)                        -> [].

%%%===================================================================
%%% Pass 3: inject
%%%===================================================================

%% -mustache_template is NOT dropped the way -mustache_tag is. Keeping it means
%% Mod:module_info(attributes) still says which templates a module was built
%% from, which is what makes the plugin's staleness fallback and any future
%% hot-reload support possible. Removing it here would look like tidying up and
%% would quietly break both.
-spec inject_all(forms(), #st{}) -> {forms(), #st{}}.
inject_all(Forms, #st{templates = []} = St) ->
    {Forms, St};
inject_all(Forms, St0) ->
    {Blocks, Exports, St1} =
        lists:foldl(fun(T, Acc) -> inject_one(T, Acc) end,
                    {[], [], St0}, St0#st.templates),
    case Exports of
        [] -> {Forms, St1};
        _  -> {splice(Forms, lists:reverse(Exports), lists:reverse(Blocks), St1),
               St1}
    end.

-spec inject_one({atom(), string(), erl_anno:anno()},
                 {[forms()], [{atom(), arity()}], #st{}}) ->
          {[forms()], [{atom(), arity()}], #st{}}.
inject_one({Name, Path, A}, {Blocks, Exports, St}) ->
    IolistName = iolist_name(Name),
    case clash(Name, IolistName, St) of
        {clash, Fun} ->
            {Blocks, Exports, error_at(A, {template_name_clash, Fun}, St)};
        ok ->
            case resolve(Path, St) of
                {error, Tried} ->
                    {Blocks, Exports,
                     error_at(A, {template_not_found, Path, Tried}, St)};
                {ok, Abs} ->
                    compile_template(Name, IolistName, Abs, A,
                                     {Blocks, Exports, St})
            end
    end.

-spec clash(atom(), atom(), #st{}) -> ok | {clash, {atom(), arity()}}.
clash(Name, IolistName, St) ->
    case [F || F <- [{Name, 1}, {IolistName, 1}],
               lists:member(F, St#st.defined)] of
        [F | _] -> {clash, F};
        []      -> ok
    end.

-spec iolist_name(atom()) -> atom().
iolist_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_iolist").

-spec compile_template(atom(), atom(), file:filename_all(), erl_anno:anno(),
                       {[forms()], [{atom(), arity()}], #st{}}) ->
          {[forms()], [{atom(), arity()}], #st{}}.
compile_template(Name, IolistName, Abs, A, {Blocks, Exports, St}) ->
    Opts = template_opts(Abs, St),
    case read_and_compile(Abs, Opts) of
        {error, {File, Line, Reason}} ->
            %% Reported against the .mustache at its own line, not against the
            %% .erl: that is the file the user has to edit.
            {Blocks, Exports,
             add(errors, {to_list(File), line_of(Line), Reason}, St)};
        {ok, Generated, Deps} ->
            St1 = warn_partials(A, Name, Deps, St),
            Block = fragment(Name, IolistName, Abs, Generated),
            {[Block | Blocks], [{IolistName, 1}, {Name, 1} | Exports], St1}
    end.

-spec read_and_compile(file:filename_all(), map()) ->
          {ok, [erl_parse:abstract_form()], [module()]} | {error, tuple()}.
read_and_compile(Abs, Opts) ->
    case file:read_file(Abs) of
        {error, Posix} ->
            {error, {Abs, 1, {template_unreadable, Posix}}};
        {ok, Body} ->
            case ai_mustache_parser:parse(Body, Opts) of
                {error, E}     -> {error, E};
                {ok, Nodes}    ->
                    case ai_mustache_compiler:forms(Nodes, Opts) of
                        {error, E}          -> {error, E};
                        {ok, Forms, Deps}   -> {ok, Forms, Deps}
                    end
            end
    end.

%% Partials compile to calls into other generated modules, which only exist if
%% something compiled the views directory. That is the plugin's job; a project
%% may still have done it by hand, so this is a warning and not an error.
-spec warn_partials(erl_anno:anno(), atom(), [module()], #st{}) -> #st{}.
warn_partials(_A, _Name, [], St) ->
    St;
warn_partials(A, Name, Deps, St) ->
    case proplists:get_value(views, mustache_opts(St), undefined) of
        undefined -> warn_at(A, {template_partial_needs_plugin, Name, Deps}, St);
        _         -> St
    end.

%%%-------------------------------------------------------------------
%%% Turning a generated module into an injectable fragment
%%%-------------------------------------------------------------------

%% ai_mustache_compiler:forms/2 builds a whole module, which is the wrong shape
%% here: what is needed is a handful of functions to graft onto somebody else's
%% module. The shell (-module, -export, -mustache_source) is dropped, render/1
%% and render_iolist/1 take the names the attribute asked for and every
%% remaining function is renamed to a private form that no user can write by
%% hand -- `-mustache-index-render_stack-'. Two -mustache_template attributes in
%% one module therefore cannot collide over sec_1/2.
%%
%% render_stack/1 and partials/0 are dropped rather than renamed: nothing calls
%% them once the export is gone and an unused private function is a warning.
-spec fragment(atom(), atom(), file:filename_all(),
               [erl_parse:abstract_form()]) -> forms().
fragment(Name, IolistName, Abs, Generated) ->
    Renames = #{{render, 1}        => Name,
                {render_iolist, 1} => IolistName},
    Dropped = [{render_stack, 1}, {partials, 0}],
    %% Only functions the generated module actually defines are renamed. The
    %% generated code also calls hd/1, is_list/1 and friends as local calls,
    %% and renaming those would turn working guards into calls to nothing.
    Defined = [{F, A} || {function, _, F, A, _} <- Generated],
    Rename = fun(F, A) ->
                     case {lists:member({F, A}, Defined),
                           maps:get({F, A}, Renames, undefined)} of
                         {false, _}     -> F;
                         {true, undefined} -> private_name(Name, F);
                         {true, New}    -> New
                     end
             end,
    Body = [rename_form(Form, Rename)
            || Form <- Generated, keep_form(Form, Dropped)],
    %% A -file marker makes stack frames and any diagnostics from the generated
    %% code point at the template rather than at a line of the .erl that has
    %% nothing to do with it.
    [{attribute, erl_anno:new(0), file, {to_list(Abs), 1}} | Body].

-spec keep_form(term(), [{atom(), arity()}]) -> boolean().
keep_form({attribute, _, module, _}, _)          -> false;
keep_form({attribute, _, export, _}, _)          -> false;
keep_form({attribute, _, mustache_source, _}, _) -> false;
keep_form({function, _, F, A, _}, Dropped)       -> not lists:member({F, A}, Dropped);
keep_form({attribute, _, spec, {{F, A}, _}}, Dropped) ->
    not lists:member({F, A}, Dropped);
keep_form(_, _)                                  -> true.

-spec rename_form(term(), fun((atom(), arity()) -> atom())) -> term().
rename_form({function, A, F, Arity, Clauses}, Rename) ->
    {function, A, Rename(F, Arity), Arity, rename_calls(Clauses, Rename)};
rename_form({attribute, A, spec, {{F, Arity}, Types}}, Rename) ->
    {attribute, A, spec, {{Rename(F, Arity), Arity}, Types}};
rename_form(Form, Rename) ->
    rename_calls(Form, Rename).

%% Local calls and local fun references have to follow the rename. Remote calls
%% must not: `ai_mustache_rt:lookup/2' is not one of ours to rename.
-spec rename_calls(term(), fun((atom(), arity()) -> atom())) -> term().
rename_calls({call, A, {atom, AA, F}, Args}, Rename) ->
    {call, A, {atom, AA, Rename(F, length(Args))}, rename_calls(Args, Rename)};
rename_calls({'fun', A, {function, F, Arity}}, Rename) ->
    {'fun', A, {function, Rename(F, Arity), Arity}};
rename_calls(T, Rename) when is_tuple(T) ->
    list_to_tuple(rename_calls(tuple_to_list(T), Rename));
rename_calls([H | T], Rename) ->
    [rename_calls(H, Rename) | rename_calls(T, Rename)];
rename_calls(Other, _Rename) ->
    Other.

%% Hyphens make this unwritable as an ordinary atom, so it cannot collide with
%% anything the user defined.
-spec private_name(atom(), atom()) -> atom().
private_name(Template, Fun) ->
    list_to_atom("-mustache-" ++ atom_to_list(Template) ++ "-"
                 ++ atom_to_list(Fun) ++ "-").

%% Erlang wants every -export before the first function definition, so the new
%% one goes just ahead of it; the generated functions go at the very end, before
%% {eof, _}, followed by a -file marker restoring the .erl.
-spec splice(forms(), [{atom(), arity()}], [forms()], #st{}) -> forms().
splice(Forms, Exports, Blocks, St) ->
    A = erl_anno:new(0),
    Export = {attribute, A, export, Exports},
    Restore = {attribute, A, file, {to_list(St#st.file), 1}},
    %% The eof split has to come first: a module with no function definitions at
    %% all would otherwise put the export after {eof, _}, where erl_lint reports
    %% it as "attribute export after function definitions" and the generated
    %% functions all read as unused.
    {Main, Eof}  = lists:splitwith(fun(F) -> element(1, F) =/= eof end, Forms),
    {Head, Body} = lists:splitwith(fun(F) -> element(1, F) =/= function end, Main),
    Head ++ [Export] ++ Body ++ lists:append(Blocks) ++ [Restore] ++ Eof.

%%%===================================================================
%%% Options
%%%===================================================================

%% rebar3 passes {mustache_opts, [...]} through erl_opts; a hand-written erlc
%% line can pass a map just as well.
-spec mustache_opts(#st{}) -> [{atom(), term()}].
mustache_opts(#st{opts = Opts}) ->
    lists:foldl(fun({mustache_opts, L}, Acc) when is_list(L) -> Acc ++ L;
                   ({mustache_opts, M}, Acc) when is_map(M)  -> Acc ++ maps:to_list(M);
                   (_, Acc)                                  -> Acc
                end, [], Opts).

%% Extensions declared here plus extensions the build assembled. Same-module
%% forms (b) and (c) are the one place a -mustache_tag genuinely takes effect.
-spec ext_modules(#st{}) -> [module()].
ext_modules(St) ->
    Declared = St#st.ext_mods,
    Assembled = proplists:get_value(extensions, mustache_opts(St), []),
    lists:usort(Declared ++ Assembled).

-spec vopts(#st{}) -> ai_mustache_ext:vopts().
vopts(St) ->
    #{search_dirs => [filename:dirname(St#st.file)]
                     ++ [D || {i, D} <- St#st.opts]}.

-spec base_opts(#st{}) -> map().
base_opts(St) ->
    Base = maps:from_list(mustache_opts(St)),
    Base#{module     => St#st.module,
          extensions => ext_modules(St)}.

-spec inline_opts(#st{}) -> map().
inline_opts(St) ->
    (base_opts(St))#{source => to_binary(St#st.file)}.

-spec template_opts(file:filename_all(), #st{}) -> map().
template_opts(Abs, St) ->
    (base_opts(St))#{source => to_binary(Abs)}.

%% Module directory first, then the configured views root, then every -I, then
%% the cwd as a last resort. The plugin resolves the same path the same way --
%% if it did not, it would touch a different file from the one read here and
%% the staleness fallback would do nothing.
-spec resolve(string(), #st{}) ->
          {ok, file:filename_all()} | {error, [file:filename_all()]}.
resolve(Path, St) ->
    Dirs = [filename:dirname(St#st.file)]
        ++ views_dirs(St)
        ++ [D || {i, D} <- St#st.opts]
        ++ ["."],
    case ai_mustache_path:resolve(Path, Dirs) of
        {ok, Abs}                 -> {ok, Abs};
        {error, {not_found, Tried}} -> {error, Tried}
    end.

-spec views_dirs(#st{}) -> [file:filename_all()].
views_dirs(St) ->
    case proplists:get_value(views, mustache_opts(St), undefined) of
        undefined            -> [];
        L when is_list(L), is_list(hd(L)) -> L;
        D                    -> [D]
    end.

%%%===================================================================
%%% Diagnostics
%%%===================================================================

-spec error_at(erl_anno:anno() | erl_anno:location(), term(), #st{}) -> #st{}.
error_at(A, Reason, St) -> add(errors, {St#st.file, loc(A), Reason}, St).

-spec warn_at(erl_anno:anno() | erl_anno:location(), term(), #st{}) -> #st{}.
warn_at(A, Reason, St) -> add(warnings, {St#st.file, loc(A), Reason}, St).

-spec add(errors | warnings, diag(), #st{}) -> #st{}.
add(errors, D, St)   -> St#st{errors = [D | St#st.errors]};
add(warnings, D, St) -> St#st{warnings = [D | St#st.warnings]}.

-spec loc(term()) -> erl_anno:location().
loc(A) when is_integer(A)                    -> A;
loc({L, C}) when is_integer(L), is_integer(C) -> {L, C};
loc(A)                                       -> erl_anno:location(A).

%% @doc Readable text for every diagnostic this transform produces.
%%
%% Extension problems are handed to ai_mustache_ext so that the message a user
%% sees while compiling a module and the message the plugin prints while
%% assembling extensions are the same words.
-spec format_error(term()) -> string().
format_error({bad_mustache_tag, Term}) ->
    io_lib:format(
      "bad -mustache_tag(~p). Write one of:~n"
      "    -mustache_tag(my_ext).                     % every marker it claims~n"
      "    -mustache_tag({$@, my_ext}).               % just $@~n"
      "    -mustache_tag({$@, {my_ext, compile_tag}}).",
      [Term]);
format_error({ext_not_assembled, Mod}) ->
    io_lib:format(
      "-mustache_tag declares ~w but it is not listed in "
      "{mustache_opts, [{extensions, [...]}]}. The attribute only validates the "
      "extension; templates compiled by the rebar3 plugin will not see it until "
      "it is added there. (Inline and -mustache_template templates in this "
      "module do use it.)", [Mod]);
format_error({ext_callback_name_ignored, Mod, Fun}) ->
    io_lib:format(
      "-mustache_tag names ~w:~w/4, but the behaviour fixes the entry point at "
      "compile_tag/4 and that is what will be called. Drop the function name.",
      [Mod, Fun]);
format_error(inline_not_literal) ->
    "the first argument of ai_mustache:inline/2 is not a binary literal, so it "
    "cannot be expanded at compile time; this call will parse and interpret the "
    "template on every invocation. Use <<\"...\">> or ~\"...\" to get the "
    "compiled path, or add nowarn_mustache_inline to silence this.";
format_error({inline_partial_unsupported, Name}) ->
    io_lib:format(
      "{{> ~ts}}: an inline template cannot use partials -- there is no views "
      "directory to resolve them against. Move this template into a file.",
      [Name]);
format_error(partial_in_inline_template) ->
    "an inline template cannot use partials -- there is no views directory to "
    "resolve them against. Move this template into a file.";
format_error({bad_mustache_template, Term}) ->
    io_lib:format(
      "bad -mustache_template(~p). Write one of:~n"
      "    -mustache_template({index, \"views/index.mustache\"}).~n"
      "    -mustache_template(\"views/index.mustache\").   % name from basename",
      [Term]);
format_error({duplicate_template_name, Name}) ->
    io_lib:format(
      "two -mustache_template attributes both generate ~w/1. Give one of them "
      "an explicit different name.", [Name]);
format_error({template_name_clash, {F, A}}) ->
    io_lib:format(
      "-mustache_template would generate ~w/~w, but this module already defines "
      "it. Rename one of the two.", [F, A]);
format_error({template_not_found, Path, Tried}) ->
    io_lib:format(
      "template ~ts not found. Searched:~n~ts",
      [Path, [io_lib:format("      ~ts~n", [to_list(D)]) || D <- Tried]]);
format_error({template_unreadable, Posix}) ->
    io_lib:format("template could not be read: ~ts", [file:format_error(Posix)]);
format_error({template_partial_needs_plugin, Name, Deps}) ->
    io_lib:format(
      "the template behind ~w/1 uses partials, which compile to calls into ~ts. "
      "Those modules are produced by the rebar3 aihtml plugin; without it (or a "
      "hand-rolled equivalent) the call fails with undef at render time.",
      [Name, string:join([lists:flatten(io_lib:format("~w", [D]))
                          || D <- Deps], ", ")]);
format_error({unclosed_tag, Keys}) ->
    io_lib:format("unclosed tag {{#~ts}}", [keys_str(Keys)]);
format_error({mismatched_close, Want, Got}) ->
    io_lib:format("{{/~ts}} closes {{#~ts}}",
                  [keys_str(Got), keys_str(Want)]);
format_error({partial_not_found, Name}) ->
    io_lib:format("partial ~ts not found", [Name]);
format_error(Reason) ->
    ai_mustache_ext:format_error(Reason).

-spec keys_str([atom()]) -> string().
keys_str(Keys) -> string:join([atom_to_list(K) || K <- Keys], ".").

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Not hd(Forms): another parse_transform may already have put a form in front
%% of -module, and there is no promise about the order transforms run in.
-spec module_of(forms()) -> module() | undefined.
module_of(Forms) ->
    case [M || {attribute, _, module, M} <- Forms] of
        [M | _] -> M;
        []      -> undefined
    end.

-spec file_of(forms()) -> file:filename_all().
file_of(Forms) ->
    case [F || {attribute, _, file, {F, _}} <- Forms] of
        [F | _] -> F;
        []      -> "nofile"
    end.

-spec to_list(file:filename_all()) -> string().
to_list(F) when is_binary(F) -> unicode:characters_to_list(F);
to_list(F)                   -> F.

-spec to_binary(file:filename_all()) -> binary().
to_binary(F) when is_binary(F) -> F;
to_binary(F)                   -> unicode:characters_to_binary(F).
