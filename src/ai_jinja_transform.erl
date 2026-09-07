%%%-------------------------------------------------------------------
%%% @doc parse_transform for the three jinja source forms.
%%%
%%% ```
%%% -module(my_views).
%%% -compile({parse_transform, ai_jinja_transform}).
%%%
%%% -jinja_ext(my_filters).                          % (a)
%%% -jinja_template({page, "views/page.j2"}).        % (c)
%%%
%%% greet(Name) ->                                   % (b)
%%%     ai_jinja:inline(~"Hello {{ name }}!", #{name => Name}).
%%% '''
%%%
%%% <ul>
%%%   <li><b>(a) `-jinja_ext'</b> declares and validates a filter/test module.
%%%       It does not assemble one: a parse_transform sees a single module, so
%%%       a declaration here says nothing about how the plugin compiles `.j2'
%%%       files -- that is what `{jinja_opts, [{extensions, [...]}]}' is for.
%%%       The declaration <em>is</em> used for forms (b) and (c) in this same
%%%       module. The attribute is consumed and does not reach the beam.</li>
%%%   <li><b>(b) `ai_jinja:inline/2'</b> with a binary literal is expanded in
%%%       place. Without this transform the very same call still works,
%%%       compiled at run time, with identical output and identical failures --
%%%       both paths refuse the same statements, via
%%%       {@link ai_jinja_compiler:inline_forbidden/1}.</li>
%%%   <li><b>(c) `-jinja_template'</b> compiles a template file into `Name/1'
%%%       and `Name_iolist/1' and exports them. Unlike (a) and (b) this one
%%%       hard-fails without the transform: the functions simply are not there.
%%%       The attribute is deliberately <em>kept</em> in the beam -- see
%%%       inject_all/2.</li>
%%% </ul>
%%%
%%% The three passes -- collect, expand, inject -- and the diagnostics
%%% machinery mirror ai_mustache_transform exactly. Where the two differ it is
%%% because the engines differ, never because this was written independently.
%%%
%%% See designs/12-jinja-toolchain.md section 2.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_transform).

-export([parse_transform/2, format_error/1]).

%% Stand-in for the context expression while the expansion is alpha-renamed.
-define(CTX_VAR, 'JinjaCtx__').

-record(st, {file            :: file:filename_all(),
             module          :: module() | undefined,
             opts            :: [term()],
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
    %% Every pass runs to completion before a result is decided, so one
    %% compilation reports every problem instead of only the first.
    St1 = lists:foldl(fun collect/2, St0, Forms),
    {Forms1, St2} = lists:mapfoldl(fun expand/2, St1#st{file = File}, Forms),
    Forms2 = [F || F <- Forms1, F =/= drop],
    {Forms3, St3} = inject_all(Forms2, St2#st{file = File}),
    result(Forms3, St3).

result(Forms, #st{errors = [], warnings = []}) ->
    Forms;
result(Forms, #st{errors = [], warnings = Ws}) ->
    {warning, Forms, group(Ws)};
result(_Forms, #st{errors = Es, warnings = Ws}) ->
    {error, group(Es), group(Ws)}.

%% erlc wants diagnostics grouped by file; putting two files' problems into one
%% group makes it print the wrong path for half of them.
group(Diags0) ->
    Diags = lists:reverse(Diags0),
    Files = lists:usort([F || {F, _, _} <- Diags]),
    [{F, lists:keysort(1, [{Loc, ?MODULE, R} || {F1, Loc, R} <- Diags, F1 =:= F])}
     || F <- Files].

%%%===================================================================
%%% Pass 1: collect
%%%===================================================================

%% `file' is tracked as the forms are walked rather than read once: after an
%% include there are several -file attributes, and a diagnostic has to name the
%% file the offending form actually came from. That is why this pass folds and
%% the next one mapfolds.
collect({attribute, _, file, {F, _}}, St) ->
    St#st{file = F};
collect({attribute, _, module, M}, St) ->
    St#st{module = M};
collect({attribute, A, jinja_ext, Term}, St) ->
    collect_ext(A, Term, St);
collect({attribute, A, jinja_template, Term}, St) ->
    collect_template(A, Term, St);
collect({function, _, Name, Arity, _}, St) ->
    St#st{defined = [{Name, Arity} | St#st.defined]};
collect(_, St) ->
    St.

%%%-------------------------------------------------------------------
%%% (a) -jinja_ext
%%%-------------------------------------------------------------------

collect_ext(A, Mod, St0) when is_atom(Mod) ->
    St1 = St0#st{ext_mods = St0#st.ext_mods ++ [Mod]},
    case ai_jinja_ext:spec_module(Mod) of
        {error, Reason} ->
            error_at(A, Reason, St1);
        {ok, #{filters := Fs, tests := Ts}} ->
            check_conflicts(A, Mod, Fs, Ts, St1)
    end;
collect_ext(A, Term, St) ->
    error_at(A, {bad_jinja_ext, Term}, St).

%% Shadowing a builtin is an error, not an override. A template that silently
%% gets a different `join' because a dependency registered one is the kind of
%% bug nobody finds.
check_conflicts(A, Mod, Fs, Ts, St) ->
    Clashes = [{filter, N} || N <- Fs, ai_jinja_ext:lookup(filter, N) =/= error]
        ++ [{test, N} || N <- Ts, ai_jinja_ext:lookup(test, N) =/= error],
    lists:foldl(fun({_Kind, N}, Acc) ->
                        error_at(A, {filter_name_conflict, N, Mod}, Acc)
                end, St, Clashes).

%%%-------------------------------------------------------------------
%%% (c) -jinja_template
%%%-------------------------------------------------------------------

collect_template(A, Term, St) ->
    case ai_html_path:template_spec(Term) of
        error ->
            error_at(A, {bad_jinja_template, Term}, St);
        {ok, {Name, Path}} ->
            case lists:keymember(Name, 1, St#st.templates) of
                true  -> error_at(A, {duplicate_template_name, Name}, St);
                false -> St#st{templates = St#st.templates ++ [{Name, Path, A}]}
            end
    end.

%%%===================================================================
%%% Pass 2: expand
%%%===================================================================

expand({attribute, _, file, {F, _}} = Form, St) ->
    {Form, St#st{file = F}};
expand({attribute, _, jinja_ext, _}, St) ->
    %% Consumed. Leaving it behind would pollute module_info(attributes) with
    %% something that means nothing at run time.
    {drop, St};
expand({function, A, Name, Arity, Clauses}, St) ->
    {Clauses1, St1} = expr(Clauses, St),
    {{function, A, Name, Arity, Clauses1}, St1};
expand(Form, St) ->
    {Form, St}.

%% The walk has to reach every expression position -- a missed one silently
%% degrades an inline call to the run-time path, which is the hardest kind of
%% bug to notice. Rather than enumerate the node types (and forget one when OTP
%% adds a construct), the default clause descends through every tuple and list.
%% The clauses above it exist for the positions where blind descent would be
%% wrong: a clause's patterns and guards, and a comprehension generator's
%% pattern, are not expression positions.
expr({call, A, {remote, _, {atom, _, ai_jinja}, {atom, _, inline}},
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
%%% (b) ai_jinja:inline/2
%%%-------------------------------------------------------------------

inline_call(A, Tpl, Ctx, Call, St0) ->
    %% Both arguments are walked first: either may itself contain an inline
    %% call, and the context expression is spliced into the expansion as is.
    {Tpl1, St1} = expr(Tpl, St0),
    {Ctx1, St2} = expr(Ctx, St1),
    case literal_binary(Tpl1) of
        error       -> {rebuild(Call, Tpl1, Ctx1), warn_not_literal(A, St2)};
        {ok, Bin}   -> do_expand(A, Bin, Ctx1, rebuild(Call, Tpl1, Ctx1), St2)
    end.

rebuild({call, A, Remote, _}, Tpl, Ctx) -> {call, A, Remote, [Tpl, Ctx]}.

%% `<<"a">>', `~"a"' and `<<"a", "b">>' are all literal binaries; anything with
%% a variable, a size or a type specifier is not. erl_parse:normalise/1 decides
%% by trying to evaluate the construction, so no case has to be enumerated.
literal_binary({bin, _, _} = Expr) ->
    try erl_parse:normalise(Expr) of
        Bin when is_binary(Bin) -> {ok, Bin};
        _                       -> error
    catch _:_ -> error
    end;
literal_binary(_) ->
    error.

warn_not_literal(A, St) ->
    case lists:member(nowarn_jinja_inline, St#st.opts) of
        true  -> St;
        false -> warn_at(A, inline_not_literal, St)
    end.

do_expand(A, Bin, CtxExpr, Call, St) ->
    Opts = inline_opts(St),
    case ai_jinja_parser:parse(Bin, Opts) of
        {error, {_F, TplLine, Reason}} ->
            {Call, error_at(shift(A, TplLine), Reason, St)};
        {ok, Nodes} ->
            build_inline(A, Nodes, CtxExpr, Call, Opts, St)
    end.

build_inline(A, Nodes, CtxExpr, Call, Opts, St) ->
    N = St#st.counter + 1,
    Placeholder = {var, erl_anno:new(0), ?CTX_VAR},
    case ai_jinja_compiler:compile_inline(Nodes, Placeholder, Opts) of
        {error, {_F, TplLine, Reason}} ->
            {Call, error_at(shift(A, TplLine), Reason, St)};
        {ok, Expr0} ->
            %% Annotations first, substitution second: mapping annotations over
            %% the finished expression would overwrite the line numbers of the
            %% user's own context expression and send its stack frames to the
            %% wrong line.
            Expr1 = erl_parse:map_anno(fun(_) -> A end, Expr0),
            {subst(Expr1, N, CtxExpr), St#st{counter = N}}
    end.

%% The compiler already namespaces its variables away from the user's, but two
%% inline calls in one function would still bind the same names twice -- the
%% second match would test against the first's value instead of binding. Every
%% generated variable therefore also carries the call's serial number. Only
%% names the compiler produced are touched: the context expression is
%% substituted afterwards, so the user's own variables never pass through here.
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
%%
%% A real annotation comes back, not a bare line: erl_anno:anno() is opaque,
%% and handing an integer to something typed for it is exactly the kind of
%% accidental type-check that dialyzer exists to catch.
shift(A, TplLine) ->
    erl_anno:new(erl_anno:line(A) + line_of(TplLine) - 1).

%% The engines report a plain line number in their error tuples.
line_of(L) -> L.

%%%===================================================================
%%% Pass 3: inject
%%%===================================================================

%% -jinja_template is NOT dropped the way -jinja_ext is. Keeping it means
%% Mod:module_info(attributes) still says which templates a module was built
%% from, which is what makes the plugin's staleness fallback work. Removing it
%% here would look like tidying up and would quietly break that.
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

clash(Name, IolistName, St) ->
    case [F || F <- [{Name, 1}, {IolistName, 1}],
               lists:member(F, St#st.defined)] of
        [F | _] -> {clash, F};
        []      -> ok
    end.

iolist_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_iolist").

compile_template(Name, IolistName, Abs, A, {Blocks, Exports, St}) ->
    Opts = template_opts(Abs, St),
    case read_and_compile(Abs, Opts) of
        {error, {File, Line, Reason}} ->
            %% Reported against the .j2 at its own line, not against the .erl:
            %% that is the file the user has to edit.
            {Blocks, Exports,
             add(errors, {to_list(File), line_of(Line), Reason}, St)};
        {ok, Generated, Deps} ->
            St1 = warn_targets(A, Name, Deps, St),
            Block = fragment(Name, IolistName, Abs, Generated, St1),
            {[Block | Blocks], [{IolistName, 1}, {Name, 1} | Exports], St1}
    end.

read_and_compile(Abs, Opts) ->
    case file:read_file(Abs) of
        {error, Posix} ->
            {error, {Abs, 1, {template_unreadable, Posix}}};
        {ok, Body} ->
            case ai_jinja_parser:parse(Body, Opts) of
                {error, E}  -> {error, E};
                {ok, Nodes} ->
                    case ai_jinja_compiler:forms(Nodes, Opts) of
                        {error, E}        -> {error, E};
                        {ok, Forms, Deps} -> {ok, Forms, Deps}
                    end
            end
    end.

%% include, extends and import compile to calls into other generated modules,
%% which only exist if something compiled the views directory. That is the
%% plugin's job; a project may still have done it by hand, so this is a
%% warning and not an error.
warn_targets(_A, _Name, [], St) ->
    St;
warn_targets(A, Name, Deps, St) ->
    case proplists:get_value(views, jinja_opts(St), undefined) of
        undefined -> warn_at(A, {template_target_needs_plugin, Name, Deps}, St);
        _         -> St
    end.

%%%-------------------------------------------------------------------
%%% Turning a generated module into an injectable fragment
%%%-------------------------------------------------------------------

%% ai_jinja_compiler:forms/2 builds a whole module, which is the wrong shape
%% here: what is needed is a handful of functions to graft onto somebody
%% else's. The shell (-module, -export, -jinja_source) is dropped, render/1 and
%% render_iolist/1 take the names the attribute asked for, and every remaining
%% function is renamed to a private form no user can write by hand --
%% `-jinja-page-render_with-'. Two -jinja_template attributes in one module
%% therefore cannot collide over block_title/3.
%%
%% render_scope/2 and partials/0 are dropped rather than renamed: nothing calls
%% them once the export is gone, and an unused private function is a warning.
fragment(Name, IolistName, Abs, Generated, St) ->
    Renames = #{{render, 1}        => Name,
                {render_iolist, 1} => IolistName},
    Dropped = [{render_scope, 2}, {partials, 0}],
    Defined = [{F, A} || {function, _, F, A, _} <- Generated],
    Rename = fun(F, A) ->
                     case {lists:member({F, A}, Defined),
                           maps:get({F, A}, Renames, undefined)} of
                         {false, _}        -> F;
                         {true, undefined} -> private_name(Name, F);
                         {true, New}       -> New
                     end
             end,
    Body = [rename_form(Form, Rename, St#st.module)
            || Form <- Generated, keep_form(Form, Dropped)],
    %% A -file marker makes stack frames and any diagnostics from the generated
    %% code point at the template rather than at a line of the .erl that has
    %% nothing to do with it.
    [{attribute, erl_anno:new(0), file, {to_list(Abs), 1}} | Body].

keep_form({attribute, _, module, _}, _)       -> false;
keep_form({attribute, _, export, _}, _)       -> false;
keep_form({attribute, _, jinja_source, _}, _) -> false;
keep_form({function, _, F, A, _}, Dropped)    -> not lists:member({F, A}, Dropped);
keep_form({attribute, _, spec, {{F, A}, _}}, Dropped) ->
    not lists:member({F, A}, Dropped);
keep_form(_, _)                               -> true.

rename_form({function, A, F, Arity, Clauses}, Rename, Mod) ->
    {function, A, Rename(F, Arity), Arity, rename_calls(Clauses, Rename, Mod)};
rename_form({attribute, A, spec, {{F, Arity}, Types}}, Rename, _Mod) ->
    {attribute, A, spec, {{Rename(F, Arity), Arity}, Types}};
rename_form(Form, Rename, Mod) ->
    rename_calls(Form, Rename, Mod).

%% Local calls and local fun references follow the rename. So does a fun
%% reference qualified with this very module: blocks/0 builds
%% `fun ?MODULE:block_title/3', and ?MODULE is the module being compiled into,
%% so the name has to move with the function. Remote calls to anything else --
%% ai_jinja_rt, another generated module -- must not be touched.
rename_calls({call, A, {atom, AA, F}, Args}, Rename, Mod) ->
    {call, A, {atom, AA, Rename(F, length(Args))},
     rename_calls(Args, Rename, Mod)};
rename_calls({'fun', A, {function, F, Arity}}, Rename, _Mod) ->
    {'fun', A, {function, Rename(F, Arity), Arity}};
rename_calls({'fun', A, {function, {atom, MA, Mod}, {atom, FA, F},
                         {integer, IA, Arity} = I}}, Rename, Mod) ->
    _ = I,
    {'fun', A, {function, {atom, MA, Mod}, {atom, FA, Rename(F, Arity)},
                {integer, IA, Arity}}};
rename_calls(T, Rename, Mod) when is_tuple(T) ->
    list_to_tuple(rename_calls(tuple_to_list(T), Rename, Mod));
rename_calls([H | T], Rename, Mod) ->
    [rename_calls(H, Rename, Mod) | rename_calls(T, Rename, Mod)];
rename_calls(Other, _Rename, _Mod) ->
    Other.

%% Hyphens make this unwritable as an ordinary atom, so it cannot collide with
%% anything the user defined.
private_name(Template, Fun) ->
    list_to_atom("-jinja-" ++ atom_to_list(Template) ++ "-"
                 ++ atom_to_list(Fun) ++ "-").

%% Erlang wants every -export before the first function definition, so the new
%% one goes just ahead of it; the generated functions go at the very end,
%% before {eof, _}, followed by a -file marker restoring the .erl.
splice(Forms, Exports, Blocks, St) ->
    A = erl_anno:new(0),
    Export = {attribute, A, export, Exports},
    Restore = {attribute, A, file, {to_list(St#st.file), 1}},
    %% The eof split has to come first: a module with no function definitions
    %% at all would otherwise put the export after {eof, _}, where erl_lint
    %% reports it as "attribute export after function definitions".
    {Main, Eof}  = lists:splitwith(fun(F) -> element(1, F) =/= eof end, Forms),
    {Head, Body} = lists:splitwith(fun(F) -> element(1, F) =/= function end, Main),
    Head ++ [Export] ++ Body ++ lists:append(Blocks) ++ [Restore] ++ Eof.

%%%===================================================================
%%% Options
%%%===================================================================

%% rebar3 passes {jinja_opts, [...]} through erl_opts; a hand-written erlc line
%% can pass a map just as well.
jinja_opts(#st{opts = Opts}) ->
    lists:foldl(fun({jinja_opts, L}, Acc) when is_list(L) -> Acc ++ L;
                   ({jinja_opts, M}, Acc) when is_map(M)  -> Acc ++ maps:to_list(M);
                   (_, Acc)                               -> Acc
                end, [], Opts).

%% Extensions declared here plus extensions the build assembled. Same-module
%% forms (b) and (c) are the one place a -jinja_ext genuinely takes effect.
ext_modules(St) ->
    lists:usort(St#st.ext_mods
                ++ proplists:get_value(extensions, jinja_opts(St), [])).

base_opts(St) ->
    Base = maps:from_list(jinja_opts(St)),
    Base#{module     => St#st.module,
          extensions => ext_modules(St)}.

inline_opts(St) ->
    (base_opts(St))#{source => to_binary(St#st.file)}.

template_opts(Abs, St) ->
    (base_opts(St))#{source => to_binary(Abs)}.

%% Module directory first, then the configured views root, then every -I, then
%% the cwd as a last resort. The plugin resolves the same path the same way --
%% if it did not, it would touch a different file from the one read here and
%% the staleness fallback would do nothing.
resolve(Path, St) ->
    Dirs = [filename:dirname(St#st.file)]
        ++ views_dirs(St)
        ++ [D || {i, D} <- St#st.opts]
        ++ ["."],
    case ai_html_path:resolve(Path, Dirs) of
        {ok, Abs}                   -> {ok, Abs};
        {error, {not_found, Tried}} -> {error, Tried}
    end.

views_dirs(St) ->
    case proplists:get_value(views, jinja_opts(St), undefined) of
        undefined -> [];
        V         -> [to_list(V)]
    end.

%%%===================================================================
%%% Diagnostics
%%%===================================================================

error_at(A, Reason, St) -> add(errors, {St#st.file, loc(A), Reason}, St).
warn_at(A, Reason, St)  -> add(warnings, {St#st.file, loc(A), Reason}, St).

%% erl_anno:line/1 accepts a bare integer annotation as well as a real one.
loc(A) -> erl_anno:line(A).

add(errors, D, St)   -> St#st{errors = [D | St#st.errors]};
add(warnings, D, St) -> St#st{warnings = [D | St#st.warnings]}.

file_of(Forms) ->
    case [F || {attribute, _, file, {F, _}} <- Forms] of
        [F | _] -> F;
        []      -> "nofile"
    end.

%% Found by searching, not by taking hd/1: another parse_transform may already
%% have inserted forms ahead of the -module attribute.
module_of(Forms) ->
    case [M || {attribute, _, module, M} <- Forms] of
        [M | _] -> M;
        []      -> undefined
    end.

to_list(B) when is_binary(B) -> unicode:characters_to_list(B);
to_list(L)                   -> L.

to_binary(L) -> unicode:characters_to_binary(L).

-spec format_error(term()) -> string().
format_error({bad_jinja_ext, Term}) ->
    f("-jinja_ext takes a module name: -jinja_ext(my_filters). Got ~p", [Term]);
format_error({bad_jinja_template, Term}) ->
    f("-jinja_template takes {Name, \"path/to/template.j2\"} or just the "
      "path. Got ~p", [Term]);
format_error({duplicate_template_name, Name}) ->
    f("two -jinja_template attributes both want to define ~p/1", [Name]);
format_error({template_name_clash, {F, A}}) ->
    f("-jinja_template would define ~p/~p, which this module already defines "
      "by hand", [F, A]);
format_error({template_not_found, Path, Tried}) ->
    f("-jinja_template: ~ts not found. Looked in: ~ts",
      [Path, string:join([to_list(D) || D <- Tried], ", ")]);
format_error({template_unreadable, Posix}) ->
    f("-jinja_template: the template could not be read (~p)", [Posix]);
format_error({template_target_needs_plugin, Name, Deps}) ->
    f("-jinja_template ~p uses {% include %}, {% extends %} or {% import %}, "
      "which compile to calls into ~p. Those modules come from the rebar3 "
      "plugin; set {jinja_opts, [{views, \"views\"}]} and run it, or the "
      "calls will not resolve at run time", [Name, Deps]);
format_error(inline_not_literal) ->
    "ai_jinja:inline/2 was left as a run-time call because its first argument "
    "is not a binary literal. The output is the same; only the cost differs. "
    "Silence this with the nowarn_jinja_inline compile option";
format_error({filter_name_conflict, Name, Mod}) ->
    ai_jinja_ext:format_error({filter_name_conflict, Name, Mod});
format_error({target_in_inline_template, Kind}) ->
    f("{% ~p %} needs a views directory to resolve against and an inline "
      "template has none. Put the template in a file and use "
      "-jinja_template instead", [Kind]);
format_error({mutual_macro_in_inline, Names}) ->
    f("the macros ~p call each other, and an inline template compiles macros "
      "to anonymous funs, which cannot. Put them in a file template", [Names]);
format_error(Other) ->
    f("~p", [Other]).

f(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).
