%%%-------------------------------------------------------------------
%%% @doc AST -> Erlang abstract forms.
%%%
%%% This is the only implementation of the AST-to-forms translation; the
%%% rebar3 plugin and the parse_transform both go through it and neither may
%%% bypass it (architecture invariant 2). forms/2 wraps a whole module,
%%% compile_inline/3 (T14) wraps a single expression, and both share the
%%% node compilation below.
%%%
%%% Static text becomes literals so it lands in the module's literal pool and
%%% is shared across processes by reference. That is the whole point of the
%%% refactor: ets:lookup/2 deep-copies its result on every call, a literal
%%% costs nothing.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_compiler).

-include("ai_mustache.hrl").

-export([forms/2, compile_inline/3, source_hash/2, normalize_opts/1]).
-export([body_exprs/3, new_state/1, aux_forms/1, state_counter/1, set_counter/2]).

%% The form constructors live in ai_html_forms, shared with the other engines
%% (designs/08-jinja-architecture.md section 1.1). Imported rather than
%% qualified so that the code generation below reads exactly as it always did.
-import(ai_html_forms,
        [a/2, v/2, n/2, cl/4, fn/4, spec/4, t/2, param/3,
         bin/2, empty_bin/1, concat_bin/3, mklist/2,
         rem_call/4, loc_call/3, g/3, anno/1, remotes/2]).

-define(SV, 'S').        % context stack variable in generated code
-define(IV, 'I').        % indent variable in generated code

-record(cs, {opts     :: map(),
             file     :: binary(),
             line_map :: boolean(),
             counter  :: non_neg_integer(),
             aux      :: [erl_parse:abstract_form()],
             exts     :: #{char() => module()},
             bol      = true  :: boolean(),
             ext_remotes = [] :: [module()],
             inline   = false :: boolean()}).

-opaque state() :: #cs{}.
-export_type([state/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Compile a template AST into a complete module.
-spec forms([ai_mustache_node()], map()) ->
          {ok, [erl_parse:abstract_form()], [module()]} | ai_mustache_error().
forms(Nodes0, Opts) ->
    case ai_mustache_ast:postprocess(Nodes0, Opts) of
        {error, _} = E -> E;
        {ok, Nodes, Deps} ->
            St0 = new_state(Opts),
            try body_exprs(Nodes, true, St0) of
                {Exprs, St1} -> module_forms(Exprs, Deps, St1)
            catch
                throw:{ai_mustache_error, Err} -> Err
            end
    end.

%% @doc Compile a template into a single expression for inline expansion.
%%
%% Used by the parse_transform when it rewrites ai_mustache:inline/2 with a
%% literal template. The result evaluates to a binary and shares the whole
%% node compilation below with forms/2, so an inline template and a file
%% template can never drift apart.
%%
%% Section bodies become anonymous funs rather than module-level functions:
%% the expansion has to sit inside somebody else's module and cannot add
%% functions to it. Partials are rejected outright -- an inline template has
%% no views directory to resolve them against.
-spec compile_inline([ai_mustache_node()], erl_parse:abstract_expr(), map()) ->
          {ok, erl_parse:abstract_expr()} | ai_mustache_error().
compile_inline(Nodes0, CtxExpr, Opts) ->
    case ai_mustache_ast:merge_text(ai_mustache_ast:drop_empty(Nodes0)) of
        Nodes ->
            St0 = (new_state(Opts))#cs{inline = true},
            try body_exprs(Nodes, true, St0) of
                {Exprs, St1} -> {ok, inline_expr(Exprs, CtxExpr, St1)}
            catch
                throw:{ai_mustache_error, Err} -> Err
            end
    end.

%% erlang:iolist_to_binary(begin S = [Ctx], I = <<>>, [I | Exprs] end)
-spec inline_expr([erl_parse:abstract_expr()], erl_parse:abstract_expr(), state()) ->
          erl_parse:abstract_expr().
inline_expr(Exprs, CtxExpr, St) ->
    L = anno(0),
    Funs = aux_forms(St),
    Block = {block, L,
             Funs ++ [{match, L, v(L, ?SV), mklist(L, [CtxExpr])},
                      {match, L, v(L, ?IV), empty_bin(L)},
                      mklist(L, Exprs)]},
    rem_call(L, erlang, iolist_to_binary, [Block]).

-spec new_state(map()) -> state().
new_state(Opts0) ->
    Opts = ai_html_text:opts(Opts0),
    #cs{opts     = Opts,
        file     = ai_html_text:source(Opts),
        line_map = maps:get(line_map, Opts, true),
        counter  = 0,
        aux      = [],
        ext_remotes = [],
        exts     = ext_table(Opts)}.

-spec aux_forms(state()) -> [erl_parse:abstract_form()].
aux_forms(#cs{aux = Aux}) -> lists:reverse(Aux).

-spec state_counter(state()) -> non_neg_integer().
state_counter(#cs{counter = C}) -> C.

-spec set_counter(state(), non_neg_integer()) -> state().
set_counter(St, C) -> St#cs{counter = C}.

%%%===================================================================
%%% Module assembly
%%%===================================================================

-spec module_forms([erl_parse:abstract_expr()], [module()], state()) ->
          {ok, [erl_parse:abstract_form()], [module()]} | ai_mustache_error().
module_forms(Exprs, Deps, St) ->
    Mod = maps:get(module, St#cs.opts),
    L = anno(0),
    Body = mklist(L, Exprs),
    Static = static_text(Exprs),
    Forms =
        [{attribute, L, module, Mod},
         {attribute, L, mustache_source, source_attr(St)},
         {attribute, L, export, [{render, 1}, {render_iolist, 1},
                                 {render_stack, 1}, {render_stack, 2},
                                 {partials, 0}]},
         spec(L, render, [t(L, term)], t(L, binary)),
         fn(L, render, [ctx_param(L, Static)], [render_expr(L, Static)]),
         spec(L, render_iolist, [t(L, term)], t(L, iolist)),
         fn(L, render_iolist, [ctx_param(L, Static)], [render_iolist_expr(L, Static)]),
         spec(L, render_stack, [t(L, list)], t(L, iolist)),
         fn(L, render_stack, [v(L, ?SV)],
            [loc_call(L, render_stack, [v(L, ?SV), empty_bin(L)])]),
         spec(L, partials, [], {type, L, list, [t(L, module)]}),
         fn(L, partials, [], [mklist(L, [a(L, D) || D <- Deps])]),
         spec(L, render_stack, [t(L, list), t(L, binary)], t(L, iolist)),
         fn(L, render_stack,
            [param(L, ?SV, Body), param(L, ?IV, Body)], [Body])]
        ++ aux_forms(St),
    %% No {eof, _} terminator: compile:forms/2 does not need one and it is not
    %% a member of erl_parse:abstract_form(), so including it would make the
    %% return type unsatisfiable.
    Allowed = [erlang, ai_mustache_rt] ++ Deps ++ St#cs.ext_remotes,
    case check_remotes(Forms, Allowed, St) of
        ok             -> {ok, Forms, Deps};
        {error, _} = E -> E
    end.

%% Constant folding: a template with no dynamic tags is one literal.
%%
%% Only render/1 and render_iolist/1 can be folded. render_stack/2 must keep
%% the indent-aware body, because the same module may be used as a partial and
%% then has to indent its own lines. The two entry points always pass an empty
%% indent, so for them the fold is exact.
-spec static_text([erl_parse:abstract_expr()]) -> {yes, binary()} | no.
static_text(Exprs) -> ai_html_forms:static_text(Exprs, ?IV).

%% A folded template never looks at its context, so the parameter has to be
%% underscored or the generated module would not survive warnings_as_errors.
-spec ctx_param(erl_anno:anno(), {yes, binary()} | no) -> erl_parse:abstract_expr().
ctx_param(L, {yes, _}) -> v(L, '_Ctx');
ctx_param(L, no)       -> v(L, 'Ctx').

-spec render_expr(erl_anno:anno(), {yes, binary()} | no) -> erl_parse:abstract_expr().
render_expr(L, {yes, Bin}) ->
    bin(L, Bin);
render_expr(L, no) ->
    rem_call(L, erlang, iolist_to_binary,
             [loc_call(L, render_stack, [mklist(L, [v(L, 'Ctx')]), empty_bin(L)])]).

-spec render_iolist_expr(erl_anno:anno(), {yes, binary()} | no) ->
          erl_parse:abstract_expr().
render_iolist_expr(L, {yes, Bin}) ->
    mklist(L, [bin(L, Bin)]);
render_iolist_expr(L, no) ->
    loc_call(L, render_stack, [mklist(L, [v(L, 'Ctx')]), empty_bin(L)]).

-spec source_attr(state()) -> map().
source_attr(#cs{opts = Opts}) ->
    Attr = #{path  => path_of(Opts),
             stamp => maps:get(stamp, Opts, <<>>),
             mtime => maps:get(mtime, Opts, 0),
             vsn   => ?AI_MUSTACHE_VSN,
             opts  => normalize_opts(Opts)},
    %% Only recorded when it is not the default, so a file template's
    %% generated .erl is unchanged. See ai_mustache_source() in the header.
    case maps:get(origin, Opts, file) of
        file   -> Attr;
        Origin -> Attr#{origin => Origin}
    end.

%% Generated modules may only call ai_mustache_rt, other generated modules and
%% erlang BIFs (architecture invariant 3). A violation is a compiler bug, so it
%% is checked rather than assumed.
%%
%% Code produced by a user extension is exempt: an extension that compiles
%% {{@ key}} into a call to its own runtime module is doing exactly what the
%% design intends, and the compiler has no business vetting it. Those modules
%% are collected as the extensions emit them.
-spec check_remotes([erl_parse:abstract_form()], [module()], state()) ->
          ok | ai_mustache_error().
check_remotes(Forms, Allowed, St) ->
    Called = remotes(Forms, []),
    case [M || M <- Called, not lists:member(M, Allowed)] of
        []  -> ok;
        Bad -> {error, {St#cs.file, 1, {unexpected_remote_calls, lists:usort(Bad)}}}
    end.

%%%===================================================================
%%% Node compilation
%%%===================================================================

%% @doc Compile a node list into the expressions of one iolist.
%%
%% The indent variable is emitted BEFORE the content of each line, never after
%% a newline. Emitting it after a newline looks equivalent but leaves a
%% trailing indent whenever a partial's output ends with one, which then shows
%% up as stray whitespace on the caller's next line. Prepending instead makes
%% that impossible: an indent is only ever emitted when there is content on
%% the line to indent.
%%
%% Whether we are at the start of a line is tracked at compile time in
%% #cs.bol. For a conditional block the state afterwards is the state before
%% it AND the state at the end of its body: if the block does not run nothing
%% changed, and if it does the body decides, so only agreement is safe to
%% assume.
-spec body_exprs([ai_mustache_node()], boolean(), state()) ->
          {[erl_parse:abstract_expr()], state()}.
body_exprs(Nodes, Bol, St) ->
    body_loop(Nodes, St#cs{bol = Bol}, []).

-spec body_loop([ai_mustache_node()], state(), [erl_parse:abstract_expr()]) ->
          {[erl_parse:abstract_expr()], state()}.
body_loop([], St, Acc) ->
    {lists:reverse(Acc), St};
body_loop([Node | Rest], St, Acc) ->
    {Exprs, St1} = node_exprs(Node, St),
    body_loop(Rest, St1, lists:reverse(Exprs) ++ Acc).

-spec node_exprs(ai_mustache_node(), state()) ->
          {[erl_parse:abstract_expr()], state()}.
node_exprs({text, Loc, Bin}, St) ->
    {Exprs, Bol} = text_exprs(Bin, St#cs.bol, line(Loc, St)),
    {Exprs, St#cs{bol = Bol}};

node_exprs({var, Loc, Keys, escape}, St) ->
    L = line(Loc, St),
    {lead(L, St) ++ [rt(L, escape, [rt(L, lookup, [keys(L, Keys), v(L, ?SV)])])],
     St#cs{bol = false}};
node_exprs({var, Loc, Keys, raw}, St) ->
    L = line(Loc, St),
    {lead(L, St) ++ [rt(L, to_binary, [rt(L, lookup, [keys(L, Keys), v(L, ?SV)])])],
     St#cs{bol = false}};

node_exprs({lambda, Loc, Keys}, St) ->
    L = line(Loc, St),
    {lead(L, St) ++ [rt(L, lambda, [rt(L, lookup, [keys(L, Keys), v(L, ?SV)]),
                                    hd_stack(L)])],
     St#cs{bol = false}};

node_exprs({partial, Loc, _Mod, _Indent}, #cs{inline = true} = St) ->
    throw({ai_mustache_error,
           {error, {St#cs.file, line(Loc, St), partial_in_inline_template}}});
node_exprs({partial, Loc, Mod, Indent}, St) ->
    L = line(Loc, St),
    %% The callee applies the indent to its own lines, so nothing is prepended
    %% here. A standalone partial owned its whole line, so its output ends at
    %% a line start; an inline one can end anywhere.
    IndentExpr = case Indent of
                     <<>> -> v(L, ?IV);
                     _    -> concat_bin(L, ?IV, Indent)
                 end,
    {[{call, L, {remote, L, a(L, Mod), a(L, render_stack)},
       [v(L, ?SV), IndentExpr]}],
     St#cs{bol = Indent =/= <<>>}};

node_exprs({section, Loc, Keys, Body}, St) ->
    aux(section, Loc, Keys, Body, St,
        fun(L, BodyRef) -> section_clauses(L, BodyRef) end);

node_exprs({inverted, Loc, Keys, Body}, St) ->
    aux(inverted, Loc, Keys, Body, St,
        fun(L, BodyRef) -> inverted_clauses(L, BodyRef) end);

node_exprs({has, Loc, Keys, Body, Positive}, St) ->
    aux(has, Loc, Keys, Body, St,
        fun(L, BodyRef) -> has_clauses(L, BodyRef, Positive) end);

node_exprs({ext, Loc, Marker, Keys, Body}, St) ->
    L = line(Loc, St),
    {BodyExprs, St1} = body_exprs(Body, St#cs.bol, St),
    case maps:get(Marker, St#cs.exts, undefined) of
        undefined ->
            throw({ai_mustache_error,
                   {error, {St#cs.file, L, {unknown_marker, Marker}}}});
        Mod ->
            %% The extension needs to know which variables hold the context
            %% stack and the indent, and where it is in the template; without
            %% them it would have to hard-code {var, 0, 'S'} and lose all
            %% diagnostics.
            TagOpts = (St#cs.opts)#{loc        => Loc,
                                    anno       => L,
                                    stack_var  => ?SV,
                                    indent_var => ?IV},
            try Mod:compile_tag(Marker, Keys, BodyExprs, TagOpts) of
                Expr ->
                    %% Whatever module the extension chose to call is its
                    %% business, not the compiler's, so record it as allowed
                    %% rather than flagging it in check_remotes/3.
                    {lead(L, St) ++ [Expr],
                     St1#cs{bol = false,
                            ext_remotes = remotes(Expr, St1#cs.ext_remotes)}}
            catch
                C:R -> throw({ai_mustache_error,
                              {error, {St#cs.file, L,
                                       {ext_crashed, Mod, Marker, {C, R}}}}})
            end
    end.

%% The indent to place before content that starts a line.
-spec lead(erl_anno:anno(), state()) -> [erl_parse:abstract_expr()].
lead(L, #cs{bol = true})  -> [v(L, ?IV)];
lead(_L, #cs{bol = false}) -> [].

%%%===================================================================
%%% Auxiliary function generation
%%%===================================================================

%% Emits `<kind>_N/2' plus `<kind>_N_body/2' and returns the call site. The
%% counter is global and monotonic so the same template always produces the
%% same names, which incremental compilation depends on.
-spec aux(atom(), ai_mustache_loc(), ai_mustache_keys(), [ai_mustache_node()],
          state(), fun((erl_anno:anno(), ref()) -> [tuple()])) ->
          {[erl_parse:abstract_expr()], state()}.
aux(Kind, Loc, Keys, Body, St0, ClausesFun) ->
    L = line(Loc, St0),
    N = St0#cs.counter + 1,
    Prefix = case Kind of
                 section  -> "sec_";
                 inverted -> "inv_";
                 has      -> "has_"
             end,
    Name     = Prefix ++ integer_to_list(N),
    BodyName = Name ++ "_body",
    In = St0#cs.bol,
    {BodyExprs, St1} = body_exprs(Body, In, St0#cs{counter = N}),
    BodyList = mklist(L, BodyExprs),
    BodyRef  = ref(BodyName, St1),
    MainRef  = ref(Name, St1),
    BodyDef  = define(L, BodyRef,
                      [param(L, ?SV, BodyList), param(L, ?IV, BodyList)], [BodyList]),
    MainDef  = define(L, MainRef, [v(L, ?SV), v(L, ?IV)],
                      [{'case', L, rt(L, lookup, [keys(L, Keys), v(L, ?SV)]),
                        ClausesFun(L, BodyRef)}]),
    %% Order matters in inline mode, where these become let-bindings in a
    %% block: the body must be bound before the dispatcher that closes over it,
    %% and both after any nested aux they contain.
    St2 = St1#cs{aux = [MainDef, BodyDef | St1#cs.aux],
                 bol = In andalso St1#cs.bol},
    {[bcall(L, MainRef, [v(L, ?SV), v(L, ?IV)])], St2}.

%% A generated helper is a module-level function when compiling a whole module
%% and a bound anonymous fun when expanding inline, since the expansion has to
%% live inside a function of somebody else's module.
-type ref() :: {local, atom()} | {bound, atom()}.

-spec ref(string(), state()) -> ref().
ref(Name, #cs{inline = false}) -> {local, list_to_atom(Name)};
ref(Name, #cs{inline = true})  -> {bound, list_to_atom(camel(Name) ++ "__")}.

-spec camel(string()) -> string().
camel([C | Rest]) -> [string:to_upper(C) | Rest].

define(L, {local, Name}, Params, Body) ->
    fn(L, Name, Params, Body);
define(L, {bound, Var}, Params, Body) ->
    {match, L, v(L, Var), {'fun', L, {clauses, [cl(L, Params, [], Body)]}}}.

bcall(L, {local, Name}, Args) -> {call, L, a(L, Name), Args};
bcall(L, {bound, Var}, Args)  -> {call, L, v(L, Var), Args}.

bfun(L, {local, Name})        -> {'fun', L, {function, Name, 2}};
bfun(L, {bound, Var})         -> v(L, Var).

%% The seven clauses are fully expanded at compile time: no runtime lookup, no
%% apply. Order matters -- [] must precede is_list.
section_clauses(L, B) ->
    Body = fun(Stack) -> [bcall(L, B, [Stack, v(L, ?IV)])] end,
    [cl(L, [{nil, L}], [], [{nil, L}]),
     cl(L, [v(L, 'L__')], [[g(L, is_list, [v(L, 'L__')])]],
        [{lc, L, bcall(L, B, [{cons, L, v(L, 'E__'), v(L, ?SV)}, v(L, ?IV)]),
          [{generate, L, v(L, 'E__'), v(L, 'L__')}]}]),
     cl(L, [v(L, 'M__')], [[g(L, is_map, [v(L, 'M__')])]],
        Body({cons, L, v(L, 'M__'), v(L, ?SV)})),
     cl(L, [a(L, true)], [], Body(v(L, ?SV))),
     cl(L, [v(L, 'F__')], [[g(L, is_function, [v(L, 'F__'), n(L, 2)])]],
        [{call, L, v(L, 'F__'),
          [rem_call(L, erlang, iolist_to_binary,
                    [bcall(L, B, [v(L, ?SV), v(L, ?IV)])]),
           hd_stack(L)]}]),
     cl(L, [v(L, 'F__')], [[g(L, is_function, [v(L, 'F__'), n(L, 1)])]],
        [rt(L, section, [{call, L, v(L, 'F__'), [hd_stack(L)]},
                         bfun(L, B), v(L, ?SV), v(L, ?IV)])]),
     cl(L, [v(L, 'V__')], [],
        [rt(L, section, [v(L, 'V__'), bfun(L, B),
                         v(L, ?SV), v(L, ?IV)])])].

%% Inverted sections run on falsy and never push a scope. The subject of the
%% outer case is the raw lookup result, so truthiness is applied here rather
%% than in aux/6 -- has_clauses/3 does the same.
inverted_clauses(L, B) ->
    [cl(L, [v(L, 'X__')], [],
        [{'case', L, rt(L, truthy, [v(L, 'X__')]),
          [cl(L, [a(L, false)], [], [bcall(L, B, [v(L, ?SV), v(L, ?IV)])]),
           cl(L, [v(L, '_T__')], [], [{nil, L}])]}])].

%% {{+x}} is a conditional, not a scope: it neither pushes nor iterates.
%% That is exactly what separates it from {{#x}}.
has_clauses(L, B, Positive) ->
    Resolved = {'case', L, v(L, 'X__'),
                [cl(L, [v(L, 'F__')], [[g(L, is_function, [v(L, 'F__'), n(L, 1)])]],
                    [{call, L, v(L, 'F__'), [hd_stack(L)]}]),
                 cl(L, [v(L, 'Y__')], [], [v(L, 'Y__')])]},
    [cl(L, [v(L, 'X__')], [],
        [{'case', L, rt(L, truthy, [Resolved]),
          [cl(L, [a(L, Positive)], [], [bcall(L, B, [v(L, ?SV), v(L, ?IV)])]),
           cl(L, [v(L, '_T__')], [], [{nil, L}])]}])].

%%%===================================================================
%%% Text with indentation
%%%===================================================================

%% Indentation applies to the partial's own static text, never to interpolated
%% values -- see the spec's "Standalone Indentation" case, where a value
%% containing a newline is not re-indented. So the indent is woven into the
%% literals at compile time rather than applied to the rendered output.
-spec text_exprs(binary(), boolean(), erl_anno:anno()) ->
          {[erl_parse:abstract_expr()], boolean()}.
text_exprs(<<>>, Bol, _L) ->
    {[], Bol};
text_exprs(Bin, Bol, L) ->
    Parts = binary:split(Bin, <<"\n">>, [global]),
    {Lines, [Tail]} = lists:split(length(Parts) - 1, Parts),
    {Acc, _} = lists:foldl(
                 fun(Seg, {A, B}) ->
                         Lit = bin(L, <<Seg/binary, "\n">>),
                         {[Lit | pre(L, B) ++ A], true}
                 end, {[], Bol}, Lines),
    case Tail of
        <<>> ->
            %% The text ended on a newline, so the next thing to be emitted
            %% starts a line -- and nothing is emitted for the empty remainder,
            %% which is what keeps a trailing indent from ever appearing.
            {lists:reverse(Acc), true};
        _ ->
            AtBol = Lines =/= [] orelse Bol,
            {lists:reverse([bin(L, Tail) | pre(L, AtBol) ++ Acc]), false}
    end.

-spec pre(erl_anno:anno(), boolean()) -> [erl_parse:abstract_expr()].
pre(L, true)  -> [v(L, ?IV)];
pre(_L, false) -> [].

%%%===================================================================
%%% Stamp
%%%===================================================================

%% @doc The combined build stamp: template content, normalised options and the
%% generated-code version.
%%
%% This is the ONLY place the stamp is computed. If the plugin and
%% ai_mustache_dev each rolled their own the two would drift, and dev would
%% then consider every module permanently stale while the plugin considered it
%% permanently fresh. erlang:md5/1 is a BIF, so this needs no crypto app.
-spec source_hash(binary(), map() | [{atom(), term()}]) -> binary().
source_hash(Body, Opts) ->
    erlang:md5(term_to_binary({Body, normalize_opts(Opts), ?AI_MUSTACHE_VSN},
                              [deterministic])).

%% Only the options that can change the generated code, as a sorted list so
%% the result does not depend on map iteration order.
%%
%% Values go through ai_html_text:opts/1 first, so `views' as a string and
%% `views' as a binary produce the same stamp. Without that, whether the stamp
%% matched would depend on how the caller happened to spell a path, and the
%% plugin and ai_mustache_dev would disagree about staleness the moment one of
%% them passed a string.
%%
%% `suffix' is excluded on purpose: it only feeds the partial-existence check,
%% never the output, so including it would rebuild every template for nothing.
%% `views_abs' is excluded for a stronger reason -- it holds an absolute path,
%% which would stamp the developer's home directory into -mustache_source and
%% make the build non-reproducible across machines.
-spec normalize_opts(map() | [{atom(), term()}]) -> [{atom(), term()}].
normalize_opts(Opts0) ->
    Opts = ai_html_text:opts(as_map(Opts0)),
    lists:sort(
      [{K, maps:get(K, Opts)} || K <- [prefix, views, extensions, ext_opts,
                                       line_map],
                                 maps:is_key(K, Opts)]).

%%%===================================================================
%%% Form constructors
%%%===================================================================

%% Mustache-specific shorthands over the shared constructors.
keys(L, Keys)  -> ai_html_forms:atoms(L, Keys).
rt(L, F, Args) -> rem_call(L, ai_mustache_rt, F, Args).
hd_stack(L)    -> ai_html_forms:hd_call(L, v(L, ?SV)).

%%%===================================================================
%%% Helpers
%%%===================================================================

line(_Loc, #cs{line_map = false}) -> anno(0);
line({L, _C}, _St)                -> anno(L).

ext_table(Opts) ->
    lists:foldl(
      fun(Mod, Acc) ->
              _ = code:ensure_loaded(Mod),
              case erlang:function_exported(Mod, markers, 0) of
                  true  -> lists:foldl(fun(C, A) -> A#{C => Mod} end, Acc,
                                       Mod:markers());
                  false -> Acc
              end
      end, #{}, maps:get(extensions, Opts, [])).

path_of(Opts) -> ai_html_text:source(Opts).

%% The self-description records the options as the sorted list
%% normalize_opts/1 produces, so reading them back finds a list.
as_map(L) when is_list(L) -> maps:from_list(L);
as_map(M) when is_map(M)  -> M.
