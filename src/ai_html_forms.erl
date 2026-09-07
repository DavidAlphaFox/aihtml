%%%-------------------------------------------------------------------
%%% @doc Erlang abstract forms constructors, shared by every aihtml engine.
%%%
%%% Nothing in this module knows what a template is. It is the half of code
%%% generation that only speaks Erlang (designs/08-jinja-architecture.md
%%% section 1), and it may not reference any ai_mustache_* or ai_jinja_*
%%% module -- architecture invariant 10, checkable with `rebar3 xref'.
%%%
%%% Two details here have been got wrong before and are the main reason this
%%% module exists rather than being copied per engine; both are documented on
%%% bin/2.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_html_forms).

-export([a/2, v/2, n/2, f/2, cl/4, fn/4, spec/4, t/2]).
-export([bin/2, empty_bin/1, concat_bin/3, mklist/2, atoms/2, abstract/2]).
-export([rem_call/4, loc_call/3, g/3, hd_call/2]).
-export([anno/1, param/3, uses_var/2, remotes/2]).
-export([literal_of/1, static_text/2]).

-type anno()  :: erl_anno:anno().
-type expr()  :: erl_parse:abstract_expr().
-type form()  :: erl_parse:abstract_form().

-export_type([anno/0, expr/0, form/0]).

%%%===================================================================
%%% Atoms, variables, numbers
%%%===================================================================

-spec a(anno(), atom()) -> expr().
a(L, A) -> {atom, L, A}.

-spec v(anno(), atom()) -> expr().
v(L, V) -> {var, L, V}.

-spec n(anno(), integer()) -> expr().
n(L, N) -> {integer, L, N}.

-spec f(anno(), float()) -> expr().
f(L, F) -> {float, L, F}.

-spec cl(anno(), [expr()], [[expr()]], [expr()]) -> tuple().
cl(L, P, G, B) -> {clause, L, P, G, B}.

%%%===================================================================
%%% Binaries
%%%===================================================================

%% @doc Static text as a binary literal.
%%
%% Two things have to be right here, and both were got wrong first time round.
%%
%% A {string, _, Chars} element must carry the DEFAULT type, not [binary]:
%% [binary] means <<"hi"/binary>>, i.e. treat the list as a binary, which is a
%% runtime badarg. With the default type each character becomes an 8-bit
%% integer, which is what <<"hi">> means.
%%
%% And non-ASCII text must go out as characters with the utf8 type, not as raw
%% bytes. A plugin writes these forms to an .erl file that erlc then reads
%% back as UTF-8 source; a byte-per-character literal would have its
%% multi-byte sequences folded into single codepoints on the way in and then
%% truncated to 8 bits, silently corrupting every non-ASCII template. Emitting
%% <<"中文"/utf8>> round-trips exactly and stays readable.
-spec bin(anno(), binary()) -> expr().
bin(L, <<>>) ->
    {bin, L, []};
bin(L, B) ->
    case unicode:characters_to_list(B, utf8) of
        Chars when is_list(Chars) ->
            case lists:all(fun(C) -> C < 128 end, Chars) of
                true  -> {bin, L, [{bin_element, L, {string, L, Chars},
                                    default, default}]};
                false -> {bin, L, [{bin_element, L, {string, L, Chars},
                                    default, [utf8]}]}
            end;
        _NotUtf8 ->
            %% Not valid UTF-8; fall back to one element per byte so the bytes
            %% survive verbatim whatever the reader assumes about encoding.
            {bin, L, [{bin_element, L, {integer, L, Byte}, default, default}
                      || <<Byte>> <= B]}
    end.

-spec empty_bin(anno()) -> expr().
empty_bin(L) -> {bin, L, []}.

%% @doc `<<Var/binary, "suffix">>'.
-spec concat_bin(anno(), atom(), binary()) -> expr().
concat_bin(L, Var, Suffix) ->
    {bin, L, Elems} = bin(L, Suffix),
    {bin, L, [{bin_element, L, v(L, Var), default, [binary]} | Elems]}.

%%%===================================================================
%%% Lists and literals
%%%===================================================================

-spec mklist(anno(), [expr()]) -> expr().
mklist(L, [])      -> {nil, L};
mklist(L, [H | T]) -> {cons, L, H, mklist(L, T)}.

%% @doc A literal list of atoms.
%%
%% Cheaper and clearer than routing through erl_parse:abstract/2, which wants
%% a plain integer line rather than an erl_anno:anno().
-spec atoms(anno(), [atom()]) -> expr().
atoms(L, As) -> mklist(L, [a(L, A) || A <- As]).

%% @doc Any term as a literal, with a proper anno.
%%
%% erl_parse:abstract/2 takes {line, Line}; it does not accept an anno, and
%% the annos it produces are bare integers. Re-annotating afterwards keeps
%% dialyzer happy about erl_anno:anno() being opaque.
-spec abstract(anno(), term()) -> expr().
abstract(L, Term) ->
    reanno(erl_parse:abstract(Term), L).

reanno(T, L) when is_tuple(T), tuple_size(T) >= 2, is_atom(element(1, T)) ->
    list_to_tuple([element(1, T), L |
                   [reanno(E, L) || E <- tl(tl(tuple_to_list(T)))]]);
reanno(L0, L) when is_list(L0) -> [reanno(E, L) || E <- L0];
reanno(Other, _L)              -> Other.

%%%===================================================================
%%% Calls
%%%===================================================================

-spec rem_call(anno(), module(), atom(), [expr()]) -> expr().
rem_call(L, M, F, Args) -> {call, L, {remote, L, a(L, M), a(L, F)}, Args}.

-spec loc_call(anno(), atom(), [expr()]) -> expr().
loc_call(L, F, Args) -> {call, L, a(L, F), Args}.

%% @doc A guard-legal BIF call, e.g. is_list/1.
-spec g(anno(), atom(), [expr()]) -> expr().
g(L, F, Args) -> {call, L, a(L, F), Args}.

-spec hd_call(anno(), expr()) -> expr().
hd_call(L, E) -> {call, L, a(L, hd), [E]}.

%%%===================================================================
%%% Functions and specs
%%%===================================================================

-spec fn(anno(), atom(), [expr()], [expr()]) -> form().
fn(L, Name, Params, Body) ->
    {function, L, Name, length(Params), [cl(L, Params, [], Body)]}.

-spec spec(anno(), atom(), [expr()], expr()) -> form().
spec(L, Name, ArgTypes, Ret) ->
    {attribute, L, spec,
     {{Name, length(ArgTypes)},
      [{type, L, 'fun', [{type, L, product, ArgTypes}, Ret]}]}}.

-spec t(anno(), atom()) -> expr().
t(L, list) -> {type, L, list, [{type, L, term, []}]};
t(L, Name) -> {type, L, Name, []}.

%% @doc A parameter that is underscored when the body never mentions it.
%%
%% Generated modules are compiled with warnings_as_errors; an unused named
%% parameter would fail the build.
-spec param(anno(), atom(), term()) -> expr().
param(L, Name, Body) ->
    case uses_var(Body, Name) of
        true  -> v(L, Name);
        false -> v(L, list_to_atom("_" ++ atom_to_list(Name)))
    end.

-spec uses_var(term(), atom()) -> boolean().
uses_var({var, _, Name}, Name)     -> true;
uses_var(T, Name) when is_tuple(T) -> uses_var(tuple_to_list(T), Name);
uses_var([H | T], Name)            -> uses_var(H, Name) orelse uses_var(T, Name);
uses_var(_, _)                     -> false.

%%%===================================================================
%%% Introspection
%%%===================================================================

%% @doc Every module named in a remote call anywhere inside Term.
-spec remotes(term(), [module()]) -> [module()].
remotes({call, _, {remote, _, {atom, _, M}, _}, Args}, Acc) ->
    remotes(Args, [M | Acc]);
remotes(T, Acc) when is_tuple(T) ->
    remotes(tuple_to_list(T), Acc);
remotes([H | T], Acc) ->
    remotes(T, remotes(H, Acc));
remotes(_, Acc) ->
    Acc.

%% @doc The binary a literal binary expression denotes, or `error'.
-spec literal_of(term()) -> binary() | error.
literal_of({bin, _, []}) ->
    <<>>;
literal_of({bin, _, [{bin_element, _, {string, _, Chars}, default, default}]}) ->
    list_to_binary(Chars);
literal_of({bin, _, [{bin_element, _, {string, _, Chars}, default, [utf8]}]}) ->
    unicode:characters_to_binary(Chars, utf8);
literal_of({bin, _, Elems}) ->
    case lists:all(fun({bin_element, _, {integer, _, _}, default, default}) -> true;
                      (_) -> false
                   end, Elems) of
        true  -> << <<B>> || {bin_element, _, {integer, _, B}, _, _} <- Elems >>;
        false -> error
    end;
literal_of(_) ->
    error.

%% @doc Constant folding: the whole expression list as one binary, or `no'.
%%
%% `IndentVar' names a variable known to be bound to the empty binary on the
%% path being folded; it contributes nothing to the output and is skipped.
-spec static_text([expr()], atom()) -> {yes, binary()} | no.
static_text(Exprs, IndentVar) ->
    case lists:all(fun(E) -> foldable(E, IndentVar) end, Exprs) of
        false -> no;
        true  -> {yes, iolist_to_binary([B || {bin, _, _} = E <- Exprs,
                                              B <- [literal_of(E)]])}
    end.

-spec foldable(expr(), atom()) -> boolean().
foldable({bin, _, _} = E, _Var) -> literal_of(E) =/= error;
foldable({var, _, Var}, Var)    -> true;
foldable(_, _)                  -> false.

%%%===================================================================
%%% Annotations
%%%===================================================================

%% @doc compile:forms/2 takes erl_anno:anno(), which is opaque; passing a bare
%% integer type-checks by accident but dialyzer rejects it.
-spec anno(non_neg_integer()) -> anno().
anno(L) -> erl_anno:new(L).
