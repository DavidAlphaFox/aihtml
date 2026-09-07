%%%-------------------------------------------------------------------
%%% @doc Template path -> module name.
%%%
%%%   views/index.mustache          -> view_index
%%%   views/shared/item.mustache    -> view_shared_item
%%%   views/layout/default.mustache -> view_layout_default
%%%
%%% The mapping itself is NOT implemented here: it is
%%% ai_mustache_ast:module_name/2, which the compiler also uses to turn
%%% `{{> shared/item}}' into a module name. Two implementations would drift
%%% and the plugin would then write view_shared_item.erl while the parent
%%% template called some other module. This module only computes the name a
%%% path reduces to, validates that the result is a bare Erlang atom, and
%%% derives the output file.
%%%
%%% Note that the reverse direction is impossible: `_' does not say whether it
%%% came from `/', `-' or `.'. Orphan collection therefore works on a forward
%%% set difference, never on reverse derivation (see rebar3_aihtml_gc).
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml_name).

-include("rebar3_aihtml.hrl").

-export([name_of/3, module_of/2, out_file/2, validate/3]).

%% @doc The views-relative, suffix-free template name, with `/' separators.
%%
%% This is exactly the string a `{{> ...}}' would have to spell to reach this
%% template, which is what makes the two naming paths agree.
name_of(AbsPath, ViewsDir, Suffix) ->
    Rel = rel(AbsPath, ViewsDir),
    Base = lists:sublist(Rel, length(Rel) - length(Suffix)),
    unicode:characters_to_binary(filename:join(filename:split(Base))).

rel(AbsPath, Dir) ->
    D = unicode:characters_to_list(Dir),
    P = unicode:characters_to_list(AbsPath),
    case lists:prefix(D ++ "/", P) of
        true  -> lists:nthtail(length(D) + 1, P);
        false -> filename:basename(P)
    end.

%% @doc Module for a views-relative template name.
module_of(Name, #mopts{compiler_opts = CO}) ->
    ai_mustache_ast:module_name(Name, CO);
module_of(Name, Opts) when is_map(Opts) ->
    ai_mustache_ast:module_name(Name, Opts).

%% @doc out_dir/<module>.erl. Generated files are flat; no subdirectories.
out_file(Mod, #mopts{out_dir = Dir}) ->
    filename:join(Dir, atom_to_list(Mod) ++ ".erl").

%% @doc Reject names that cannot become a bare atom.
%%
%% Silent escaping is not an option: it would create a second, invisible
%% source of module name collisions on top of the `shared/item' vs
%% `shared_item' one. Note that a space is rejected rather than folded to `_',
%% because ai_mustache_ast:module_name/2 does not fold it either and a
%% `{{> a b}}' reference would then resolve to a different (unquotable) atom.
validate(Mod, RelPath, #mopts{prefix = Prefix}) ->
    case bare_atom(atom_to_list(Mod)) of
        true  -> ok;
        false -> {error, {RelPath, 0, {illegal_module_name, Mod, Prefix}}}
    end.

bare_atom([C | Rest]) when C >= $a, C =< $z -> lists:all(fun tail/1, Rest);
bare_atom(_) -> false.

tail(C) when C >= $a, C =< $z -> true;
tail(C) when C >= $A, C =< $Z -> true;
tail(C) when C >= $0, C =< $9 -> true;
tail($_) -> true;
tail($@) -> true;
tail(_)  -> false.
