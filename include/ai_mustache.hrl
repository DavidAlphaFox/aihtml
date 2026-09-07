%%%-------------------------------------------------------------------
%%% aihtml -- shared data contract.
%%%
%%% This header is the single source of truth for the types that scanner,
%%% parser, AST post-processing, compiler, the rebar3 plugin and user-written
%%% extension modules all agree on. It contains types, macros and comments
%%% only -- no functions.
%%%
%%% See designs/04-codegen.md and tasks/T06.md.
%%%-------------------------------------------------------------------
-ifndef(AI_MUSTACHE_HRL).
-define(AI_MUSTACHE_HRL, true).

%%%===================================================================
%%% Delimiters and version
%%%===================================================================

-define(AI_MUSTACHE_START, <<"{{">>).
-define(AI_MUSTACHE_STOP,  <<"}}">>).

%% Shape version of the generated code. Bump whenever the compiler changes
%% what it emits, otherwise previously generated modules are not recompiled.
%% Feeds both the `vsn' field of -mustache_source and the stamp computed by
%% ai_mustache_compiler:source_hash/2.
-define(AI_MUSTACHE_VSN, 1).

%% Sentinel key for the implicit iterator {{.}}. ai_mustache_rt:lookup/2
%% recognises a keys() of exactly [?AI_MUSTACHE_DOT] and returns hd(Stack).
-define(AI_MUSTACHE_DOT, '.').

%% Markers claimed by aihtml itself. Extension modules may only register
%% characters outside this set; the conflict check lives in the plugin and in
%% the parse_transform, but this list is the only definition of the set.
-define(AI_MUSTACHE_BUILTIN_MARKERS,
        [$#, $^, $/, $>, $!, $=, $&, ${, $}, $+, $-, $*]).

%%%===================================================================
%%% Basic types
%%%===================================================================

-type ai_mustache_key()  :: atom().
-type ai_mustache_keys() :: [ai_mustache_key()].

%% Template bodies and paths are UTF-8 binaries throughout. Callers may hand
%% in any unicode:chardata() at an entry point; ai_mustache_text normalises it
%% once and everything downstream sees a binary.
-type ai_mustache_template() :: binary().
-type ai_mustache_path()     :: binary().

%% Position in the template source. Used for diagnostics and, when the
%% line_map option is on, for the line numbers of the generated forms.
-type ai_mustache_loc()  :: {Line :: pos_integer(), Col :: pos_integer()}.

%%%===================================================================
%%% Scanner output -- the only interface between T07 and T08
%%%===================================================================

%% `none' is a plain {{x}} interpolation; anything else is the marker
%% character that introduced the tag.
-type ai_mustache_marker() :: none | char().

-type ai_mustache_token() ::
      {text, ai_mustache_loc(), binary()}
    | {tag,  ai_mustache_loc(), ai_mustache_marker(),
             Content :: binary(),      % marker stripped, outer space trimmed
             Indent  :: binary()}.     % standalone line indent, else <<>>

%%%===================================================================
%%% AST
%%%===================================================================

%% A partial's target starts life as the raw path written in the template and
%% becomes a module name once ai_mustache_ast:resolve_partials/2 has run.
%%   before: <<"shared/item">>
%%   after:  view_shared_item
-type ai_mustache_partial_target() :: binary() | module().

%% NOTE: this type cannot be named node/0 -- that is an Erlang builtin and the
%% compiler rejects any attempt to redefine it.
-type ai_mustache_node() ::
      {text,     ai_mustache_loc(), binary()}
    | {var,      ai_mustache_loc(), ai_mustache_keys(), escape | raw}
    | {section,  ai_mustache_loc(), ai_mustache_keys(), [ai_mustache_node()]}
    | {inverted, ai_mustache_loc(), ai_mustache_keys(), [ai_mustache_node()]}
    | {has,      ai_mustache_loc(), ai_mustache_keys(), [ai_mustache_node()],
                 Positive :: boolean()}
    | {lambda,   ai_mustache_loc(), ai_mustache_keys()}
    | {partial,  ai_mustache_loc(), ai_mustache_partial_target(),
                 Indent :: binary()}
    | {ext,      ai_mustache_loc(), Marker :: char(), ai_mustache_keys(),
                 [ai_mustache_node()]}.

%%%===================================================================
%%% Compiler options
%%%===================================================================

-type ai_mustache_opts() :: #{
        module     := module(),
        source     := ai_mustache_path(),
        prefix     => ai_mustache_path(),       % default <<"view_">>
        views      => ai_mustache_path(),
        views_abs  => ai_mustache_path(),       % resolved; kept out of the stamp
        suffix     => ai_mustache_path(),
        extensions => [module()],
        ext_opts   => #{module() => term()},    % passed to compile_tag/4
        stack_var  => atom(),                   % context stack variable name
        line_map   => boolean()                 % default true
       }.

%%%===================================================================
%%% Errors
%%%===================================================================

-type ai_mustache_reason() ::
      {unclosed_tag, ai_mustache_keys()}
    | {mismatched_close, Expected :: ai_mustache_keys(),
                         Got      :: ai_mustache_keys()}
    | {partial_not_found, binary()}
    | partial_in_inline_template
    | {unknown_marker, char()}
    | {invalid_delimiter, binary()}
    | {invalid_utf8, ByteOffset :: non_neg_integer()}
    | {ext_crashed, module(), char(), term()}
    | {unexpected_remote_calls, [module()]}
    | {codegen_failed, term()}.

-type ai_mustache_error() ::
        {error, {File   :: binary(),
                 Line   :: pos_integer(),
                 Reason :: ai_mustache_reason()}}.

%%%===================================================================
%%% Generated module self-description
%%%===================================================================

%% Every generated module carries this as a -mustache_source attribute.
%% The plugin reads it for incremental compilation and ai_mustache_dev reads
%% it for hot reloading; neither keeps any state of its own.
%%
%% `stamp' is deliberately not called `hash': it is the combined digest of
%% template content, normalised options and ?AI_MUSTACHE_VSN, not a digest of
%% the template text. `opts' is required, not redundant -- without it a change
%% to mustache_opts would not trigger a rebuild and ai_mustache_dev could not
%% faithfully reproduce the compilation the plugin originally performed.
-type ai_mustache_source() :: #{
        path  := binary(),
        stamp := binary(),
        mtime := integer(),
        vsn   := pos_integer(),
        opts  := map()
       }.

-endif.
