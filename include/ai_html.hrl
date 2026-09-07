%%%-------------------------------------------------------------------
%%% aihtml -- the part of the data contract that is engine-neutral.
%%%
%%% Only what the mustache and jinja front ends genuinely share lives here.
%%% Engine-specific node, token and option types belong in ai_mustache.hrl and
%%% ai_jinja.hrl respectively; the moment this header starts collecting them it
%%% becomes a second dumping ground and the shared layer stops being shared.
%%%
%%% Types, macros and comments only -- no functions.
%%%
%%% See designs/08-jinja-architecture.md.
%%%-------------------------------------------------------------------
-ifndef(AI_HTML_HRL).
-define(AI_HTML_HRL, true).

%%%===================================================================
%%% Basic types
%%%===================================================================

%% Position in a template source, 1-based in both dimensions.
-type ai_html_loc() :: {Line :: pos_integer(), Col :: pos_integer()}.

%% Template bodies and paths are UTF-8 binaries throughout; ai_html_text is
%% the only place a conversion happens.
-type ai_html_path() :: binary().

%% Every engine reports failures in this shape, so the plugin and the
%% parse_transforms can format a diagnostic without knowing which engine
%% produced it. `Reason' is the engine's own reason type.
-type ai_html_error(Reason) ::
        {error, {File :: ai_html_path(), Line :: pos_integer(), Reason}}.

%%%===================================================================
%%% Configuration keys
%%%===================================================================

%% What each engine's options block understands. Anything else earns a warning
%% (an error under warnings_as_errors). The lists live here rather than in the
%% plugin because they describe the engine, not the build tool; the plugin
%% reaches them through Engine:known_keys/0.
-define(AI_HTML_MUSTACHE_KEYS,
        [views, suffix, out_dir, prefix, extensions, ext_opts, line_map,
         warnings_as_errors]).

-define(AI_HTML_JINJA_KEYS,
        [views, suffix, out_dir, prefix, extensions, line_map,
         warnings_as_errors,
         escape, trim_blocks, lstrip_blocks, keep_trailing_newline,
         strict_undefined]).

-endif.
