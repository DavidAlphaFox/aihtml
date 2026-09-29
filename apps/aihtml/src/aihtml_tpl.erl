%%%-------------------------------------------------------------------
%%% @doc Shared Mustache templates: apps/aihtml/templates/<name>.mustache.
%%%
%%% A fragment that both the server and the browser render (a datepicker
%%% month, a tag chip, a toast) is written once as a Mustache template and
%%% compiled at build time twice:
%%%
%%%   Erlang   the component module declares
%%%              -compile({parse_transform, beamai_mustache_transform}).
%%%              -mustache_template({tpl_<name>, "../templates/<name>.mustache"}).
%%%            and calls aihtml_tpl:safe(tpl_<name>(Data)), Data using
%%%            atom keys.
%%%   browser  scripts/build-js.mjs compiles every template into
%%%            AH.tpl.<name>(data), data using string keys.
%%%
%%% Both follow beamai_render's semantics (scripts/mustache.mjs), and
%%% aihtml_tpl_tests renders every template's fixtures
%%% (templates/<name>.fixtures.json) on both sides and requires identical
%%% bytes. Templates are logic-less: compute flags and classes in the view
%%% data, in both languages.
%%%
%%% Conventions: no partials or set delimiters; one trailing newline of the
%%% file is not part of the output (both sides strip it).
%%%
%%% rebar3 does not see that a module depends on its template files: after
%%% editing a template, touch the module (or run rebar3 clean). The
%%% cross-language test fails on a stale module.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_tpl).

-export([safe/1, dir/0, names/0]).

%% @doc A rendered template as trusted HTML for an element tree, without
%% the file's trailing newline.
-spec safe(iodata()) -> {safe, binary()}.
safe(IoData) ->
    B = iolist_to_binary(IoData),
    {safe, case B of
               <<Body:(byte_size(B) - 2)/binary, "\r\n">> -> Body;
               <<Body:(byte_size(B) - 1)/binary, "\n">> -> Body;
               _ -> B
           end}.

%% @doc Where the template sources live (in the source tree).
-spec dir() -> file:filename().
dir() ->
    source_dir().

%% @doc Names of all shared templates.
-spec names() -> [binary()].
names() ->
    [list_to_binary(filename:basename(F, ".mustache"))
     || F <- filelib:wildcard(filename:join(dir(), "*.mustache"))].

source_dir() ->
    %% compile info points at the .erl; templates sit next to src/
    Src = proplists:get_value(source, ?MODULE:module_info(compile)),
    filename:join(filename:dirname(filename:dirname(Src)), "templates").
