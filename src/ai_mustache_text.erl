%%%-------------------------------------------------------------------
%%% @doc Deprecated forwarding shell for {@link ai_html_text}.
%%%
%%% Text normalisation is engine-neutral and now lives in `ai_html_text', so
%%% that the mustache and jinja front ends cannot drift on what a path or a
%%% template body is (designs/08-jinja-architecture.md section 1.1).
%%%
%%% This module stays as a forwarding shell because it is a public module and
%%% callers outside this repository may use it. It is scheduled for removal in
%%% v0.6.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_text).

-export([template/1, path/1, source/1, opts/1, to_list/1]).

-deprecated([{template, 1, "use ai_html_text:template/1"},
             {path,     1, "use ai_html_text:path/1"},
             {source,   1, "use ai_html_text:source/1"},
             {opts,     1, "use ai_html_text:opts/1"},
             {to_list,  1, "use ai_html_text:to_list/1"}]).

-spec template(unicode:chardata()) ->
          {ok, binary()} | {error, {invalid_utf8, non_neg_integer()}}.
template(Data) -> ai_html_text:template(Data).

-spec path(unicode:chardata()) -> binary().
path(Data) -> ai_html_text:path(Data).

-spec source(map()) -> binary().
source(Opts) -> ai_html_text:source(Opts).

-spec opts(map()) -> map().
opts(Opts) -> ai_html_text:opts(Opts).

-spec to_list(unicode:chardata()) -> string().
to_list(Data) -> ai_html_text:to_list(Data).
