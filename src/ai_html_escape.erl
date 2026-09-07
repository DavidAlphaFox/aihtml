%%%-------------------------------------------------------------------
%%% @doc HTML escaping and value formatting, shared by every aihtml engine.
%%%
%%% Both functions take an error tag so that a failure surfaces under the
%%% engine the template was written in: a jinja template that interpolates a
%%% pid should not raise `{ai_mustache, ...}'. The tag is an extra argument
%%% rather than a try/catch in each engine's runtime because these two
%%% functions are the hot path of every render.
%%%
%%% This module may not reference any ai_mustache_* or ai_jinja_* module
%%% (designs/08-jinja-architecture.md, architecture invariant 10).
%%% @end
%%%-------------------------------------------------------------------
-module(ai_html_escape).

-export([escape/1, escape/2, escape/3, to_binary/1, to_binary/2]).

%% The two spellings of the double-quote entity that the two engines use.
%% Mustache's spec says &quot;; jinja's Markup.escape emits &#34;. Everything
%% else about the escape set is identical, so it is one parameter rather than
%% two implementations.
-define(QUOT_NAMED, <<"&quot;">>).
-define(QUOT_NUMERIC, <<"&#34;">>).

%%%===================================================================
%%% to_binary
%%%===================================================================

-spec to_binary(term()) -> binary().
to_binary(V) -> to_binary(V, ai_html).

%% @doc Render a value as a binary.
%%
%% Floats use `float_to_binary/2' with `short' (OTP 24+): the default
%% formatting turns `1.21' into `"1.21000000000000000000e+00"' and fails the
%% mustache spec's decimal interpolation cases
%% (designs/02-architecture.md 6.2).
%%
%% `undefined' and `null' render as the empty binary -- a missing variable
%% renders as nothing -- so both clauses have to sit above the generic atom
%% clause.
-spec to_binary(term(), atom()) -> binary().
to_binary(B, _Tag) when is_binary(B)  -> B;      % zero copy, the hot path
to_binary(I, _Tag) when is_integer(I) -> integer_to_binary(I);
to_binary(F, _Tag) when is_float(F)   -> float_to_binary(F, [short]);
to_binary(undefined, _Tag)            -> <<>>;
to_binary(null, _Tag)                 -> <<>>;
to_binary(A, _Tag) when is_atom(A)    -> atom_to_binary(A, utf8);
%% Lists are ambiguous by nature: `[104, 105]' is both a two element list of
%% integers and the string "hi", and nothing in the value itself tells the
%% two apart. A defined behaviour beats a crash, so a list is read as a
%% unicode character list, falling back to a plain iolist (byte list) when
%% that fails.
to_binary(L, _Tag) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _Error              -> iolist_to_binary(L)
    end;
to_binary(T, Tag) -> error({Tag, {not_renderable, T}}).

%%%===================================================================
%%% escape
%%%===================================================================

-spec escape(term()) -> iodata().
escape(V) -> escape(V, ai_html).

%% @doc HTML-escape a value, returning iodata.
%%
%% The escape set is exactly five characters:
%%
%%   `&' -> `&amp;'   `<' -> `&lt;'   `>' -> `&gt;'
%%   `"' -> Q          `'' -> `&#39;'
%%
%% where Q is `&quot;' or `&#34;', see the macros above.
%%
%% `&' becomes `&amp;' WITH the semicolon (bug B3 in the pre-0.4 ailib
%% table), and `/', `=' and the backtick are deliberately NOT escaped:
%% escaping them corrupts URLs and ordinary prose.
%%
%% One pass over the binary, slicing with binary:part/3 and building an
%% iolist -- no `re:replace', no byte-wise append. A single pass is also what
%% makes the escape order irrelevant: with multiple passes, `&' having to run
%% first or last is a correctness trap (`&lt;' getting re-escaped into
%% `&amp;lt;'), and here it simply cannot happen.
%%
%% Scanning byte by byte is safe for UTF-8: all five characters are ASCII and
%% every continuation byte of a multi-byte sequence is >= 0x80, so no
%% multi-byte character can be hit by accident. No decoding is needed.
-spec escape(term(), atom()) -> iodata().
escape(V, Tag) -> escape(V, Tag, ?QUOT_NAMED).

-spec escape(term(), atom(), binary()) -> iodata().
escape(B, _Tag, Q) when is_binary(B) -> scan(B, 0, 0, [], Q);
escape(V, Tag, Q)                    -> scan(to_binary(V, Tag), 0, 0, [], Q).

%% scan(Bin, SegStart, Pos, RevAcc): `SegStart' is where the current verbatim
%% run started, `Pos' the byte being looked at, `RevAcc' the output in reverse.
-spec scan(binary(), non_neg_integer(), non_neg_integer(), [iodata()], binary()) ->
          iodata().
scan(Bin, Start, Pos, Acc, Q) when Pos < byte_size(Bin) ->
    case binary:at(Bin, Pos) of
        $&  -> hit(Bin, Start, Pos, Acc, <<"&amp;">>, Q);
        $<  -> hit(Bin, Start, Pos, Acc, <<"&lt;">>, Q);
        $>  -> hit(Bin, Start, Pos, Acc, <<"&gt;">>, Q);
        $\" -> hit(Bin, Start, Pos, Acc, Q, Q);
        $'  -> hit(Bin, Start, Pos, Acc, <<"&#39;">>, Q);
        _   -> scan(Bin, Start, Pos + 1, Acc, Q)
    end;
%% Nothing was escaped: hand back the very same binary, no allocation. This is
%% why escape/1 returns iodata() and not binary().
scan(Bin, 0, _Pos, [], _Q) -> Bin;
scan(_Bin, Pos, Pos, Acc, _Q) -> lists:reverse(Acc);
scan(Bin, Start, Pos, Acc, _Q) ->
    lists:reverse(Acc, [binary:part(Bin, Start, Pos - Start)]).

-spec hit(binary(), non_neg_integer(), non_neg_integer(), [iodata()], binary(),
          binary()) -> iodata().
hit(Bin, Pos, Pos, Acc, Rep, Q) ->
    scan(Bin, Pos + 1, Pos + 1, [Rep | Acc], Q);
hit(Bin, Start, Pos, Acc, Rep, Q) ->
    Seg = binary:part(Bin, Start, Pos - Start),
    scan(Bin, Pos + 1, Pos + 1, [Rep, Seg | Acc], Q).
