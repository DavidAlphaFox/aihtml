%%%-------------------------------------------------------------------
%%% @doc Lexical scanning for the jinja engine.
%%%
%%% Splits a template into text, `{{ ... }}' and `{% ... %}' tokens, drops
%%% comments, consumes `{% raw %}' blocks, and applies the whole whitespace
%%% control story (designs/09-jinja-syntax.md section 2).
%%%
%%% == Two passes, on purpose ==
%%%
%%% chunks/2 finds the tags and records, for each, which whitespace markers
%%% surrounded it; weave/2 then applies the trimming. Doing both at once means
%%% deciding how to trim a piece of text before knowing what follows it, which
%%% is precisely the thing that makes hand-written whitespace handling wrong.
%%%
%%% == Trimming never touches interpolated values ==
%%%
%%% `{{- x -}}' trims the static text on either side of the tag. The value of
%%% `x' keeps whatever padding it has. A bug here only shows up when a value
%%% happens to start with whitespace, which is why there is a fixture for
%%% exactly that.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_scanner).

-include("ai_jinja.hrl").

-export([scan/2, opt/3]).

-type loc()   :: ai_html_loc().
-type mark()  :: none | minus | plus.
-type kind()  :: expr | stmt | comment | raw_open | raw_close.

%% An intermediate item. Text is raw; a tag knows the markers on both sides so
%% that weave/2 can decide what to strip.
-type chunk() ::
      {text, loc(), binary()}
    | {tag, loc(), kind(), Data :: term(), mark(), mark()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec scan(unicode:chardata(), map()) ->
          {ok, [ai_jinja_token()]} | ai_jinja_error().
scan(Data, Opts) ->
    case ai_html_text:template(Data) of
        {error, {invalid_utf8, _} = R} ->
            {error, {ai_html_text:source(Opts), 1, R}};
        {ok, Bin0} ->
            Bin = trailing_newline(newlines(Bin0), Opts),
            case chunks(Bin, {1, 1}, []) of
                {error, {L, _C}, Reason} ->
                    {error, {ai_html_text:source(Opts), L, Reason}};
                {ok, Chunks} ->
                    {ok, weave(Chunks, Opts)}
            end
    end.

%% @doc Read a boolean option with the engine's default.
%%
%% Exported because the parser and the compiler need the same defaults and
%% there must be exactly one statement of what they are.
-spec opt(atom(), map(), boolean()) -> boolean().
opt(Key, Opts, Default) ->
    case Opts of
        #{Key := V} when is_boolean(V) -> V;
        _                              -> Default
    end.

%% Line endings are normalised to \n before anything else looks at the text.
%% The reference implementation does the same (its newline_sequence), and
%% without it every whitespace rule would need a CRLF variant.
-spec newlines(binary()) -> binary().
newlines(Bin) ->
    case binary:match(Bin, [<<"\r\n">>, <<"\r">>]) of
        nomatch -> Bin;
        _ -> binary:replace(binary:replace(Bin, <<"\r\n">>, <<"\n">>, [global]),
                            <<"\r">>, <<"\n">>, [global])
    end.

%% keep_trailing_newline is off by default, matching the reference
%% implementation: exactly one trailing newline is dropped, not all of them.
-spec trailing_newline(binary(), map()) -> binary().
trailing_newline(Bin, Opts) ->
    case opt(keep_trailing_newline, Opts, false) of
        true  -> Bin;
        false ->
            case Bin of
                <<>> -> Bin;
                _ ->
                    case binary:last(Bin) of
                        $\n -> binary:part(Bin, 0, byte_size(Bin) - 1);
                        _   -> Bin
                    end
            end
    end.

%%%===================================================================
%%% Pass 1: find the tags
%%%===================================================================

-spec chunks(binary(), loc(), [chunk()]) ->
          {ok, [chunk()]} | {error, loc(), ai_jinja_reason()}.
chunks(Bin, Loc, Acc) ->
    %% `{#' is looked for alongside the others rather than after them: a
    %% comment may legally contain an unclosed `{%', and searching for
    %% statements first would report that as a syntax error.
    case binary:match(Bin, [?AI_JINJA_EXPR_START, ?AI_JINJA_STMT_START,
                            ?AI_JINJA_COMM_START]) of
        nomatch ->
            {ok, lists:reverse(text(Loc, Bin, Acc))};
        {Pos, 2} ->
            Text = binary:part(Bin, 0, Pos),
            TagLoc = advance(Loc, Text),
            Rest0 = binary:part(Bin, Pos, byte_size(Bin) - Pos),
            Acc1 = text(Loc, Text, Acc),
            case tag(Rest0, TagLoc) of
                {error, _, _} = E   -> E;
                {ok, Chunks, Rest, Loc1} -> chunks(Rest, Loc1,
                                                   lists:reverse(Chunks) ++ Acc1)
            end
    end.

text(_Loc, <<>>, Acc) -> Acc;
text(Loc, Bin, Acc)   -> [{text, Loc, Bin} | Acc].

%%%===================================================================
%%% One tag
%%%===================================================================

-spec tag(binary(), loc()) ->
          {ok, [chunk()], binary(), loc()} | {error, loc(), ai_jinja_reason()}.
tag(<<"{#", Rest/binary>>, Loc) ->
    comment(Rest, Loc);
tag(<<"{{", _/binary>> = Bin, Loc) ->
    inline_tag(Bin, Loc, expr, expr_end);
tag(<<"{%", _/binary>> = Bin, Loc) ->
    inline_tag(Bin, Loc, stmt, stmt_end).

%% `{{' and `{%' differ only in which stop sequence ends them, so they share
%% everything but the two constants.
inline_tag(Bin, Loc, Kind, Stop) ->
    <<_:2/binary, Rest0/binary>> = Bin,
    {LMark, Body, Loc0} = left_mark(Rest0, adv(Loc, 2)),
    case ai_jinja_lexer:tokens(Body, Loc0, Stop) of
        {error, _, _} = E ->
            E;
        {ok, Toks, RMark, Rest, Loc1} when Kind =:= expr ->
            {ok, [{tag, Loc, expr, Toks, LMark, RMark}], Rest, Loc1};
        {ok, Toks, RMark, Rest, Loc1} ->
            statement(Toks, RMark, LMark, Loc, Rest, Loc1)
    end.

left_mark(<<$-, R/binary>>, Loc) -> {minus, R, adv(Loc, 1)};
left_mark(<<$+, R/binary>>, Loc) -> {plus,  R, adv(Loc, 1)};
left_mark(R, Loc)                -> {none,  R, Loc}.

%%%===================================================================
%%% Comments
%%%===================================================================

%% A comment has no lexical structure at all -- it may contain unbalanced
%% quotes, unclosed tags, anything -- so its end is a plain byte search.
comment(Rest0, Loc) ->
    {LMark, Body, Loc0} = left_mark(Rest0, adv(Loc, 2)),
    case binary:match(Body, ?AI_JINJA_COMM_STOP) of
        nomatch ->
            {error, Loc, {unclosed_block, comment, Loc}};
        {Pos, 2} ->
            Inner = binary:part(Body, 0, Pos),
            {RMark, Kept} = right_mark(Inner),
            Rest = binary:part(Body, Pos + 2, byte_size(Body) - Pos - 2),
            Loc1 = adv(advance(Loc0, Kept), 2 + mark_width(RMark)),
            {ok, [{tag, Loc, comment, none, LMark, RMark}], Rest, Loc1}
    end.

right_mark(Bin) ->
    case byte_size(Bin) of
        0 -> {none, Bin};
        N ->
            case binary:last(Bin) of
                $- -> {minus, binary:part(Bin, 0, N - 1)};
                $+ -> {plus,  binary:part(Bin, 0, N - 1)};
                _  -> {none, Bin}
            end
    end.

mark_width(none) -> 0;
mark_width(_)    -> 1.

%%%===================================================================
%%% Statements, and the raw block
%%%===================================================================

%% The keyword is the first token. It may lex as a keyword rather than a name
%% -- `{% if %}' and `{% for %}' take different token types for the same job --
%% so both are accepted here and the parser sees one uniform atom.
statement([], _RMark, _LMark, Loc, _Rest, _Loc1) ->
    {error, Loc, {unknown_statement, ''}};
statement([{name, _, raw} | _], RMark, LMark, Loc, Rest, Loc1) ->
    raw_block(Rest, Loc1, Loc, LMark, RMark);
statement([First | Args], RMark, LMark, Loc, Rest, Loc1) ->
    case keyword(First) of
        {ok, Kw} -> {ok, [{tag, Loc, stmt, {Kw, Args}, LMark, RMark}], Rest, Loc1};
        error    -> {error, element(2, First), {unknown_statement, ''}}
    end.

keyword({name, _, N}) -> {ok, N};
keyword({kw, _, K})   -> {ok, K};
keyword(_)            -> error.

%% `{% raw %}' is consumed here, so the statement parser never learns it
%% exists. The body becomes an ordinary text chunk and the two tags become
%% marker-only chunks, which is what makes `{%- raw -%}' work without any
%% special case downstream.
raw_block(Bin, Loc, OpenLoc, LMark, RMark) ->
    case find_endraw(Bin, 0) of
        notfound ->
            {error, OpenLoc, {unclosed_block, raw, OpenLoc}};
        {Pos, TagLen, ELMark, ERMark} ->
            Body = binary:part(Bin, 0, Pos),
            BodyLoc = Loc,
            CloseLoc = advance(Loc, Body),
            Rest = binary:part(Bin, Pos + TagLen, byte_size(Bin) - Pos - TagLen),
            Chunks = [{tag, OpenLoc, raw_open, none, LMark, RMark}]
                ++ [{text, BodyLoc, Body} || Body =/= <<>>]
                ++ [{tag, CloseLoc, raw_close, none, ELMark, ERMark}],
            {ok, Chunks, Rest, adv(CloseLoc, TagLen)}
    end.

%% Scan forward for a `{% endraw %}', tolerating the markers and any spacing.
%% Anything else that looks like a tag inside the body is left alone, which is
%% the entire point of raw; nested `{% raw %}' does not nest, matching the
%% reference implementation.
find_endraw(Bin, From) ->
    case binary:match(Bin, ?AI_JINJA_STMT_START, [{scope, {From, byte_size(Bin) - From}}]) of
        nomatch -> notfound;
        {Pos, 2} ->
            case endraw_at(Bin, Pos + 2) of
                {ok, EndPos, LMark, RMark} ->
                    {Pos, EndPos - Pos, LMark, RMark};
                no ->
                    find_endraw(Bin, Pos + 2)
            end
    end.

endraw_at(Bin, P0) ->
    {LMark, P1} = mark_at(Bin, P0),
    P2 = skip_ws(Bin, P1),
    case word_at(Bin, P2) of
        {<<"endraw">>, P3} ->
            P4 = skip_ws(Bin, P3),
            {RMark, P5} = mark_at(Bin, P4),
            case binary:part(Bin, P5, min(2, byte_size(Bin) - P5)) of
                ?AI_JINJA_STMT_STOP -> {ok, P5 + 2, LMark, RMark};
                _                   -> no
            end;
        _ -> no
    end.

mark_at(Bin, P) when P < byte_size(Bin) ->
    case binary:at(Bin, P) of
        $- -> {minus, P + 1};
        $+ -> {plus, P + 1};
        _  -> {none, P}
    end;
mark_at(_Bin, P) -> {none, P}.

skip_ws(Bin, P) when P < byte_size(Bin) ->
    case binary:at(Bin, P) of
        C when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n -> skip_ws(Bin, P + 1);
        _ -> P
    end;
skip_ws(_Bin, P) -> P.

word_at(Bin, P) -> word_at(Bin, P, P).
word_at(Bin, P0, P) when P < byte_size(Bin) ->
    case binary:at(Bin, P) of
        C when C =:= $_; C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9 ->
            word_at(Bin, P0, P + 1);
        _ -> {binary:part(Bin, P0, P - P0), P}
    end;
word_at(Bin, P0, P) -> {binary:part(Bin, P0, P - P0), P}.

%%%===================================================================
%%% Pass 2: whitespace control
%%%===================================================================

%% Both ends of a text chunk are decided from the ORIGINAL chunk, then cut
%% together. Doing it sequentially -- trim the front, then lstrip what is left
%% -- gives the wrong answer for the very common `{% if %}\n  {% endif %}',
%% where trim_blocks eats the newline and lstrip would then no longer see a
%% line start in front of the two spaces. In the source there is one, so both
%% cuts must be measured before either is applied.
%%
%% Order within one end is still fixed (designs/09-jinja-syntax.md 2.3): the
%% explicit marker overrides the global option.
-spec weave([chunk()], map()) -> [ai_jinja_token()].
weave(Chunks, Opts) ->
    Cfg = {opt(trim_blocks, Opts, true), opt(lstrip_blocks, Opts, true)},
    weave(Chunks, Cfg, none, true, []).

weave([], _Cfg, _Pending, _First, Acc) ->
    lists:reverse(Acc);
weave([{text, Loc, Bin} | Rest], Cfg, Pending, First, Acc) ->
    Start = front_cut(Bin, Pending, Cfg),
    Stop  = back_cut(Bin, next_tag(Rest), First, Cfg),
    Kept  = case Stop > Start of
                true  -> binary:part(Bin, Start, Stop - Start);
                false -> <<>>
            end,
    Loc1 = advance(Loc, binary:part(Bin, 0, Start)),
    weave(Rest, Cfg, none, false, emit_text(Loc1, Kept, Acc));
weave([{tag, Loc, Kind, Data, _LMark, RMark} | Rest], Cfg, _Pending, _First, Acc) ->
    weave(Rest, Cfg, {RMark, Kind}, false, emit_tag(Loc, Kind, Data, Acc)).

next_tag([{tag, _, Kind, _, LMark, _} | _]) -> {LMark, Kind};
next_tag(_)                                 -> none.

emit_text(_Loc, <<>>, Acc)  -> Acc;
emit_text(Loc, Bin, Acc)    -> [{text, Loc, Bin} | Acc].

emit_tag(Loc, expr, Toks, Acc)        -> [{expr, Loc, Toks} | Acc];
emit_tag(Loc, stmt, {Kw, Args}, Acc)  -> [{stmt, Loc, Kw, Args} | Acc];
emit_tag(_Loc, _Kind, _Data, Acc)     -> Acc.   % comment, raw_open, raw_close

%%%===================================================================
%%% The three rules
%%%===================================================================

%% How much to drop from the FRONT: the right-hand rule of the preceding tag.
-spec front_cut(binary(), {mark(), kind()} | none, {boolean(), boolean()}) ->
          non_neg_integer().
front_cut(_Bin, none, _Cfg)          -> 0;
front_cut(Bin, {minus, _Kind}, _Cfg) -> leading_ws(Bin, 0);
front_cut(_Bin, {plus, _Kind}, _Cfg) -> 0;
front_cut(Bin, {none, Kind}, {Trim, _LStrip}) ->
    case Trim andalso trim_after(Kind) of
        true  -> one_newline(Bin);
        false -> 0
    end.

%% Where to stop at the BACK: the left-hand rule of the following tag.
-spec back_cut(binary(), {mark(), kind()} | none, boolean(),
               {boolean(), boolean()}) -> non_neg_integer().
back_cut(Bin, none, _First, _Cfg)          -> byte_size(Bin);
back_cut(Bin, {minus, _Kind}, _First, _Cfg) -> trailing_ws(Bin, byte_size(Bin));
back_cut(Bin, {plus, _Kind}, _First, _Cfg)  -> byte_size(Bin);
back_cut(Bin, {none, Kind}, First, {_Trim, LStrip}) ->
    case LStrip andalso lstrip_before(Kind) of
        true  -> lstrip_at(Bin, First);
        false -> byte_size(Bin)
    end.

%% trim_blocks and lstrip_blocks apply to `{%' and `{#', never to `{{'.
lstrip_before(expr)      -> false;
lstrip_before(comment)   -> true;
lstrip_before(stmt)      -> true;
lstrip_before(raw_open)  -> true;
lstrip_before(raw_close) -> true.

%% ... with one asymmetry: trim_blocks does NOT eat the newline after
%% `{% raw %}'. The reference implementation spells its raw-open rule without
%% the block suffix, so `{% raw %}\na\n{% endraw %}' keeps both newlines even
%% with trim_blocks on. `{%- raw -%}' still trims, because that is the
%% explicit marker and it is handled before this ever runs.
trim_after(expr)      -> false;
trim_after(comment)   -> true;
trim_after(stmt)      -> true;
trim_after(raw_open)  -> false;
trim_after(raw_close) -> true.

%% `{{-' and friends eat every whitespace character, newlines included and
%% across as many lines as there are. Surprising, and exactly what the
%% reference implementation does.
leading_ws(Bin, N) when N < byte_size(Bin) ->
    case binary:at(Bin, N) of
        C when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n -> leading_ws(Bin, N + 1);
        _ -> N
    end;
leading_ws(_Bin, N) -> N.

trailing_ws(_Bin, 0) -> 0;
trailing_ws(Bin, N) ->
    case binary:at(Bin, N - 1) of
        C when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n -> trailing_ws(Bin, N - 1);
        _ -> N
    end.

%% trim_blocks: exactly one newline directly after the tag, plus the optional
%% carriage return in front of it.
one_newline(<<"\r\n", _/binary>>) -> 2;
one_newline(<<"\n", _/binary>>)    -> 1;
one_newline(_)                    -> 0.

%% lstrip_blocks: the run of spaces and tabs between the last newline and the
%% tag, and only if that is all there is. `a {% if %}' keeps its space.
%%
%% `First' says whether this chunk begins the template, which is the only way
%% a chunk can be at a line start without containing the newline itself: every
%% other chunk is preceded by a tag, and a tag is not a line start even when
%% it produces no output.
lstrip_at(Bin, First) ->
    N = byte_size(Bin),
    Start = tabs_and_spaces(Bin, N),
    AtLineStart = case Start of
                      0 -> First;
                      _ -> binary:at(Bin, Start - 1) =:= $\n
                  end,
    case AtLineStart of
        true  -> Start;
        false -> N
    end.

tabs_and_spaces(_Bin, 0) -> 0;
tabs_and_spaces(Bin, N) ->
    case binary:at(Bin, N - 1) of
        C when C =:= $\s; C =:= $\t -> tabs_and_spaces(Bin, N - 1);
        _ -> N
    end.

%%%===================================================================
%%% Positions
%%%===================================================================

-spec adv(loc(), non_neg_integer()) -> loc().
adv({L, C}, N) -> {L, C + N}.

%% Move a position over a piece of text, counting the newlines in it.
-spec advance(loc(), binary()) -> loc().
advance(Loc, <<>>) -> Loc;
advance({L, C}, Bin) ->
    case binary:matches(Bin, <<"\n">>) of
        []      -> {L, C + count_chars(Bin)};
        Matches ->
            {Last, 1} = lists:last(Matches),
            Tail = binary:part(Bin, Last + 1, byte_size(Bin) - Last - 1),
            {L + length(Matches), 1 + count_chars(Tail)}
    end.

%% Columns count characters, not bytes: a diagnostic that points past the end
%% of a line because the line had a `中' in it is worse than useless.
count_chars(Bin) ->
    case unicode:characters_to_list(Bin, utf8) of
        L when is_list(L) -> length(L);
        _                 -> byte_size(Bin)
    end.
