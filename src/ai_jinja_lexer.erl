%%%-------------------------------------------------------------------
%%% @doc Expression lexer for the jinja engine.
%%%
%%% Turns the bytes inside `{{ ... }}' or `{% ... %}' into tokens, and tells
%%% the scanner where the tag ended.
%%%
%%% == Why the scanner cannot find the end itself ==
%%%
%%% `{{ a|replace("%}", "x") }}' contains a `%}' that is not a delimiter, and
%%% `{{ {"a": 1} }}' contains a `}' that is not one either. Finding the end of
%%% a tag therefore requires knowing about string literals and bracket depth,
%%% which is exactly what a lexer is. So the scanner delegates: tokens/3 runs
%%% until it sees the stop sequence at bracket depth zero and hands back what
%%% is left (designs/09-jinja-syntax.md section 3.4).
%%%
%%% == What this module does not do ==
%%%
%%% No syntax checking whatsoever. `{{ + }}' lexes happily into one operator
%%% token and is rejected by ai_jinja_expr. Keeping the two apart is what lets
%%% both be tested on their own.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_lexer).

-include("ai_jinja.hrl").

-export([tokens/3, keyword_of/1]).

-type stop()  :: expr_end | stmt_end.
-type trim()  :: none | minus | plus.
-type loc()   :: ai_html_loc().
-type token() :: ai_jinja_expr_token().

-export_type([stop/0, trim/0]).

-record(ls, {stop  :: stop(),
             depth = 0 :: non_neg_integer(),
             acc   = [] :: [token()]}).

%% Keywords. `True'/`False'/`None' are the Python spellings; jinja accepts
%% both and so do we, mapped to one token each.
-define(KEYWORDS,
        #{<<"and">> => 'and', <<"or">> => 'or', <<"not">> => 'not',
          <<"in">> => 'in', <<"is">> => 'is',
          <<"if">> => 'if', <<"else">> => 'else',
          <<"true">> => 'true', <<"True">> => 'true',
          <<"false">> => 'false', <<"False">> => 'false',
          <<"none">> => 'none', <<"None">> => 'none'}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Lex until the stop sequence at bracket depth zero.
%%
%% Returns the tokens, the whitespace-control marker that preceded the stop,
%% the bytes after the stop, and the position just after it.
-spec tokens(binary(), loc(), stop()) ->
          {ok, [token()], trim(), binary(), loc()}
        | {error, loc(), ai_jinja_reason()}.
tokens(Bin, Loc, Stop) ->
    scan(Bin, Loc, #ls{stop = Stop}).

%% @doc The keyword a name maps to, or `error'.
-spec keyword_of(binary()) -> {ok, atom()} | error.
keyword_of(Bin) ->
    case ?KEYWORDS of
        #{Bin := K} -> {ok, K};
        _           -> error
    end.

%%%===================================================================
%%% Main loop
%%%===================================================================

-spec scan(binary(), loc(), #ls{}) ->
          {ok, [token()], trim(), binary(), loc()}
        | {error, loc(), ai_jinja_reason()}.
scan(<<>>, Loc, #ls{stop = Stop}) ->
    {error, Loc, {unclosed_block, tag_kind(Stop), Loc}};

%% Whitespace, including newlines inside a multi-line tag.
scan(<<$\n, Rest/binary>>, {L, _C}, St) -> scan(Rest, {L + 1, 1}, St);
scan(<<C, Rest/binary>>, Loc, St) when C =:= $\s; C =:= $\t; C =:= $\r ->
    scan(Rest, adv(Loc, 1), St);

scan(Bin, Loc, #ls{depth = 0} = St) ->
    case stop_at(Bin, St#ls.stop) of
        {yes, Trim, Rest, Width} ->
            {ok, lists:reverse(St#ls.acc), Trim, Rest, adv(Loc, Width)};
        no ->
            token(Bin, Loc, St)
    end;
scan(Bin, Loc, St) ->
    token(Bin, Loc, St).

%% The stop sequence, possibly preceded by a whitespace-control marker.
%%
%% `-' and `+' are only markers directly before the delimiter; anywhere else
%% `-' is subtraction, which is why this is checked as one unit rather than by
%% peeking after a minus token has already been emitted.
-spec stop_at(binary(), stop()) ->
          {yes, trim(), binary(), pos_integer()} | no.
stop_at(<<"-}}", R/binary>>, expr_end) -> {yes, minus, R, 3};
stop_at(<<"+}}", R/binary>>, expr_end) -> {yes, plus,  R, 3};
stop_at(<<"}}",  R/binary>>, expr_end) -> {yes, none,  R, 2};
stop_at(<<"-%}", R/binary>>, stmt_end) -> {yes, minus, R, 3};
stop_at(<<"+%}", R/binary>>, stmt_end) -> {yes, plus,  R, 3};
stop_at(<<"%}",  R/binary>>, stmt_end) -> {yes, none,  R, 2};
stop_at(_, _)                          -> no.

tag_kind(expr_end) -> expr;
tag_kind(stmt_end) -> stmt.

%%%===================================================================
%%% One token
%%%===================================================================

-spec token(binary(), loc(), #ls{}) ->
          {ok, [token()], trim(), binary(), loc()}
        | {error, loc(), ai_jinja_reason()}.

%% Strings first: everything else could otherwise match inside one.
token(<<Q, _/binary>> = Bin, Loc, St) when Q =:= $"; Q =:= $' ->
    case string_lit(Bin, Loc) of
        {ok, S, Rest, Loc1} -> scan(Rest, Loc1, push({str, Loc, S}, St));
        {error, _, _} = E   -> E
    end;

%% Numbers.
token(<<D, _/binary>> = Bin, Loc, St) when D >= $0, D =< $9 ->
    case number(Bin, Loc) of
        {ok, Tok, Rest, Loc1} -> scan(Rest, Loc1, push(setelement(2, Tok, Loc), St));
        {error, _, _} = E     -> E
    end;

%% Names and keywords.
token(<<C, _/binary>> = Bin, Loc, St) when C =:= $_;
                                           C >= $a, C =< $z;
                                           C >= $A, C =< $Z ->
    {Word, Rest} = name(Bin, 0),
    Tok = case keyword_of(Word) of
              {ok, K} -> {kw, Loc, K};
              error   -> {name, Loc, binary_to_atom(Word, utf8)}
          end,
    scan(Rest, adv(Loc, byte_size(Word)), push(Tok, St));

%% Brackets, which is where depth comes from.
token(<<C, Rest/binary>>, Loc, St) when C =:= $(; C =:= $[; C =:= ${ ->
    scan(Rest, adv(Loc, 1), push({open, Loc, C}, St#ls{depth = St#ls.depth + 1}));
%% A close with nothing open is caught here rather than in the parser: at this
%% point the offending byte is still in hand, and by the time a token list
%% reaches the parser the position of the stray bracket is much less obvious.
token(<<C, Rest/binary>>, Loc, #ls{depth = D} = St)
  when C =:= $); C =:= $]; C =:= $} ->
    case D of
        0 -> {error, Loc, {unexpected_token, <<C>>}};
        _ -> scan(Rest, adv(Loc, 1), push({close, Loc, C}, St#ls{depth = D - 1}))
    end;

token(Bin, Loc, St) ->
    case operator(Bin) of
        {ok, Op, Width, Rest} ->
            scan(Rest, adv(Loc, Width), push({op, Loc, Op}, St));
        error ->
            {error, Loc, {unexpected_token, first_char(Bin)}}
    end.

push(Tok, #ls{acc = Acc} = St) -> St#ls{acc = [Tok | Acc]}.

%% Only used to make the diagnostic readable; a non-ASCII identifier lands
%% here, and reporting the whole codepoint beats reporting one stray byte.
-spec first_char(binary()) -> binary().
first_char(Bin) ->
    case unicode:characters_to_list(Bin, utf8) of
        [C | _] when is_integer(C) -> unicode:characters_to_binary([C], utf8);
        _                          -> binary:part(Bin, 0, min(1, byte_size(Bin)))
    end.

%%%===================================================================
%%% Names
%%%===================================================================

%% ASCII only (deviation J10). A non-ASCII byte simply ends the name, and the
%% caller then fails on it as an unexpected token rather than silently
%% truncating the identifier.
-spec name(binary(), non_neg_integer()) -> {binary(), binary()}.
name(Bin, N) when N < byte_size(Bin) ->
    case binary:at(Bin, N) of
        C when C =:= $_;
               C >= $a, C =< $z;
               C >= $A, C =< $Z;
               C >= $0, C =< $9 -> name(Bin, N + 1);
        _ -> split(Bin, N)
    end;
name(Bin, N) -> split(Bin, N).

split(Bin, N) -> {binary:part(Bin, 0, N), binary:part(Bin, N, byte_size(Bin) - N)}.

%%%===================================================================
%%% Numbers
%%%===================================================================

-spec number(binary(), loc()) ->
          {ok, token(), binary(), loc()} | {error, loc(), ai_jinja_reason()}.
number(<<"0x", R/binary>>, Loc) -> radix(R, 16, Loc, 2);
number(<<"0X", R/binary>>, Loc) -> radix(R, 16, Loc, 2);
number(<<"0o", R/binary>>, Loc) -> radix(R, 8, Loc, 2);
number(<<"0O", R/binary>>, Loc) -> radix(R, 8, Loc, 2);
number(<<"0b", R/binary>>, Loc) -> radix(R, 2, Loc, 2);
number(<<"0B", R/binary>>, Loc) -> radix(R, 2, Loc, 2);
number(Bin, Loc)                -> decimal(Bin, Loc).

-spec radix(binary(), 2 | 8 | 16, loc(), pos_integer()) ->
          {ok, token(), binary(), loc()} | {error, loc(), ai_jinja_reason()}.
radix(Bin, Base, Loc, Prefix) ->
    {Digits, Rest} = take(Bin, fun(C) -> is_digit(C, Base) orelse C =:= $_ end),
    case strip_underscores(Digits) of
        <<>> -> {error, Loc, {unexpected_token, binary:part(Bin, 0, min(1, byte_size(Bin)))}};
        D    -> {ok, {int, Loc, binary_to_integer(D, Base)},
                 Rest, adv(Loc, Prefix + byte_size(Digits))}
    end.

-spec decimal(binary(), loc()) -> {ok, token(), binary(), loc()}.
decimal(Bin, Loc) ->
    {Int, R1} = take(Bin, fun(C) -> (C >= $0 andalso C =< $9) orelse C =:= $_ end),
    {Frac, R2} = fraction(R1),
    {Exp, R3} = exponent(R2),
    Width = byte_size(Int) + byte_size(Frac) + byte_size(Exp),
    Text = strip_underscores(<<Int/binary, Frac/binary, Exp/binary>>),
    Tok = case {Frac, Exp} of
              {<<>>, <<>>} -> {int, Loc, binary_to_integer(Text)};
              _            -> {float, Loc, to_float(Text)}
          end,
    {ok, Tok, R3, adv(Loc, Width)}.

%% A `.' only continues the number when a digit follows; `1.foo' is an
%% attribute access on an integer, not a malformed float.
fraction(<<$., D, _/binary>> = Bin) when D >= $0, D =< $9 ->
    <<$., Rest0/binary>> = Bin,
    {Digits, Rest} = take(Rest0, fun(C) -> (C >= $0 andalso C =< $9) orelse C =:= $_ end),
    {<<$., Digits/binary>>, Rest};
fraction(Bin) ->
    {<<>>, Bin}.

exponent(<<E, S, D, _/binary>> = Bin)
  when (E =:= $e orelse E =:= $E), (S =:= $+ orelse S =:= $-), D >= $0, D =< $9 ->
    <<_:2/binary, Rest0/binary>> = Bin,
    {Digits, Rest} = take(Rest0, fun(C) -> C >= $0 andalso C =< $9 end),
    {<<E, S, Digits/binary>>, Rest};
exponent(<<E, D, _/binary>> = Bin) when (E =:= $e orelse E =:= $E), D >= $0, D =< $9 ->
    <<_:1/binary, Rest0/binary>> = Bin,
    {Digits, Rest} = take(Rest0, fun(C) -> C >= $0 andalso C =< $9 end),
    {<<E, Digits/binary>>, Rest};
exponent(Bin) ->
    {<<>>, Bin}.

%% `1e3' is a float in Python and must not go through binary_to_float/1, which
%% insists on a decimal point.
to_float(Text) ->
    case binary:match(Text, <<".">>) of
        nomatch -> insert_point(Text);
        _       -> binary_to_float(Text)
    end.

insert_point(Text) ->
    [Mant, Exp] = binary:split(Text, [<<"e">>, <<"E">>]),
    binary_to_float(<<Mant/binary, ".0e", Exp/binary>>).

is_digit(C, 16) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f)
                       orelse (C >= $A andalso C =< $F);
is_digit(C, 8)  -> C >= $0 andalso C =< $7;
is_digit(C, 2)  -> C =:= $0 orelse C =:= $1.

strip_underscores(B) -> binary:replace(B, <<"_">>, <<>>, [global]).

take(Bin, Pred) -> take(Bin, Pred, 0).
take(Bin, Pred, N) when N < byte_size(Bin) ->
    case Pred(binary:at(Bin, N)) of
        true  -> take(Bin, Pred, N + 1);
        false -> split(Bin, N)
    end;
take(Bin, _Pred, N) -> split(Bin, N).

%%%===================================================================
%%% Strings
%%%===================================================================

-spec string_lit(binary(), loc()) ->
          {ok, binary(), binary(), loc()} | {error, loc(), ai_jinja_reason()}.
string_lit(<<Q, Rest/binary>>, Loc) ->
    str(Rest, Q, adv(Loc, 1), []).

str(<<>>, _Q, Loc, _Acc) ->
    {error, Loc, {unclosed_block, string, Loc}};
str(<<Q, Rest/binary>>, Q, Loc, Acc) ->
    {ok, unicode:characters_to_binary(lists:reverse(Acc), utf8),
     Rest, adv(Loc, 1)};
str(<<$\\, Rest/binary>>, Q, Loc, Acc) ->
    escape(Rest, Q, adv(Loc, 1), Acc);
str(<<$\n, Rest/binary>>, Q, {L, _}, Acc) ->
    str(Rest, Q, {L + 1, 1}, [$\n | Acc]);
%% One codepoint at a time. Taking a fixed-size prefix and decoding that would
%% split a multi-byte character whenever it straddled the boundary, which is
%% how `'中文'' came out as mojibake the first time round.
str(<<C/utf8, Rest/binary>>, Q, Loc, Acc) ->
    str(Rest, Q, adv(Loc, 1), [C | Acc]);
str(<<C, Rest/binary>>, Q, Loc, Acc) ->
    %% Not valid UTF-8; keep the byte so the diagnostic still shows the text.
    str(Rest, Q, adv(Loc, 1), [C | Acc]).

%% Python's rule: an unrecognised escape keeps the backslash.
escape(<<$n, R/binary>>, Q, Loc, Acc)  -> str(R, Q, adv(Loc, 1), [$\n | Acc]);
escape(<<$t, R/binary>>, Q, Loc, Acc)  -> str(R, Q, adv(Loc, 1), [$\t | Acc]);
escape(<<$r, R/binary>>, Q, Loc, Acc)  -> str(R, Q, adv(Loc, 1), [$\r | Acc]);
escape(<<$\\, R/binary>>, Q, Loc, Acc) -> str(R, Q, adv(Loc, 1), [$\\ | Acc]);
escape(<<$", R/binary>>, Q, Loc, Acc)  -> str(R, Q, adv(Loc, 1), [$" | Acc]);
escape(<<$', R/binary>>, Q, Loc, Acc)  -> str(R, Q, adv(Loc, 1), [$' | Acc]);
escape(<<$0, R/binary>>, Q, Loc, Acc)  -> str(R, Q, adv(Loc, 1), [0 | Acc]);
escape(<<$x, A, B, R/binary>>, Q, Loc, Acc) ->
    case hex([A, B]) of
        {ok, V} -> str(R, Q, adv(Loc, 3), [V | Acc]);
        error   -> str(<<$x, A, B, R/binary>>, Q, Loc, [$\\ | Acc])
    end;
escape(<<$u, A, B, C, D, R/binary>>, Q, Loc, Acc) ->
    case hex([A, B, C, D]) of
        {ok, V} -> str(R, Q, adv(Loc, 5), [V | Acc]);
        error   -> str(<<$u, A, B, C, D, R/binary>>, Q, Loc, [$\\ | Acc])
    end;
escape(Bin, Q, Loc, Acc) ->
    str(Bin, Q, Loc, [$\\ | Acc]).

hex(Chars) ->
    try list_to_integer(Chars, 16) of V -> {ok, V}
    catch _:_ -> error end.

%%%===================================================================
%%% Operators
%%%===================================================================

%% Longest match. Getting the order wrong here produces parse errors that are
%% very hard to trace back: `a // b' lexed as two divisions, `a == b' as an
%% assignment followed by an equals.
-spec operator(binary()) -> {ok, atom(), pos_integer(), binary()} | error.
operator(<<"**", R/binary>>) -> {ok, '**', 2, R};
operator(<<"//", R/binary>>) -> {ok, '//', 2, R};
operator(<<"==", R/binary>>) -> {ok, '==', 2, R};
operator(<<"!=", R/binary>>) -> {ok, '!=', 2, R};
operator(<<"<=", R/binary>>) -> {ok, '<=', 2, R};
operator(<<">=", R/binary>>) -> {ok, '>=', 2, R};
operator(<<"+",  R/binary>>) -> {ok, '+',  1, R};
operator(<<"-",  R/binary>>) -> {ok, '-',  1, R};
operator(<<"*",  R/binary>>) -> {ok, '*',  1, R};
operator(<<"/",  R/binary>>) -> {ok, '/',  1, R};
operator(<<"%",  R/binary>>) -> {ok, '%',  1, R};
operator(<<"~",  R/binary>>) -> {ok, '~',  1, R};
operator(<<"<",  R/binary>>) -> {ok, '<',  1, R};
operator(<<">",  R/binary>>) -> {ok, '>',  1, R};
operator(<<"=",  R/binary>>) -> {ok, '=',  1, R};
operator(<<".",  R/binary>>) -> {ok, '.',  1, R};
operator(<<",",  R/binary>>) -> {ok, ',',  1, R};
operator(<<":",  R/binary>>) -> {ok, ':',  1, R};
operator(<<"|",  R/binary>>) -> {ok, '|',  1, R};
operator(_)                  -> error.

%%%===================================================================
%%% Positions
%%%===================================================================

-spec adv(loc(), non_neg_integer()) -> loc().
adv({L, C}, N) -> {L, C + N}.
