%%%-------------------------------------------------------------------
%%% @doc The builtin filter library.
%%%
%%% Every filter is `f(Value, Args)' where Args is a map keyed by parameter
%%% name. The compiler maps positional arguments onto those names using
%%% params/0 below, so `x|replace("a","b")' and `x|replace(new="b",old="a")'
%%% arrive identically.
%%%
%%% == The @safe annotation is not decoration ==
%%%
%%% Each filter is tagged with how it treats the autoescape marker:
%%%
%%%   @safe transparent  a safe input stays safe
%%%   @safe escaping     an unsafe input is escaped, the result is safe
%%%   @safe generating   the result is always safe, the filter guarantees it
%%%   @safe none         the result is not text
%%%
%%% ai_jinja_escape_tests checks that every exported filter carries one of
%%% those four. Getting a tag wrong is an XSS, which is why it is asserted
%%% mechanically rather than left to review.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_filters).

-export([filters/0, params/0]).

-export([upper/2, lower/2, capitalize/2, title/2, trim/2, striptags/2,
         truncate/2, wordwrap/2, wordcount/2, center/2, indent/2, replace/2,
         format/2, urlencode/2, urlize/2, escape/2, e/2, forceescape/2,
         safe/2, string/2]).
-export([first/2, last/2, length/2, count/2, list/2, join/2, reverse/2,
         sort/2, sum/2, min/2, max/2, unique/2, slice/2, batch/2, groupby/2,
         map/2, select/2, reject/2, selectattr/2, rejectattr/2, random/2]).
-export([dictsort/2, items/2, attr/2, tojson/2]).
-export([abs/2, round/2, int/2, float/2, filesizeformat/2]).
-export([default/2, d/2, pprint/2]).

-define(TAG, ai_jinja).

%%%===================================================================
%%% Registry
%%%===================================================================

-spec filters() -> #{atom() => {module(), atom()}}.
filters() ->
    maps:from_list([{N, {?MODULE, N}} || N <- names()]).

names() ->
    [upper, lower, capitalize, title, trim, striptags, truncate, wordwrap,
     wordcount, center, indent, replace, format, urlencode, urlize, escape, e,
     forceescape, safe, string,
     first, last, length, count, list, join, reverse, sort, sum, min, max,
     unique, slice, batch, groupby, map, select, reject, selectattr,
     rejectattr, random,
     dictsort, items, attr, tojson,
     abs, round, int, float, filesizeformat,
     default, d, pprint].

%% Positional parameter names, in order. A filter absent from this table takes
%% no positional arguments.
-spec params() -> #{atom() => [atom()]}.
params() ->
    #{trim           => [chars],
      truncate       => [length, killwords, 'end', leeway],
      wordwrap       => [width, break_long_words, wrapstring],
      center         => [width],
      indent         => [width, first, blank],
      replace        => [old, new, count],
      urlize         => [trim_url_limit],
      join           => [d, attribute],
      sort           => [reverse, case_sensitive, attribute],
      unique         => [case_sensitive, attribute],
      slice          => [slices, fill_with],
      batch          => [linecount, fill_with],
      groupby        => [attribute, default],
      map            => [name],
      select         => [name],
      reject         => [name],
      selectattr     => [attribute, name],
      rejectattr     => [attribute, name],
      sum            => [attribute, start],
      min            => [case_sensitive, attribute],
      max            => [case_sensitive, attribute],
      dictsort       => [case_sensitive, by, reverse],
      attr           => [name],
      tojson         => [indent],
      round          => [precision, method],
      int            => [default, base],
      float          => [default],
      filesizeformat => [binary],
      default        => [default_value, boolean],
      d              => [default_value, boolean]}.

%%%===================================================================
%%% Strings
%%%===================================================================

%% @safe transparent
-spec upper(term(), map()) -> term().
upper(V, _A)      -> keep_safe(V, fun(B) -> uc(B) end).
%% @safe transparent
-spec lower(term(), map()) -> term().
lower(V, _A)      -> keep_safe(V, fun(B) -> lc(B) end).

%% @safe transparent
-spec capitalize(term(), map()) -> term().
capitalize(V, _A) ->
    keep_safe(V, fun(<<>>) -> <<>>;
                    (B) ->
                         {H, T} = split_first(B),
                         <<(uc(H))/binary, (lc(T))/binary>>
                 end).

%% @safe transparent
%%
%% The delimiter set is the reference implementation's -- runs of `-',
%% whitespace, `(', `{', `[' or `<' -- and each surviving chunk is
%% capitalised, not word-cased. That is why `<b>x</b>|title' becomes
%% `<B>X</b>': the chunk after `<' starts with `b', the one after `</' starts
%% with `/'.
-spec title(term(), map()) -> term().
title(V, _A) ->
    keep_safe(V, fun(B) ->
                         iolist_to_binary(
                           [cap_chunk(C) || C <- split_delims(B)])
                 end).

cap_chunk({delim, D}) -> D;
cap_chunk({word, <<>>}) -> <<>>;
cap_chunk({word, W}) ->
    {H, T} = split_first(W),
    <<(uc(H))/binary, (lc(T))/binary>>.

is_delim(C) -> lists:member(C, [$\s, $\t, $\n, $\r, $-, $(, ${, $[, $<]).

%% Split into alternating word and delimiter runs, keeping both, so that
%% rejoining reproduces the input exactly.
split_delims(B) -> lists:reverse(split_delims(B, 0, 0, [])).

split_delims(B, Start, Pos, Acc) when Pos < byte_size(B) ->
    Delim = is_delim(binary:at(B, Pos)),
    Prev = Pos > Start andalso is_delim(binary:at(B, Pos - 1)),
    case Pos > Start andalso Delim =/= Prev of
        true  -> split_delims(B, Pos, Pos + 1,
                              [chunk(B, Start, Pos, Prev) | Acc]);
        false -> split_delims(B, Start, Pos + 1, Acc)
    end;
split_delims(B, Start, _Pos, Acc) ->
    WasDelim = Start < byte_size(B) andalso is_delim(binary:at(B, Start)),
    [chunk(B, Start, byte_size(B), WasDelim) | Acc].

chunk(B, Start, Stop, true)  -> {delim, binary:part(B, Start, Stop - Start)};
chunk(B, Start, Stop, false) -> {word, binary:part(B, Start, Stop - Start)}.

%% @safe transparent
-spec trim(term(), map()) -> term().
trim(V, A) ->
    Chars = case A of
                #{chars := C} when C =/= undefined -> chars_of(C);
                _ -> " \t\r\n"
            end,
    keep_safe(V, fun(B) -> trim_both(B, Chars) end).

%% @safe escaping
-spec striptags(term(), map()) -> term().
striptags(V, _A) ->
    B = to_bin(ai_jinja_rt:unsafe(V)),
    Text = strip_tags(B, <<>>, false),
    ai_jinja_rt:safe(collapse_ws(Text)).

strip_tags(<<>>, Acc, _In)                -> Acc;
strip_tags(<<$<, R/binary>>, Acc, _In)    -> strip_tags(R, Acc, true);
strip_tags(<<$>, R/binary>>, Acc, _In)    -> strip_tags(R, Acc, false);
strip_tags(<<C, R/binary>>, Acc, false)   -> strip_tags(R, <<Acc/binary, C>>, false);
strip_tags(<<_, R/binary>>, Acc, true)    -> strip_tags(R, Acc, true).

collapse_ws(B) ->
    Parts = [P || P <- binary:split(B, [<<" ">>, <<"\t">>, <<"\n">>, <<"\r">>],
                                    [global]), P =/= <<>>],
    iolist_to_binary(lists:join(<<" ">>, Parts)).

%% @safe transparent
-spec truncate(term(), map()) -> term().
truncate(V, A) ->
    Len     = int_arg(A, length, 255),
    Kill    = bool_arg(A, killwords, false),
    End     = bin_arg(A, 'end', <<"...">>),
    Leeway  = int_arg(A, leeway, 5),
    keep_safe(V, fun(B) -> do_truncate(B, Len, Kill, End, Leeway) end).

do_truncate(B, Len, Kill, End, Leeway) ->
    case chars(B) of
        Cs when erlang:length(Cs) =< Len + Leeway -> B;
        Cs ->
            Cut = bin(lists:sublist(Cs, erlang:max(Len - erlang:length(chars(End)), 0))),
            Head = case Kill of
                       true  -> Cut;
                       false -> rsplit_space(Cut)
                   end,
            <<Head/binary, End/binary>>
    end.

rsplit_space(B) ->
    case binary:matches(B, <<" ">>) of
        []      -> B;
        Matches -> {P, 1} = lists:last(Matches), binary:part(B, 0, P)
    end.

%% @safe transparent
-spec wordwrap(term(), map()) -> term().
wordwrap(V, A) ->
    Width = int_arg(A, width, 79),
    Wrap  = bin_arg(A, wrapstring, <<"\n">>),
    keep_safe(V, fun(B) -> do_wrap(B, Width, Wrap) end).

do_wrap(B, Width, Wrap) ->
    Lines = [wrap_line(L, Width) || L <- binary:split(B, <<"\n">>, [global])],
    iolist_to_binary(lists:join(Wrap, lists:append(Lines))).

wrap_line(L, Width) ->
    Words = [W || W <- binary:split(L, <<" ">>, [global])],
    lists:reverse(fold_words(Words, Width, <<>>, [])).

fold_words([], _W, <<>>, Acc)   -> Acc;
fold_words([], _W, Cur, Acc)    -> [Cur | Acc];
fold_words([Word | Rest], W, <<>>, Acc) ->
    fold_words(Rest, W, Word, Acc);
fold_words([Word | Rest], W, Cur, Acc) ->
    Candidate = <<Cur/binary, " ", Word/binary>>,
    case erlang:length(chars(Candidate)) =< W of
        true  -> fold_words(Rest, W, Candidate, Acc);
        false -> fold_words(Rest, W, Word, [Cur | Acc])
    end.

%% @safe none
-spec wordcount(term(), map()) -> term().
wordcount(V, _A) ->
    erlang:length([W || W <- binary:split(to_bin(ai_jinja_rt:unsafe(V)),
                                          [<<" ">>, <<"\n">>, <<"\t">>, <<"\r">>],
                                          [global]), W =/= <<>>]).

%% @safe transparent
-spec center(term(), map()) -> term().
center(V, A) ->
    Width = int_arg(A, width, 80),
    keep_safe(V, fun(B) -> do_center(B, Width) end).

do_center(B, Width) ->
    N = erlang:length(chars(B)),
    case Width =< N of
        true  -> B;
        false ->
            Total = Width - N,
            L = Total div 2,
            R = Total - L,
            <<(spaces(L))/binary, B/binary, (spaces(R))/binary>>
    end.

spaces(N) -> binary:copy(<<" ">>, erlang:max(N, 0)).

%% @safe transparent
-spec indent(term(), map()) -> term().
indent(V, A) ->
    Width = int_arg(A, width, 4),
    First = bool_arg(A, first, false),
    Blank = bool_arg(A, blank, false),
    Pad   = spaces(Width),
    keep_safe(V, fun(B) -> do_indent(B, Pad, First, Blank) end).

do_indent(B, Pad, First, Blank) ->
    [H | T] = binary:split(B, <<"\n">>, [global]),
    Rest = [indent_line(L, Pad, Blank) || L <- T],
    Head = case First of true -> <<Pad/binary, H/binary>>; false -> H end,
    iolist_to_binary(lists:join(<<"\n">>, [Head | Rest])).

indent_line(<<>>, _Pad, false) -> <<>>;
indent_line(L, Pad, _Blank)    -> <<Pad/binary, L/binary>>.

%% @safe transparent
-spec replace(term(), map()) -> term().
replace(V, A) ->
    Old   = bin_arg(A, old, <<>>),
    New   = bin_arg(A, new, <<>>),
    Count = int_arg(A, count, -1),
    keep_safe(V, fun(B) -> do_replace(B, Old, New, Count) end).

do_replace(B, <<>>, _New, _Count) -> B;
do_replace(B, Old, New, Count) when Count < 0 ->
    binary:replace(B, Old, New, [global]);
do_replace(B, Old, New, Count) ->
    replace_n(B, Old, New, Count, <<>>).

replace_n(B, _Old, _New, 0, Acc) -> <<Acc/binary, B/binary>>;
replace_n(B, Old, New, N, Acc) ->
    case binary:match(B, Old) of
        nomatch -> <<Acc/binary, B/binary>>;
        {P, L}  ->
            Head = binary:part(B, 0, P),
            Tail = binary:part(B, P + L, byte_size(B) - P - L),
            replace_n(Tail, Old, New, N - 1, <<Acc/binary, Head/binary, New/binary>>)
    end.

%% @safe escaping
-spec format(term(), map()) -> term().
format(V, A) ->
    Fmt = to_bin(ai_jinja_rt:unsafe(V)),
    Args = maps:get('$positional', A, []),
    ai_jinja_rt:safe(printf(Fmt, Args, <<>>)).

printf(<<>>, _Args, Acc) -> Acc;
printf(<<"%%", R/binary>>, Args, Acc) -> printf(R, Args, <<Acc/binary, $%>>);
printf(<<$%, C, R/binary>>, [A | Args], Acc) when C =:= $s; C =:= $d; C =:= $r;
                                                  C =:= $f; C =:= $i ->
    printf(R, Args, <<Acc/binary, (fmt_one(C, A))/binary>>);
printf(<<C, R/binary>>, Args, Acc) -> printf(R, Args, <<Acc/binary, C>>).

fmt_one($s, A) -> ai_jinja_rt:to_binary(A);
fmt_one($r, A) -> ai_jinja_rt:repr(A);
fmt_one($d, A) -> integer_to_binary(trunc(num(A)));
fmt_one($i, A) -> integer_to_binary(trunc(num(A)));
fmt_one($f, A) -> erlang:float_to_binary(erlang:float(num(A)), [{decimals, 6}]).

num(A) when is_number(A) -> A;
num(A)                   -> erlang:error({?TAG, {unsupported_operands, format, A, A}}).

%% @safe generating
-spec urlencode(term(), map()) -> term().
urlencode(V, _A) ->
    B = ai_jinja_rt:unsafe(V),
    ai_jinja_rt:safe(
      case B of
          M when is_map(M) ->
              iolist_to_binary(
                lists:join(<<"&">>,
                           [<<(pct(atom_to_binary(K, utf8)))/binary, "=",
                              (pct(ai_jinja_rt:to_binary(Val)))/binary>>
                            || {K, Val} <- lists:sort(maps:to_list(M))]));
          _ ->
              pct(to_bin(B))
      end).

pct(B) -> iolist_to_binary([pct_char(C) || <<C>> <= B]).

pct_char(C) when C >= $a, C =< $z -> <<C>>;
pct_char(C) when C >= $A, C =< $Z -> <<C>>;
pct_char(C) when C >= $0, C =< $9 -> <<C>>;
pct_char(C) when C =:= $-; C =:= $_; C =:= $.; C =:= $~; C =:= $/ -> <<C>>;
pct_char(C) -> iolist_to_binary(io_lib:format("%~2.16.0B", [C])).

%% @safe generating
-spec urlize(term(), map()) -> term().
urlize(V, _A) ->
    B = to_bin(ai_jinja_rt:unsafe(V)),
    Words = binary:split(B, <<" ">>, [global]),
    ai_jinja_rt:safe(
      iolist_to_binary(lists:join(<<" ">>, [urlize_word(W) || W <- Words]))).

urlize_word(W) ->
    case is_url(W) of
        true  -> Esc = ai_html_escape:escape(W, ?TAG),
                 [<<"<a href=\"">>, Esc, <<"\" rel=\"noopener\">">>, Esc, <<"</a>">>];
        false -> ai_html_escape:escape(W, ?TAG)
    end.

is_url(<<"http://", _/binary>>)  -> true;
is_url(<<"https://", _/binary>>) -> true;
is_url(_)                        -> false.

%% @safe generating
-spec escape(term(), map()) -> term().
escape(V, _A) -> ai_jinja_rt:safe(iolist_to_binary(ai_jinja_rt:escape(V))).
%% @safe generating
-spec e(term(), map()) -> term().
e(V, A)       -> escape(V, A).

%% @safe generating
-spec forceescape(term(), map()) -> term().
forceescape(V, _A) ->
    %% ai_html_escape:escape/2 returns iodata, and ai_jinja_rt:safe/1 renders
    %% whatever it is handed -- a raw list would come out as a Python list
    %% repr, so it is flattened here.
    ai_jinja_rt:safe(
      iolist_to_binary(
        ai_html_escape:escape(ai_jinja_rt:to_binary(ai_jinja_rt:unsafe(V)),
                              ?TAG, <<"&#34;">>))).

%% @safe generating
-spec safe(term(), map()) -> term().
safe(V, _A) -> ai_jinja_rt:safe(V).

%% @safe escaping
-spec string(term(), map()) -> term().
string(V, _A) -> ai_jinja_rt:to_binary(ai_jinja_rt:unsafe(V)).

%% @safe escaping
-spec pprint(term(), map()) -> term().
pprint(V, _A) -> ai_jinja_rt:repr(ai_jinja_rt:unsafe(V)).

%%%===================================================================
%%% Sequences
%%%===================================================================

%% @safe none
-spec first(term(), map()) -> term().
first(V, _A) -> case seq(V) of [] -> undefined; [H | _] -> H end.
%% @safe none
-spec last(term(), map()) -> term().
last(V, _A)  -> case seq(V) of [] -> undefined; L -> lists:last(L) end.

%% @safe none
-spec length(term(), map()) -> term().
length(V, _A) -> len(V).
%% @safe none
-spec count(term(), map()) -> term().
count(V, A)   -> length(V, A).

len(V) when is_binary(V) -> erlang:length(chars(V));
len({safe, D})           -> len(iolist_to_binary(D));
len(V) when is_map(V)    -> map_size(V);
len(undefined)           -> undefined;
len(V)                   -> erlang:length(seq(V)).

%% @safe none
-spec list(term(), map()) -> term().
list(V, _A) when is_binary(V) -> [<<C/utf8>> || <<C/utf8>> <= V];
list(V, _A)                   -> seq(V).

%% @safe escaping
-spec join(term(), map()) -> term().
join(V, A) ->
    Sep = bin_arg(A, d, <<>>),
    Items = case A of
                #{attribute := At} when At =/= undefined ->
                    [ai_jinja_rt:attr(E, atomize(At)) || E <- seq(V)];
                _ -> seq(V)
            end,
    Parts = [ai_jinja_rt:escape(E) || E <- Items],
    ai_jinja_rt:safe(iolist_to_binary(lists:join(ai_jinja_rt:escape(Sep), Parts))).

%% @safe transparent
-spec reverse(term(), map()) -> term().
reverse(V, _A) when is_binary(V) ->
    bin(lists:reverse(chars(V)));
reverse({safe, D}, A) ->
    ai_jinja_rt:safe(reverse(iolist_to_binary(D), A));
reverse(V, _A) ->
    lists:reverse(seq(V)).

%% @safe none
-spec sort(term(), map()) -> term().
sort(V, A) ->
    Rev  = bool_arg(A, reverse, false),
    CS   = bool_arg(A, case_sensitive, false),
    Att  = maps:get(attribute, A, undefined),
    Key  = fun(E) -> sort_key(E, Att, CS) end,
    Sorted = lists:sort(fun(X, Y) -> Key(X) =< Key(Y) end, seq(V)),
    case Rev of true -> lists:reverse(Sorted); false -> Sorted end.

sort_key(E, undefined, CS) -> fold_case(E, CS);
sort_key(E, Att, CS)       -> fold_case(ai_jinja_rt:attr(E, atomize(Att)), CS).

fold_case(B, false) when is_binary(B) -> lc(B);
fold_case(V, _)                        -> V.

%% @safe none
-spec sum(term(), map()) -> term().
sum(V, A) ->
    Items = case A of
                #{attribute := At} when At =/= undefined ->
                    [ai_jinja_rt:attr(E, atomize(At)) || E <- seq(V)];
                _ -> seq(V)
            end,
    lists:foldl(fun(E, Acc) -> ai_jinja_rt:add(Acc, E) end,
                maps:get(start, A, 0), Items).

%% @safe none
-spec min(term(), map()) -> term().
min(V, A) -> extreme(V, A, fun(X, Y) -> X =< Y end).
%% @safe none
-spec max(term(), map()) -> term().
max(V, A) -> extreme(V, A, fun(X, Y) -> X >= Y end).

extreme(V, A, Better) ->
    CS  = bool_arg(A, case_sensitive, false),
    Att = maps:get(attribute, A, undefined),
    case seq(V) of
        [] -> undefined;
        [H | T] ->
            lists:foldl(fun(E, Acc) ->
                                case Better(sort_key(E, Att, CS),
                                            sort_key(Acc, Att, CS)) of
                                    true  -> E;
                                    false -> Acc
                                end
                        end, H, T)
    end.

%% @safe none
-spec unique(term(), map()) -> term().
unique(V, A) ->
    CS  = bool_arg(A, case_sensitive, false),
    Att = maps:get(attribute, A, undefined),
    uniq(seq(V), Att, CS, [], []).

uniq([], _Att, _CS, _Seen, Acc) -> lists:reverse(Acc);
uniq([E | Rest], Att, CS, Seen, Acc) ->
    K = sort_key(E, Att, CS),
    case lists:member(K, Seen) of
        true  -> uniq(Rest, Att, CS, Seen, Acc);
        false -> uniq(Rest, Att, CS, [K | Seen], [E | Acc])
    end.

%% @safe none
-spec slice(term(), map()) -> term().
slice(V, A) ->
    N    = int_arg(A, slices, 1),
    Fill = maps:get(fill_with, A, undefined),
    L = seq(V),
    Len = erlang:length(L),
    Per = Len div N,
    Extra = Len rem N,
    take_slices(L, N, Per, Extra, Fill, []).

take_slices(_L, 0, _Per, _Extra, _Fill, Acc) -> lists:reverse(Acc);
take_slices(L, N, Per, Extra, Fill, Acc) ->
    Size = Per + (case Extra > 0 of true -> 1; false -> 0 end),
    {Chunk0, Rest} = lists:split(erlang:min(Size, erlang:length(L)), L),
    Chunk = case {Extra, Fill} of
                {0, F} when F =/= undefined, Per > 0 -> Chunk0 ++ [F];
                _ -> Chunk0
            end,
    take_slices(Rest, N - 1, Per, erlang:max(Extra - 1, 0), Fill, [Chunk | Acc]).

%% @safe none
-spec batch(term(), map()) -> term().
batch(V, A) ->
    N    = int_arg(A, linecount, 1),
    Fill = maps:get(fill_with, A, undefined),
    batches(seq(V), N, Fill, []).

batches([], _N, _Fill, Acc) -> lists:reverse(Acc);
batches(L, N, Fill, Acc) ->
    {Chunk0, Rest} = lists:split(erlang:min(N, erlang:length(L)), L),
    Chunk = case Fill =/= undefined andalso erlang:length(Chunk0) < N of
                true  -> Chunk0 ++ lists:duplicate(N - erlang:length(Chunk0), Fill);
                false -> Chunk0
            end,
    batches(Rest, N, Fill, [Chunk | Acc]).

%% @safe none
-spec groupby(term(), map()) -> term().
groupby(V, A) ->
    Att = atomize(maps:get(attribute, A, undefined)),
    Default = maps:get(default, A, undefined),
    Pairs = [{key_or(ai_jinja_rt:attr(E, Att), Default), E} || E <- seq(V)],
    Keys = lists:usort([K || {K, _} <- Pairs]),
    [{K, [E || {K2, E} <- Pairs, K2 =:= K]} || K <- Keys].

key_or(undefined, Default) when Default =/= undefined -> Default;
key_or(K, _Default)                                   -> K.

%% @safe none
-spec map(term(), map()) -> term().
map(V, A) ->
    case A of
        #{attribute := Att} when Att =/= undefined ->
            [ai_jinja_rt:attr(E, atomize(Att)) || E <- seq(V)];
        #{name := Name} when Name =/= undefined ->
            Extra = maps:get('$positional', A, []),
            [ai_jinja_rt:apply_named(filter, atomize(Name), E, Extra)
             || E <- seq(V)];
        _ ->
            seq(V)
    end.

%% @safe none
-spec select(term(), map()) -> term().
select(V, A)     -> [E || E <- seq(V), test_of(E, A)].
%% @safe none
-spec reject(term(), map()) -> term().
reject(V, A)     -> [E || E <- seq(V), not test_of(E, A)].
%% @safe none
-spec selectattr(term(), map()) -> term().
selectattr(V, A) -> [E || E <- seq(V), attr_test(E, A)].
%% @safe none
-spec rejectattr(term(), map()) -> term().
rejectattr(V, A) -> [E || E <- seq(V), not attr_test(E, A)].

test_of(E, #{name := Name} = A) when Name =/= undefined ->
    Extra = maps:get('$positional', A, []),
    ai_jinja_rt:truthy(ai_jinja_rt:apply_named(test, atomize(Name), E, Extra));
test_of(E, _A) ->
    ai_jinja_rt:truthy(E).

attr_test(E, #{attribute := Att} = A) ->
    V = ai_jinja_rt:attr(E, atomize(Att)),
    case A of
        #{name := Name} when Name =/= undefined ->
            Extra = maps:get('$positional', A, []),
            ai_jinja_rt:truthy(ai_jinja_rt:apply_named(test, atomize(Name), V, Extra));
        _ ->
            ai_jinja_rt:truthy(V)
    end.

%% @safe none
%%
%% The only impure filter in the library.
-spec random(term(), map()) -> term().
random(V, _A) ->
    case seq(V) of
        []   -> undefined;
        L    -> lists:nth(rand:uniform(erlang:length(L)), L)
    end.

%%%===================================================================
%%% Mappings
%%%===================================================================

%% @safe none
-spec dictsort(term(), map()) -> term().
dictsort(V, A) ->
    By  = maps:get(by, A, <<"key">>),
    Rev = bool_arg(A, reverse, false),
    CS  = bool_arg(A, case_sensitive, false),
    Pairs = [{K, Val} || {K, Val} <- maps:to_list(as_map(V))],
    Key = case By of
              <<"value">> -> fun({_, Val}) -> fold_case(Val, CS) end;
              _           -> fun({K, _}) -> fold_case(atom_to_binary(K, utf8), CS) end
          end,
    Sorted = lists:sort(fun(X, Y) -> Key(X) =< Key(Y) end, Pairs),
    Out = [{atom_to_binary(K, utf8), Val} || {K, Val} <- Sorted],
    case Rev of true -> lists:reverse(Out); false -> Out end.

%% @safe none
-spec items(term(), map()) -> term().
items(V, _A) ->
    [{atom_to_binary(K, utf8), Val} || {K, Val} <- ai_jinja_rt:items(as_map(V))].

%% @safe none
%%
%% Attribute access, not item access -- exactly like the reference
%% implementation, where `{'a': 1}|attr('a')' is undefined because a dict has
%% no attribute `a'. Only a namespace() responds.
-spec attr(term(), map()) -> term().
attr(V, A) ->
    case ai_jinja_rt:unsafe(V) of
        {ns, M} -> maps:get(atomize(maps:get(name, A, undefined)), M, undefined);
        _       -> undefined
    end.

%% @safe generating
%%
%% The output goes inside HTML, so `<', `>', `&' and `'' are escaped as \\uXXXX
%% rather than left for the HTML escaper, which would corrupt the JSON. A
%% `</script>' in a value is the case that matters.
-spec tojson(term(), map()) -> term().
tojson(V, _A) ->
    ai_jinja_rt:safe(json_encode(ai_jinja_rt:unsafe(V))).

json_encode(undefined)            -> <<"null">>;
json_encode(null)                 -> <<"null">>;
json_encode(true)                 -> <<"true">>;
json_encode(false)                -> <<"false">>;
json_encode(I) when is_integer(I) -> integer_to_binary(I);
json_encode(F) when is_float(F)   -> erlang:float_to_binary(F, [short]);
json_encode(B) when is_binary(B)  -> json_string(B);
json_encode(A) when is_atom(A)    -> json_string(atom_to_binary(A, utf8));
json_encode(L) when is_list(L) ->
    <<"[", (join_bins([json_encode(E) || E <- L]))/binary, "]">>;
json_encode(T) when is_tuple(T) -> json_encode(tuple_to_list(T));
json_encode(M) when is_map(M) ->
    Parts = [<<(json_string(atom_to_binary(K, utf8)))/binary, ": ",
               (json_encode(Val))/binary>>
             || {K, Val} <- lists:sort(maps:to_list(M))],
    <<"{", (join_bins(Parts))/binary, "}">>.

join_bins(Parts) -> iolist_to_binary(lists:join(<<", ">>, Parts)).

json_string(B) ->
    <<$", (iolist_to_binary([json_char(C) || <<C/utf8>> <= B]))/binary, $">>.

json_char($<)  -> <<"\\u003c">>;
json_char($>)  -> <<"\\u003e">>;
json_char($&)  -> <<"\\u0026">>;
json_char($')  -> <<"\\u0027">>;
json_char($")  -> <<"\\\"">>;
json_char($\\) -> <<"\\\\">>;
json_char($\n) -> <<"\\n">>;
json_char($\t) -> <<"\\t">>;
json_char($\r) -> <<"\\r">>;
json_char(C) when C < 16#20 -> iolist_to_binary(io_lib:format("\\u~4.16.0b", [C]));
json_char(C)   -> <<C/utf8>>.

%%%===================================================================
%%% Numbers
%%%===================================================================

%% @safe none
-spec abs(term(), map()) -> term().
abs(V, _A) when is_number(V) -> erlang:abs(V);
abs(V, _A)                   -> erlang:abs(numeric(V, 0)).

%% @safe none
%%
%% Python's round: half to even, on the exact binary value. `0.5|round' is
%% 0.0, not 1.0. Rounding the shortest decimal representation instead would
%% get `2.345|round(2)' wrong, because 2.345 is stored slightly above the tie.
-spec round(term(), map()) -> term().
round(V, A) ->
    P = int_arg(A, precision, 0),
    Method = maps:get(method, A, <<"common">>),
    F = erlang:float(numeric(V, 0)),
    case Method of
        <<"ceil">>  -> scale_apply(F, P, fun(X) -> math:ceil(X) end);
        <<"floor">> -> scale_apply(F, P, fun(X) -> math:floor(X) end);
        _           -> round_half_even(F, P)
    end.

scale_apply(F, P, Fun) ->
    S = math:pow(10, P),
    Fun(F * S) / S.

round_half_even(F, P) ->
    Exact = erlang:float_to_binary(F, [{decimals, 20}]),
    binary_to_float(round_decimal(Exact, P)).

%% Round a fixed-point decimal string at P places, half to even.
round_decimal(Bin, P) ->
    {Sign, Rest} = case Bin of
                       <<$-, R/binary>> -> {<<"-">>, R};
                       _                -> {<<>>, Bin}
                   end,
    [Int, Frac] = binary:split(Rest, <<".">>),
    Digits = <<Int/binary, Frac/binary>>,
    Point  = byte_size(Int),
    Keep   = Point + erlang:max(P, 0),
    Head = binary:part(Digits, 0, Keep),
    Tail = binary:part(Digits, Keep, byte_size(Digits) - Keep),
    Rounded = case round_up(Head, Tail) of
                  true  -> bump(Head);
                  false -> Head
              end,
    rebuild(Sign, Rounded, Point + (byte_size(Rounded) - byte_size(Head)),
            erlang:max(P, 0)).

round_up(_Head, <<>>) -> false;
round_up(Head, <<D, Rest/binary>>) when D > $5 -> _ = Head, _ = Rest, true;
round_up(_Head, <<D, _/binary>>) when D < $5   -> false;
round_up(Head, <<$5, Rest/binary>>) ->
    case binary:match(Rest, [<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>,
                             <<"6">>, <<"7">>, <<"8">>, <<"9">>]) of
        nomatch ->
            %% Exact tie: go to the even neighbour.
            case Head of
                <<>> -> false;
                _    -> (binary:last(Head) - $0) band 1 =:= 1
            end;
        _ -> true
    end.

bump(Head) -> integer_to_binary(binary_to_integer(<<"0", Head/binary>>) + 1).

rebuild(Sign, Digits, Point, P) ->
    Padded = case byte_size(Digits) < Point + P of
                 true  -> <<Digits/binary,
                            (binary:copy(<<"0">>, Point + P - byte_size(Digits)))/binary>>;
                 false -> Digits
             end,
    Int  = binary:part(Padded, 0, Point),
    Frac = binary:part(Padded, Point, byte_size(Padded) - Point),
    Frac1 = case Frac of <<>> -> <<"0">>; _ -> Frac end,
    <<Sign/binary, Int/binary, ".", Frac1/binary>>.

%% @safe none
-spec int(term(), map()) -> term().
int(V, A) ->
    Default = maps:get(default, A, 0),
    Base    = int_arg(A, base, 10),
    case ai_jinja_rt:unsafe(V) of
        I when is_integer(I) -> I;
        F when is_float(F)   -> trunc(F);
        B when is_binary(B)  -> parse_int(B, Base, Default);
        true                 -> 1;
        false                -> 0;
        _                    -> Default
    end.

parse_int(B, Base, Default) ->
    try binary_to_integer(B, Base)
    catch error:badarg ->
            try trunc(binary_to_float(B))
            catch error:badarg -> Default end
    end.

%% @safe none
-spec float(term(), map()) -> term().
float(V, A) ->
    Default = maps:get(default, A, 0.0),
    case ai_jinja_rt:unsafe(V) of
        F when is_float(F)   -> F;
        I when is_integer(I) -> erlang:float(I);
        B when is_binary(B)  -> parse_float(B, Default);
        _                    -> Default
    end.

parse_float(B, Default) ->
    try binary_to_float(B)
    catch error:badarg ->
            try erlang:float(binary_to_integer(B))
            catch error:badarg -> Default end
    end.

%% @safe none
-spec filesizeformat(term(), map()) -> term().
filesizeformat(V, A) ->
    Binary = bool_arg(A, binary, false),
    Bytes = numeric(V, 0),
    {Base, Units} = case Binary of
                        true  -> {1024, [<<"KiB">>, <<"MiB">>, <<"GiB">>,
                                         <<"TiB">>, <<"PiB">>]};
                        false -> {1000, [<<"kB">>, <<"MB">>, <<"GB">>,
                                         <<"TB">>, <<"PB">>]}
                    end,
    case erlang:abs(Bytes) < Base of
        true  -> unit_bytes(Bytes);
        false -> scale(erlang:float(Bytes), Base, Units)
    end.

unit_bytes(1)     -> <<"1 Byte">>;
unit_bytes(Bytes) -> <<(integer_to_binary(trunc(Bytes)))/binary, " Bytes">>.

scale(F, Base, [U | Units]) ->
    V = F / Base,
    case erlang:abs(V) < Base orelse Units =:= [] of
        true  -> <<(erlang:float_to_binary(V, [{decimals, 1}]))/binary, " ", U/binary>>;
        false -> scale(V, Base, Units)
    end.

%%%===================================================================
%%% General
%%%===================================================================

%% @safe transparent
-spec default(term(), map()) -> term().
default(V, A) ->
    Fallback = maps:get(default_value, A, <<>>),
    case bool_arg(A, boolean, false) of
        true  -> case ai_jinja_rt:truthy(V) of true -> V; false -> Fallback end;
        false -> case V of undefined -> Fallback; null -> Fallback; _ -> V end
    end.

%% @safe transparent
-spec d(term(), map()) -> term().
d(V, A) -> default(V, A).

%%%===================================================================
%%% Shared helpers
%%%===================================================================

%% Preserve the safe marker across a text transformation. This is what makes a
%% filter `transparent' in the sense of the @safe annotations.
keep_safe({safe, D}, F) -> {safe, F(iolist_to_binary(D))};
%% An undefined becomes the empty string rather than staying undefined, which
%% is what the reference implementation's soft_str does. It matters at the end
%% of a chain: `nope|upper|default("d")' yields "" there, not "d".
keep_safe(undefined, F)  -> F(<<>>);
keep_safe(V, F)          -> F(to_bin(V)).

to_bin(V) -> ai_jinja_rt:to_binary(V).

seq(undefined)          -> [];
seq({safe, D})          -> seq(iolist_to_binary(D));
seq(V) when is_binary(V) -> [<<C/utf8>> || <<C/utf8>> <= V];
seq(V)                  -> ai_jinja_rt:to_list(V).

as_map(M) when is_map(M) -> M;
as_map({ns, M})          -> M;
as_map(undefined)        -> #{};
as_map(V)                -> erlang:error({?TAG, {not_iterable, V}}).

chars(B) ->
    case unicode:characters_to_list(B, utf8) of
        L when is_list(L) -> L;
        _                 -> binary_to_list(B)
    end.

bin(Chars) -> unicode:characters_to_binary(Chars, utf8).

split_first(B) ->
    [C | Rest] = chars(B),
    {bin([C]), bin(Rest)}.

uc(B) -> bin(string:uppercase(chars(B))).
lc(B) -> bin(string:lowercase(chars(B))).

trim_both(B, Chars) ->
    bin(string:trim(chars(B), both, Chars)).

chars_of(B) when is_binary(B) -> chars(B);
chars_of(L) when is_list(L)   -> L.

int_arg(A, K, D) ->
    case maps:get(K, A, D) of
        undefined -> D;
        V when is_integer(V) -> V;
        V when is_float(V)   -> trunc(V);
        V when is_binary(V)  -> parse_int(V, 10, D);
        _ -> D
    end.

bool_arg(A, K, D) ->
    case maps:get(K, A, D) of
        undefined -> D;
        V -> ai_jinja_rt:truthy(V)
    end.

bin_arg(A, K, D) ->
    case maps:get(K, A, D) of
        undefined -> D;
        V -> ai_jinja_rt:to_binary(ai_jinja_rt:unsafe(V))
    end.

numeric(V, D) ->
    case ai_jinja_rt:unsafe(V) of
        N when is_number(N) -> N;
        B when is_binary(B) -> parse_float(B, D);
        _                   -> D
    end.

atomize(A) when is_atom(A)   -> A;
atomize(B) when is_binary(B) ->
    try binary_to_existing_atom(B, utf8) catch error:badarg -> '$no_such_key' end;
atomize(_) -> '$no_such_key'.
