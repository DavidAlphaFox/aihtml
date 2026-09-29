%%%-------------------------------------------------------------------
%%% @doc The diff view, ported from sigil (data/diff). DOM and class names
%%% are sigil's, so the styles in priv/css/sigil apply unchanged.
%%%
%%%   diff(Old, New, Css, Attrs)              line or word diff, computed here
%%%
%%% The diff is static HTML.
%%%
%%% diff/4 builds an element record (#ah_diff{}, defined in
%%% include/aihtml_diff.hrl) and render/1 turns it into HTML, so pages may
%%% also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_diff).
-behaviour(aihtml_element).

-include("aihtml_diff.hrl").

-export([diff/4, render/1, fields/1, catalog/0]).
%% The diff model, for tests and for pages that want the numbers.
-export([line_rows/2, word_parts/2, split_rows/1]).

-export_type([element/0, diff_row/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type element() :: #ah_diff{}.
%% One line of a line diff; the numbers are 1-based, `undefined' on the
%% side the line is not on.
-type diff_row() :: #{type := ctx | add | del, text := binary(),
                      old_no := pos_integer() | undefined,
                      new_no := pos_integer() | undefined}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc The differences between two texts. Css: `line' (default) or `word'
%% (inline word diff, always one column), `unified' (default) or `split'
%% (old and new side by side), `line_numbers' (in unified view; split
%% always shows them), `stats' (a +n / -n bar on top).
-spec diff(unicode:chardata(), unicode:chardata(), css(), attrs()) -> #ah_diff{}.
diff(Old, New, Css, Attrs) ->
    ?E:build(?MODULE, #ah_diff{old = Old, new = New}, Css, Attrs).

%% @doc The field names of this component's record.
-spec fields(atom()) -> [atom()].
fields(ah_diff) -> record_info(fields, ah_diff).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_diff{} = R) -> render_diff(R).

render_diff(#ah_diff{old = Old, new = New, mode = Mode, view = View0,
                     line_numbers = Numbers, stats = Stats} = R) ->
    Classes = ?E:classes(?MODULE, R),           % checks mode, view and the flags
    View = case Mode of word -> unified; line -> View0 end,
    Rows = case Mode of line -> line_rows(Old, New); word -> [] end,
    StatsBar = case Stats andalso Mode =:= line of
                   false -> [];
                   true ->
                       Add = length([x || #{type := add} <- Rows]),
                       Del = length([x || #{type := del} <- Rows]),
                       ?H:el('div',
                             [?H:el(span, [<<"+">>, integer_to_binary(Add)],
                                    [<<"ah-diff__stat">>], [{data_type, add}]),
                              ?H:el(span, [<<"-">>, integer_to_binary(Del)],
                                    [<<"ah-diff__stat">>], [{data_type, del}])],
                             [<<"ah-diff__stats">>], [])
               end,
    Body = case {Mode, View} of
               {word, _} ->
                   ?H:el('div',
                         [?H:el(span, V, [<<"ah-diff__word">>], [{data_type, T}])
                          || #{type := T, value := V} <- word_parts(Old, New)],
                         [<<"ah-diff__words">>], []);
               {line, split} ->
                   ?H:el('div', [diff_pair(P) || P <- split_rows(Rows)],
                         [<<"ah-diff__split">>], []);
               {line, unified} ->
                   ?H:el('div', [diff_row(Row, Numbers) || Row <- Rows],
                         [<<"ah-diff__body">>], [])
           end,
    ?H:el('div', [StatsBar, Body], Classes,
          [[{data_mode, Mode}, {data_view, View}], ?E:root_attrs(R, none)]).

diff_row(#{type := T, text := Text, old_no := O, new_no := N}, Numbers) ->
    ?H:el('div',
          [case Numbers of
               true -> [lineno(O), lineno(N)];
               false -> []
           end,
           ?H:el(span, marker(T), [<<"ah-diff__marker">>], [{aria_hidden, <<"true">>}]),
           ?H:el(span, row_text(Text), [<<"ah-diff__text">>], [])],
          [<<"ah-diff__row">>], [{data_type, T}]).

diff_pair({Left, Right}) ->
    ?H:el('div', [diff_side(old, Left, old_no), diff_side(new, Right, new_no)],
          [<<"ah-diff__pair">>], []).

diff_side(Side, undefined, _) ->
    ?H:el('div', [lineno(undefined), ?H:el(span, row_text(<<>>), [<<"ah-diff__text">>], [])],
          [<<"ah-diff__side">>], [{data_side, Side}, {data_type, empty}]);
diff_side(Side, #{type := T, text := Text} = Row, NoKey) ->
    ?H:el('div', [lineno(maps:get(NoKey, Row)),
                  ?H:el(span, row_text(Text), [<<"ah-diff__text">>], [])],
          [<<"ah-diff__side">>], [{data_side, Side}, {data_type, T}]).

lineno(N) ->
    ?H:el(span, case N of undefined -> <<>>; _ -> integer_to_binary(N) end,
          [<<"ah-diff__lineno">>], [{aria_hidden, <<"true">>}]).

marker(add) -> <<"+">>;
marker(del) -> <<"-">>;
marker(ctx) -> <<" ">>.

%% An empty line keeps its height with a no-break space.
row_text(<<>>) -> <<" "/utf8>>;
row_text(T) -> T.

%% @doc The line diff of two texts: one row per line, removed lines
%% before the added lines that replace them. A final newline does not
%% make an extra empty line.
-spec line_rows(unicode:chardata(), unicode:chardata()) -> [diff_row()].
line_rows(Old, New) ->
    number(group_changes(myers(lines(Old), lines(New))), 1, 1).

number([], _, _) -> [];
number([{eq, L} | Rest], O, N) ->
    [#{type => ctx, text => L, old_no => O, new_no => N} | number(Rest, O + 1, N + 1)];
number([{del, L} | Rest], O, N) ->
    [#{type => del, text => L, old_no => O, new_no => undefined} | number(Rest, O + 1, N)];
number([{ins, L} | Rest], O, N) ->
    [#{type => add, text => L, old_no => undefined, new_no => N} | number(Rest, O, N + 1)].

lines(T) ->
    case text(T) of
        <<>> -> [];
        B ->
            Ls = binary:split(B, <<"\n">>, [global]),
            Ls1 = case lists:last(Ls) of
                      <<>> when length(Ls) > 1 -> lists:droplast(Ls);
                      _ -> Ls
                  end,
            [strip_cr(L) || L <- Ls1]
    end.

strip_cr(L) ->
    case byte_size(L) of
        0 -> L;
        S -> case binary:last(L) of
                 $\r -> binary:part(L, 0, S - 1);
                 _ -> L
             end
    end.

%% @doc Pair the rows of a line diff for the side by side view: a run of
%% removed lines and the run of added lines after it share rows, the
%% shorter side padded with `undefined'; context lines are on both sides.
-spec split_rows([diff_row()]) -> [{diff_row() | undefined, diff_row() | undefined}].
split_rows([]) -> [];
split_rows([#{type := ctx} = R | Rest]) -> [{R, R} | split_rows(Rest)];
split_rows(Rows) ->
    {Dels, Rest1} = lists:splitwith(fun(#{type := T}) -> T =:= del end, Rows),
    {Adds, Rest2} = lists:splitwith(fun(#{type := T}) -> T =:= add end, Rest1),
    pad(Dels, Adds) ++ split_rows(Rest2).

pad([], []) -> [];
pad([D | Ds], [A | As]) -> [{D, A} | pad(Ds, As)];
pad([D | Ds], []) -> [{D, undefined} | pad(Ds, [])];
pad([], [A | As]) -> [{undefined, A} | pad([], As)].

%% @doc The word diff of two texts: words, runs of white space and single
%% other characters (so CJK text compares character by character), merged
%% into runs of the same type.
-spec word_parts(unicode:chardata(), unicode:chardata()) ->
          [#{type := ctx | add | del, value := binary()}].
word_parts(Old, New) ->
    Ops = group_changes(myers(tokens(text(Old)), tokens(text(New)))),
    merge_parts([{case Op of eq -> ctx; del -> del; ins -> add end, V} || {Op, V} <- Ops]).

merge_parts([]) -> [];
merge_parts([{T, A}, {T, B} | Rest]) -> merge_parts([{T, <<A/binary, B/binary>>} | Rest]);
merge_parts([{T, V} | Rest]) -> [#{type => T, value => V} | merge_parts(Rest)].

tokens(B) ->
    [unicode:characters_to_binary(Cs) || Cs <- chunk(unicode:characters_to_list(B))].

chunk([]) -> [];
chunk([C | _] = Cs) ->
    case char_class(C) of
        other -> [[C] | chunk(tl(Cs))];
        Class ->
            {Run, Rest} = lists:splitwith(fun(X) -> char_class(X) =:= Class end, Cs),
            [Run | chunk(Rest)]
    end.

char_class(C) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r; C =:= 16#3000 -> space;
char_class(C) when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9; C =:= $_ -> word;
char_class(C) when C >= 16#C0, C < 16#2000, C =/= 16#D7, C =/= 16#F7 -> word;
char_class(_) -> other.

%% Within each run of changes, removals first, then insertions.
group_changes(Ops) -> group_changes(Ops, [], []).

group_changes([], Dels, Ins) -> lists:reverse(Dels) ++ lists:reverse(Ins);
group_changes([{eq, _} = E | Rest], Dels, Ins) ->
    lists:reverse(Dels) ++ lists:reverse(Ins) ++ [E | group_changes(Rest, [], [])];
group_changes([{del, _} = D | Rest], Dels, Ins) -> group_changes(Rest, [D | Dels], Ins);
group_changes([{ins, _} = I | Rest], Dels, Ins) -> group_changes(Rest, Dels, [I | Ins]).

%% Myers' O(ND) diff: the shortest edit script from A to B as
%% [{eq | del | ins, Element}]. The common prefix and suffix are cut
%% first, which keeps D small for typical edits.
myers(A, B) ->
    {Pre, A1, B1} = common_prefix(A, B, []),
    {Suf, A2, B2} = common_suffix(A1, B1),
    [{eq, X} || X <- Pre] ++ myers_core(A2, B2) ++ [{eq, X} || X <- Suf].

common_prefix([X | A], [X | B], Acc) -> common_prefix(A, B, [X | Acc]);
common_prefix(A, B, Acc) -> {lists:reverse(Acc), A, B}.

common_suffix(A, B) ->
    {Suf, RA, RB} = common_prefix(lists:reverse(A), lists:reverse(B), []),
    {lists:reverse(Suf), lists:reverse(RA), lists:reverse(RB)}.

myers_core([], B) -> [{ins, X} || X <- B];
myers_core(A, []) -> [{del, X} || X <- A];
myers_core(A, B) ->
    At = list_to_tuple(A), Bt = list_to_tuple(B),
    N = tuple_size(At), M = tuple_size(Bt),
    Trace = myers_forward(At, Bt, N, M, 0, #{1 => 0}, []),
    myers_back(Trace, At, Bt, N, M, []).

%% Returns the V maps saved at the start of each D, the last D first.
myers_forward(At, Bt, N, M, D, V, Trace) ->
    case myers_step(At, Bt, N, M, D, -D, V, V) of
        {done, _} -> [{D, V} | Trace];
        {next, V1} -> myers_forward(At, Bt, N, M, D + 1, V1, [{D, V} | Trace])
    end.

myers_step(_, _, _, _, D, K, _, V1) when K > D -> {next, V1};
myers_step(At, Bt, N, M, D, K, V0, V1) ->
    X0 = case K =:= -D orelse (K =/= D andalso
                                 maps:get(K - 1, V1) < maps:get(K + 1, V1)) of
             true -> maps:get(K + 1, V1);
             false -> maps:get(K - 1, V1) + 1
         end,
    X = snake(At, Bt, N, M, X0, X0 - K),
    V2 = V1#{K => X},
    case X >= N andalso X - K >= M of
        true -> {done, V2};
        false -> myers_step(At, Bt, N, M, D, K + 2, V0, V2)
    end.

snake(At, Bt, N, M, X, Y) when X < N, Y < M ->
    case element(X + 1, At) =:= element(Y + 1, Bt) of
        true -> snake(At, Bt, N, M, X + 1, Y + 1);
        false -> X
    end;
snake(_, _, _, _, X, _) -> X.

myers_back([], _, _, _, _, Acc) -> Acc;
myers_back([{D, V} | Trace], At, Bt, X, Y, Acc) ->
    K = X - Y,
    PrevK = case K =:= -D orelse (K =/= D andalso
                                    maps:get(K - 1, V) < maps:get(K + 1, V)) of
                true -> K + 1;
                false -> K - 1
            end,
    PrevX = maps:get(PrevK, V),
    PrevY = PrevX - PrevK,
    {X1, Y1, Acc1} = diagonal(At, X, Y, PrevX, PrevY, Acc),
    case D of
        0 -> Acc1;
        _ ->
            Op = case X1 =:= PrevX of
                     true -> {ins, element(Y1, Bt)};
                     false -> {del, element(X1, At)}
                 end,
            myers_back(Trace, At, Bt, PrevX, PrevY, [Op | Acc1])
    end.

diagonal(At, X, Y, PX, PY, Acc) when X > PX, Y > PY ->
    diagonal(At, X - 1, Y - 1, PX, PY, [{eq, element(X, At)} | Acc]);
diagonal(_, X, Y, _, _, Acc) -> {X, Y, Acc}.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => diff, category => data,
       signature => <<"diff(Old, New, Css, Attrs)">>,
       root => <<"ah-diff">>,
       groups => #{mode => {[line, word], line}, view => {[unified, split], unified}},
       flags => [line_numbers, stats],
       classes => #{line => [], word => [], unified => [], split => [],
                    line_numbers => [], stats => []},
       doc => <<"The differences between two texts, line by line (unified or side by side) "
                "or word by word, computed on the server.">>,
       option_docs => #{line_numbers => <<"Show line numbers in the unified view (split always does).">>,
                        stats => <<"A bar with the number of added and removed lines.">>},
       methods => []}].

%%%===================================================================
%%% Internal
%%%===================================================================

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_text, L}})
    end;
text(X) -> beamai_html_escape:to_binary(X, aihtml).
