%%%-------------------------------------------------------------------
%%% @doc Markdown to HTML on the server, for markdown_view
%%% (aihtml_markdown_view) and anything else that shows Markdown written
%%% with markdown_editor to readers and search engines.
%%%
%%%   render(Markdown) -> binary()        the HTML
%%%
%%% A port of markdown-it 14 (the parser inside the editor bundle) with the
%%% options the editor uses: the "default" preset (CommonMark plus GFM
%%% tables and strikethrough, no typographer), `html' off and `linkify'
%%% off, plus the editor's task list rule ("- [ ] x" / "- [x] x" bullet
%%% lists). The block and inline parsers follow markdown-it rule by rule
%%% and produce the same tokens, and the renderer writes the same HTML:
%%% aihtml_lib_markdown_tests renders a fixtures file with both and
%%% requires identical bytes (scripts/render-markdown.mjs runs
%%% markdown-it). The tables markdown-it takes from its dependencies
%%% (HTML entities, Unicode punctuation) are generated into
%%% aihtml_lib_markdown_data by scripts/gen-markdown-data.mjs.
%%%
%%% Supported: ATX and setext headings, paragraphs, hard breaks (two
%%% spaces or a backslash at the end of a line), emphasis, strong,
%%% ~~strikethrough~~, `code`, links and images (inline and reference
%%% style), <autolinks>, blockquotes, bullet and ordered lists (tight and
%%% loose, nested), task lists, fenced and indented code blocks, thematic
%%% breaks, GFM tables with column alignment, backslash escapes and HTML
%%% entities.
%%%
%%% Safety, where the view differs from markdown-it's defaults:
%%%   - Raw HTML is never passed through: `<b>' in the Markdown is shown
%%%     as the text "<b>" (markdown-it with `html' off, as the editor).
%%%   - Every text and attribute value is escaped with beamai_html_escape
%%%     (the escaping of aihtml_html): & < > " and ' (markdown-it leaves '
%%%     alone; the only difference in the output).
%%%   - Links, images and autolinks accept only http:, https:, mailto: and
%%%     URLs without a scheme (relative ones: "/a", "b.html", "#c",
%%%     "//host/d"). With any other scheme (javascript:, data:, ftp: ...)
%%%     the link is not recognised and its Markdown stays as text, which is
%%%     what markdown-it does for the schemes it refuses. URLs are
%%%     normalised as markdown-it does (entities decoded, unsafe characters
%%%     percent-encoded, international host names in punycode), so the
%%%     check sees what the browser will see.
%%%
%%% Task lists render as `<ul class="contains-task-list">' with
%%% `<li class="task-list-item">' items, each starting with a disabled
%%% `<input type="checkbox" class="task-list-item-checkbox">' (checked
%%% when done). Fenced code with an info string gets
%%% `<code class="language-...">'. No other classes are written; style the
%%% output through a container (markdown_view's `ah-markdown-view').
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_markdown).

-export([render/1]).

-define(D, aihtml_lib_markdown_data).
-define(MAX_NESTING, 100).
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f)
                    orelse (C >= $A andalso C =< $F))).

-type token() :: #{atom() => term()}.
-type refs() :: undefined | #{binary() => {binary(), binary()}}.

%% Block parser state (markdown-it's StateBlock). Line tables are maps
%% from the line number: bm/em the line's begin/end offsets, ts the offset
%% of its first non-space character, sc its indent with tabs expanded, bsc
%% the virtual spaces before the line (blockquotes and lists rewrite them
%% while they parse their content and put them back afterwards).
-record(b, {src :: binary(),
            bm :: #{integer() => integer()},
            em :: #{integer() => integer()},
            ts :: #{integer() => integer()},
            sc :: #{integer() => integer()},
            bsc :: #{integer() => integer()},
            blk = 0 :: integer(),
            line = 0 :: integer(),
            line_max = 0 :: integer(),
            tight = false :: boolean(),
            list_indent = -1 :: integer(),
            parent = root :: atom(),
            level = 0 :: integer(),
            toks = [] :: [token()],             % reversed
            ntok = 0 :: non_neg_integer(),
            refs = undefined :: refs()}).

%% Inline parser state (markdown-it's StateInline). Tokens and delimiter
%% lists are maps from their index, as the post-processing rules rewrite
%% them in place; `dl' is the current delimiter list, `metas' the lists
%% opened by nested tokens (links), in order.
-record(i, {src :: binary(),
            pos = 0 :: integer(),
            max :: integer(),
            level = 0 :: integer(),
            pending = <<>> :: binary(),
            plevel = 0 :: integer(),
            toks = #{} :: #{non_neg_integer() => token()},
            ntok = 0 :: non_neg_integer(),
            dl = 0 :: non_neg_integer(),
            prev = [] :: [non_neg_integer()],
            metas = [] :: [non_neg_integer()],   % reversed
            dls = #{0 => {#{}, 0}} :: #{non_neg_integer() => {#{non_neg_integer() => map()},
                                                              non_neg_integer()}},
            next_dl = 1 :: pos_integer(),
            cache = #{} :: #{integer() => integer()},
            bt = #{} :: #{integer() => integer()},
            bt_scanned = false :: boolean(),
            link_level = 0 :: integer(),
            refs :: refs()}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Render Markdown (a binary or string, UTF-8) to HTML.
-spec render(unicode:chardata()) -> binary().
render(Markdown) ->
    Src = normalize(text(Markdown)),
    B = block_parse(Src),
    Blocks0 = lists:reverse(B#b.toks),
    Refs = B#b.refs,
    Blocks1 = [case T of
                   #{type := inline, content := C} ->
                       T#{children := text_join(inline_parse(C, Refs))};
                   _ -> T
               end || T <- Blocks0],
    Blocks = task_checkboxes(task_lists(list_to_tuple(Blocks1))),
    iolist_to_binary(render_tokens(Blocks)).

text(B) when is_binary(B) -> check_utf8(B);
text(L) -> check_utf8(unicode:characters_to_binary(L)).

check_utf8(B) when is_binary(B) ->
    case unicode:characters_to_binary(B) of
        B -> B;
        _ -> error({aihtml, {bad_markdown, not_utf8}})
    end;
check_utf8(_) -> error({aihtml, {bad_markdown, not_utf8}}).

%% markdown-it's normalize: \r\n and \r become \n, NUL becomes U+FFFD.
normalize(S) ->
    S1 = binary:replace(binary:replace(S, <<"\r\n">>, <<"\n">>, [global]),
                        <<"\r">>, <<"\n">>, [global]),
    binary:replace(S1, <<0>>, <<16#FFFD/utf8>>, [global]).

%%%===================================================================
%%% Tokens
%%%===================================================================

tok(Type, Tag, Nesting) ->
    #{type => Type, tag => Tag, nesting => Nesting, level => 0, attrs => [],
      content => <<>>, markup => <<>>, info => <<>>, hidden => false,
      block => false, children => []}.

%%%===================================================================
%%% Block state
%%%===================================================================

block_parse(<<>>) ->
    #b{src = <<>>, bm = #{}, em = #{}, ts = #{}, sc = #{}, bsc = #{}};
block_parse(Src) ->
    Lines = scan_lines(Src, 0, 0, 0, 0, false, byte_size(Src), []),
    Len = byte_size(Src),
    All = lists:reverse([{Len, Len, 0, 0} | Lines]),
    N = length(All),
    Idx = lists:seq(0, N - 1),
    Z = fun(F) -> maps:from_list(lists:zip(Idx, [F(X) || X <- All])) end,
    S = #b{src = Src,
           bm = Z(fun({B, _, _, _}) -> B end), em = Z(fun({_, E, _, _}) -> E end),
           ts = Z(fun({_, _, T, _}) -> T end), sc = Z(fun({_, _, _, C}) -> C end),
           bsc = maps:from_list([{I, 0} || I <- Idx]),
           line_max = N - 1},
    tokenize(S, 0, N - 1).

%% StateBlock's constructor: {begin, end, first non-space, indent} per line.
scan_lines(_Src, Pos, _Start, _Indent, _Offset, _Found, Len, Acc) when Pos >= Len -> Acc;
scan_lines(Src, Pos, Start, Indent, Offset, false, Len, Acc) ->
    case binary:at(Src, Pos) of
        $\t -> scan_lines(Src, Pos + 1, Start, Indent + 1, Offset + 4 - Offset rem 4, false,
                          Len, Acc);
        $\s -> scan_lines(Src, Pos + 1, Start, Indent + 1, Offset + 1, false, Len, Acc);
        _ -> scan_lines(Src, Pos, Start, Indent, Offset, true, Len, Acc)
    end;
scan_lines(Src, Pos, Start, Indent, Offset, true, Len, Acc) ->
    Ch = binary:at(Src, Pos),
    case Ch =:= $\n orelse Pos =:= Len - 1 of
        true ->
            End = case Ch of $\n -> Pos; _ -> Pos + 1 end,
            scan_lines(Src, End + 1, End + 1, 0, 0, false, Len,
                       [{Start, End, Indent, Offset} | Acc]);
        false ->
            scan_lines(Src, Pos + 1, Start, Indent, Offset, true, Len, Acc)
    end.

bm(S, L) -> maps:get(L, S#b.bm).
em(S, L) -> maps:get(L, S#b.em).
ts(S, L) -> maps:get(L, S#b.ts).
sc(S, L) -> maps:get(L, S#b.sc).
bsc(S, L) -> maps:get(L, S#b.bsc).

set(bm, S, L, V) -> S#b{bm = maps:put(L, V, S#b.bm)};
set(ts, S, L, V) -> S#b{ts = maps:put(L, V, S#b.ts)};
set(sc, S, L, V) -> S#b{sc = maps:put(L, V, S#b.sc)};
set(bsc, S, L, V) -> S#b{bsc = maps:put(L, V, S#b.bsc)}.

is_empty(S, L) -> bm(S, L) + ts(S, L) >= em(S, L).

skip_empty_lines(S, From) ->
    case From < S#b.line_max andalso is_empty(S, From) of
        true -> skip_empty_lines(S, From + 1);
        false -> From
    end.

%% The character (byte) at Pos, -1 outside the text.
c(Src, P) when P >= 0, P < byte_size(Src) -> binary:at(Src, P);
c(_, _) -> -1.

slice(Src, From0, To0) ->
    Size = byte_size(Src),
    From = max(0, min(From0, Size)),
    To = max(0, min(To0, Size)),
    case To > From of
        true -> binary:part(Src, From, To - From);
        false -> <<>>
    end.

slice(Src, From) -> slice(Src, From, byte_size(Src)).

is_space(C) -> C =:= $\s orelse C =:= $\t.

skip_spaces(Src, P) ->
    case is_space(c(Src, P)) of true -> skip_spaces(Src, P + 1); false -> P end.

skip_chars(Src, P, Ch) ->
    case c(Src, P) of Ch -> skip_chars(Src, P + 1, Ch); _ -> P end.

skip_spaces_back(Src, P, Min) when P > Min ->
    case is_space(c(Src, P - 1)) of true -> skip_spaces_back(Src, P - 1, Min); false -> P end;
skip_spaces_back(_, P, _) -> P.

skip_chars_back(Src, P, Ch, Min) when P > Min ->
    case c(Src, P - 1) of Ch -> skip_chars_back(Src, P - 1, Ch, Min); _ -> P end;
skip_chars_back(_, P, _, _) -> P.

push(Type, Tag, Nesting, Extra, #b{level = L0} = S) ->
    L1 = case Nesting < 0 of true -> L0 - 1; false -> L0 end,
    T = maps:merge((tok(Type, Tag, Nesting))#{level := L1, block := true}, Extra),
    L2 = case Nesting > 0 of true -> L1 + 1; false -> L1 end,
    S#b{level = L2, toks = [T | S#b.toks], ntok = S#b.ntok + 1}.

%% StateBlock.getLines: the lines [Begin, End) without Indent columns of
%% indentation.
get_lines(_S, Begin, End, _Indent, _KeepLastLF) when Begin >= End -> <<>>;
get_lines(S, Begin, End, Indent, KeepLastLF) ->
    iolist_to_binary([get_line(S, L, End, Indent, KeepLastLF) || L <- lists:seq(Begin, End - 1)]).

get_line(#b{src = Src} = S, L, End, Indent, KeepLastLF) ->
    Start = bm(S, L),
    Last = case L + 1 < End orelse KeepLastLF of
               true -> em(S, L) + 1;
               false -> em(S, L)
           end,
    {First, LineIndent} = line_indent(S, L, Start, Start, Last, 0, Indent),
    case LineIndent > Indent of
        true -> [binary:copy(<<" ">>, LineIndent - Indent), slice(Src, First, Last)];
        false -> slice(Src, First, Last)
    end.

line_indent(#b{src = Src} = S, L, Start, First, Last, LI, Indent)
  when First < Last, LI < Indent ->
    case c(Src, First) of
        $\t ->
            line_indent(S, L, Start, First + 1, Last, LI + 4 - (LI + bsc(S, L)) rem 4, Indent);
        $\s ->
            line_indent(S, L, Start, First + 1, Last, LI + 1, Indent);
        _ ->
            %% blockquote and list markers count as indentation
            case First - Start < ts(S, L) of
                true -> line_indent(S, L, Start, First + 1, Last, LI + 1, Indent);
                false -> {First, LI}
            end
    end;
line_indent(_, _, _, First, _, LI, _) -> {First, LI}.

%%%===================================================================
%%% Block tokenizer (ParserBlock)
%%%===================================================================

-define(RULES, [table, code, fence, blockquote, hr, list, reference, heading, lheading,
                paragraph]).
%% The rules that may end a paragraph, a reference, a blockquote, a list
%% (their "alt" lists; html_block is off).
-define(TERM_PARAGRAPH, [table, fence, blockquote, hr, list, heading]).
-define(TERM_BLOCKQUOTE, [fence, blockquote, hr, list, heading]).
-define(TERM_LIST, [fence, blockquote, hr]).

tokenize(S, Start, End) -> tokenize(S, Start, End, false).

tokenize(S0, Line0, End, HasEmpty) when Line0 < End ->
    Line = skip_empty_lines(S0, Line0),
    S = S0#b{line = Line},
    if
        Line >= End -> S;
        true ->
            case sc(S, Line) < S#b.blk of
                true -> S;
                false when S#b.level >= ?MAX_NESTING -> S#b{line = End};
                false ->
                    S1 = try_rules(?RULES, S, Line, End),
                    S2 = S1#b{tight = not HasEmpty},
                    HasEmpty1 = HasEmpty orelse is_empty(S2, S2#b.line - 1),
                    L2 = S2#b.line,
                    case L2 < End andalso is_empty(S2, L2) of
                        true -> tokenize(S2#b{line = L2 + 1}, L2 + 1, End, true);
                        false -> tokenize(S2, L2, End, HasEmpty1)
                    end
            end
    end;
tokenize(S, _, _, _) -> S.

try_rules([R | Rs], S, Line, End) ->
    case rule(R, S, Line, End, false) of
        {true, S1} -> S1;
        false -> try_rules(Rs, S, Line, End)
    end.

terminates(Rules, S, Line, End) ->
    lists:any(fun(R) -> rule(R, S, Line, End, true) =/= false end, Rules).

rule(table, S, L, E, Silent) -> r_table(S, L, E, Silent);
rule(code, S, L, E, _) -> r_code(S, L, E);
rule(fence, S, L, E, Silent) -> r_fence(S, L, E, Silent);
rule(blockquote, S, L, E, Silent) -> r_blockquote(S, L, E, Silent);
rule(hr, S, L, _, Silent) -> r_hr(S, L, Silent);
rule(list, S, L, E, Silent) -> r_list(S, L, E, Silent);
rule(reference, S, L, _, Silent) -> r_reference(S, L, Silent);
rule(heading, S, L, _, Silent) -> r_heading(S, L, Silent);
rule(lheading, S, L, E, _) -> r_lheading(S, L, E);
rule(paragraph, S, L, E, _) -> r_paragraph(S, L, E).

%%% code: indented code block

r_code(#b{blk = Blk} = S, Start, End) ->
    case sc(S, Start) - Blk >= 4 of
        false -> false;
        true ->
            Last = code_last(S, Start + 1, Start + 1, End),
            Content = <<(get_lines(S, Start, Last, 4 + Blk, false))/binary, "\n">>,
            {true, push(code_block, <<"code">>, 0, #{content => Content}, S#b{line = Last})}
    end.

code_last(_S, Next, Last, End) when Next >= End -> Last;
code_last(S, Next, Last, End) ->
    case is_empty(S, Next) of
        true -> code_last(S, Next + 1, Last, End);
        false ->
            case sc(S, Next) - S#b.blk >= 4 of
                true -> code_last(S, Next + 1, Next + 1, End);
                false -> Last
            end
    end.

%%% fence

r_fence(#b{src = Src, blk = Blk} = S, Start, End, Silent) ->
    Pos = bm(S, Start) + ts(S, Start),
    Max = em(S, Start),
    Marker = c(Src, Pos),
    case sc(S, Start) - Blk >= 4 orelse Pos + 3 > Max
        orelse (Marker =/= $~ andalso Marker =/= $`) of
        true -> false;
        false ->
            Pos2 = skip_chars(Src, Pos, Marker),
            Len = Pos2 - Pos,
            Params = slice(Src, Pos2, Max),
            case Len < 3 orelse
                (Marker =:= $` andalso binary:match(Params, <<"`">>) =/= nomatch) of
                true -> false;
                false when Silent -> {true, S};
                false ->
                    Markup = slice(Src, Pos, Pos2),
                    {Next, Have} = fence_end(S, Start, End, Marker, Len),
                    Content = get_lines(S, Start + 1, Next, sc(S, Start), true),
                    Line = Next + case Have of true -> 1; false -> 0 end,
                    {true, push(fence, <<"code">>, 0,
                                #{info => Params, content => Content, markup => Markup},
                                S#b{line = Line})}
            end
    end.

fence_end(#b{src = Src, blk = Blk} = S, Next0, End, Marker, Len) ->
    Next = Next0 + 1,
    case Next >= End of
        true -> {Next, false};
        false ->
            Pos = bm(S, Next) + ts(S, Next),
            Max = em(S, Next),
            case Pos < Max andalso sc(S, Next) < Blk of
                true -> {Next, false};
                false ->
                    case c(Src, Pos) =/= Marker orelse sc(S, Next) - Blk >= 4 of
                        true -> fence_end(S, Next, End, Marker, Len);
                        false ->
                            P2 = skip_chars(Src, Pos, Marker),
                            case P2 - Pos < Len orelse skip_spaces(Src, P2) < Max of
                                true -> fence_end(S, Next, End, Marker, Len);
                                false -> {Next, true}
                            end
                    end
            end
    end.

%%% blockquote

r_blockquote(#b{src = Src, blk = Blk} = S0, Start, End, Silent) ->
    Pos = bm(S0, Start) + ts(S0, Start),
    case sc(S0, Start) - Blk >= 4 orelse c(Src, Pos) =/= $> of
        true -> false;
        false when Silent -> {true, S0};
        false ->
            OldLineMax = S0#b.line_max,
            OldParent = S0#b.parent,
            {Next, Saved, S1} = bq_lines(S0#b{parent = blockquote}, Start, End, false, []),
            S2 = push(blockquote_open, <<"blockquote">>, 1, #{markup => <<">">>},
                      S1#b{blk = 0}),
            S3 = tokenize(S2, Start, Next),
            S4 = push(blockquote_close, <<"blockquote">>, -1, #{markup => <<">">>}, S3),
            S5 = restore_lines(S4#b{line_max = OldLineMax, parent = OldParent},
                               Start, lists:reverse(Saved)),
            {true, S5#b{blk = Blk}}
    end.

restore_lines(S, _, []) -> S;
restore_lines(S, L, [{BM, BSC, SC, TS} | Rest]) ->
    restore_lines(set(bsc, set(sc, set(ts, set(bm, S, L, BM), L, TS), L, SC), L, BSC),
                  L + 1, Rest).

saved(S, L) -> {bm(S, L), bsc(S, L), sc(S, L), ts(S, L)}.

bq_lines(S, Next, End, _LastEmpty, Saved) when Next >= End -> {Next, Saved, S};
bq_lines(#b{src = Src, blk = Blk} = S, Next, End, LastEmpty, Saved) ->
    IsOutdented = sc(S, Next) < Blk,
    Pos = bm(S, Next) + ts(S, Next),
    Max = em(S, Next),
    if
        Pos >= Max -> {Next, Saved, S};
        true ->
            case c(Src, Pos) =:= $> andalso not IsOutdented of
                true ->
                    P1 = Pos + 1,
                    Initial0 = sc(S, Next) + 1,
                    {P2, Initial, Adjust, SpaceAfter} =
                        case c(Src, P1) of
                            $\s -> {P1 + 1, Initial0 + 1, false, true};
                            $\t ->
                                case (bsc(S, Next) + Initial0) rem 4 =:= 3 of
                                    true -> {P1 + 1, Initial0 + 1, false, true};
                                    false -> {P1, Initial0, true, true}
                                end;
                            _ -> {P1, Initial0, false, false}
                        end,
                    Saved1 = [saved(S, Next) | Saved],
                    {P3, Offset} = bq_offset(S, Next, P2, Max, Initial, Adjust),
                    S1 = set(bm, S, Next, P2),
                    S2 = set(bsc, S1, Next, sc(S, Next) + 1 + case SpaceAfter of
                                                                  true -> 1;
                                                                  false -> 0
                                                              end),
                    S3 = set(sc, S2, Next, Offset - Initial),
                    S4 = set(ts, S3, Next, P3 - P2),
                    bq_lines(S4, Next + 1, End, P3 >= Max, Saved1);
                false when LastEmpty -> {Next, Saved, S};
                false ->
                    case terminates(?TERM_BLOCKQUOTE, S, Next, End) of
                        true ->
                            S1 = S#b{line_max = Next},
                            case Blk =/= 0 of
                                true ->
                                    {Next, [saved(S1, Next) | Saved],
                                     set(sc, S1, Next, sc(S1, Next) - Blk)};
                                false -> {Next, Saved, S1}
                            end;
                        false ->
                            bq_lines(set(sc, S, Next, -1), Next + 1, End, LastEmpty,
                                     [saved(S, Next) | Saved])
                    end
            end
    end.

bq_offset(#b{src = Src} = S, L, P, Max, Offset, Adjust) when P < Max ->
    case c(Src, P) of
        $\t ->
            A = case Adjust of true -> 1; false -> 0 end,
            bq_offset(S, L, P + 1, Max, Offset + 4 - (Offset + bsc(S, L) + A) rem 4, Adjust);
        $\s -> bq_offset(S, L, P + 1, Max, Offset + 1, Adjust);
        _ -> {P, Offset}
    end;
bq_offset(_, _, P, _, Offset, _) -> {P, Offset}.

%%% hr

r_hr(#b{src = Src} = S, Start, Silent) ->
    Max = em(S, Start),
    Pos = bm(S, Start) + ts(S, Start),
    Marker = c(Src, Pos),
    case sc(S, Start) - S#b.blk >= 4 orelse
        not (Marker =:= $* orelse Marker =:= $- orelse Marker =:= $_) of
        true -> false;
        false ->
            case hr_count(Src, Pos + 1, Max, Marker, 1) of
                Cnt when Cnt >= 3 ->
                    case Silent of
                        true -> {true, S};
                        false ->
                            {true, push(hr, <<"hr">>, 0,
                                        #{markup => binary:copy(<<Marker>>, Cnt)},
                                        S#b{line = Start + 1})}
                    end;
                _ -> false
            end
    end.

hr_count(Src, P, Max, Marker, Cnt) when P < Max ->
    case c(Src, P) of
        Marker -> hr_count(Src, P + 1, Max, Marker, Cnt + 1);
        Ch -> case is_space(Ch) of
                  true -> hr_count(Src, P + 1, Max, Marker, Cnt);
                  false -> 0
              end
    end;
hr_count(_, _, _, _, Cnt) -> Cnt.

%%% list

skip_bullet(#b{src = Src} = S, L) ->
    Max = em(S, L),
    Pos = bm(S, L) + ts(S, L),
    case c(Src, Pos) of
        M when M =:= $*; M =:= $-; M =:= $+ ->
            case Pos + 1 < Max andalso not is_space(c(Src, Pos + 1)) of
                true -> -1;
                false -> Pos + 1
            end;
        _ -> -1
    end.

skip_ordered(#b{src = Src} = S, L) ->
    Start = bm(S, L) + ts(S, L),
    Max = em(S, L),
    Ch = c(Src, Start),
    case Start + 1 >= Max orelse Ch < $0 orelse Ch > $9 of
        true -> -1;
        false ->
            case ordered_digits(Src, Start + 1, Start, Max) of
                -1 -> -1;
                Pos ->
                    case Pos < Max andalso not is_space(c(Src, Pos)) of
                        true -> -1;
                        false -> Pos
                    end
            end
    end.

ordered_digits(_Src, Pos, _Start, Max) when Pos >= Max -> -1;
ordered_digits(Src, Pos, Start, Max) ->
    Ch = c(Src, Pos),
    if
        Ch >= $0, Ch =< $9 ->
            case Pos + 1 - Start >= 10 of
                true -> -1;
                false -> ordered_digits(Src, Pos + 1, Start, Max)
            end;
        Ch =:= $); Ch =:= $. -> Pos + 1;
        true -> -1
    end.

r_list(#b{src = Src, blk = Blk} = S0, Start, End, Silent) ->
    Sc = sc(S0, Start),
    LI = S0#b.list_indent,
    case Sc - Blk >= 4 orelse (LI >= 0 andalso Sc - LI >= 4 andalso Sc < Blk) of
        true -> false;
        false ->
            IsTermPara = Silent andalso S0#b.parent =:= paragraph andalso Sc >= Blk,
            Marker =
                case skip_ordered(S0, Start) of
                    P when P >= 0 ->
                        SPos0 = bm(S0, Start) + ts(S0, Start),
                        V0 = binary_to_integer(slice(Src, SPos0, P - 1)),
                        case IsTermPara andalso V0 =/= 1 of
                            true -> false;
                            false -> {ordered, P, SPos0, V0}
                        end;
                    _ ->
                        case skip_bullet(S0, Start) of
                            P when P >= 0 -> {bullet, P, 0, 0};
                            _ -> false
                        end
                end,
            case Marker of
                false -> false;
                {_, PAM, _, _} when IsTermPara ->
                    case skip_spaces(Src, PAM) >= em(S0, Start) of
                        true -> false;
                        false -> {true, S0}
                    end;
                _ when Silent -> {true, S0};
                {Kind, PAM, SPos, V} ->
                    MarkerChar = c(Src, PAM - 1),
                    ListTokIdx = S0#b.ntok,
                    MarkupB = <<MarkerChar>>,
                    S1 = case Kind of
                             ordered ->
                                 push(ordered_list_open, <<"ol">>, 1,
                                      #{markup => MarkupB,
                                        attrs => [{<<"start">>, integer_to_binary(V)}
                                                  || V =/= 1]}, S0);
                             bullet ->
                                 push(bullet_list_open, <<"ul">>, 1, #{markup => MarkupB}, S0)
                         end,
                    OldParent = S1#b.parent,
                    {Next, Tight, S2} = list_items(S1#b{parent = list}, Kind, Start, End,
                                                   PAM, SPos, MarkerChar, false, true),
                    CloseType = case Kind of ordered -> ordered_list_close;
                                    bullet -> bullet_list_close end,
                    CloseTag = case Kind of ordered -> <<"ol">>; bullet -> <<"ul">> end,
                    S3 = push(CloseType, CloseTag, -1, #{markup => MarkupB}, S2),
                    S4 = S3#b{line = Next, parent = OldParent},
                    {true, case Tight of
                               true -> mark_tight(S4, ListTokIdx);
                               false -> S4
                           end}
            end
    end.

list_items(#b{src = Src} = S0, Kind, Next, End, PAM, SPos, MarkerChar, PrevEmptyEnd, Tight) ->
    Max = em(S0, Next),
    Initial = sc(S0, Next) + PAM - (bm(S0, Next) + ts(S0, Next)),
    {ContentStart, Offset} = list_offset(S0, Next, PAM, Max, Initial),
    IAM0 = case ContentStart >= Max of
               true -> 1;
               false -> Offset - Initial
           end,
    IAM = case IAM0 > 4 of true -> 1; false -> IAM0 end,
    Indent = Initial + IAM,
    Info = case Kind of ordered -> slice(Src, SPos, PAM - 1); bullet -> <<>> end,
    S1 = push(list_item_open, <<"li">>, 1, #{markup => <<MarkerChar>>, info => Info}, S0),
    OldTight = S1#b.tight,
    OldTShift = ts(S1, Next),
    OldSCount = sc(S1, Next),
    OldListIndent = S1#b.list_indent,
    S2 = set(sc, set(ts, S1#b{list_indent = S1#b.blk, blk = Indent, tight = true},
                     Next, ContentStart - bm(S1, Next)), Next, Offset),
    S3 = case ContentStart >= Max andalso is_empty(S2, Next + 1) of
             true -> S2#b{line = min(S2#b.line + 2, End)};
             false -> tokenize(S2, Next, End)
         end,
    Tight1 = Tight andalso S3#b.tight andalso not PrevEmptyEnd,
    Line = S3#b.line,
    PrevEmptyEnd1 = Line - Next > 1 andalso is_empty(S3, Line - 1),
    S4 = set(sc, set(ts, S3#b{blk = S3#b.list_indent, list_indent = OldListIndent,
                              tight = OldTight}, Next, OldTShift), Next, OldSCount),
    S5 = push(list_item_close, <<"li">>, -1, #{markup => <<MarkerChar>>}, S4),
    Next1 = S5#b.line,
    Stop = Next1 >= End orelse sc(S5, Next1) < S5#b.blk
        orelse sc(S5, Next1) - S5#b.blk >= 4
        orelse terminates(?TERM_LIST, S5, Next1, End),
    case Stop of
        true -> {Next1, Tight1, S5};
        false ->
            {P, SPos1} = case Kind of
                             ordered -> {skip_ordered(S5, Next1), bm(S5, Next1) + ts(S5, Next1)};
                             bullet -> {skip_bullet(S5, Next1), SPos}
                         end,
            case P < 0 orelse c(Src, P - 1) =/= MarkerChar of
                true -> {Next1, Tight1, S5};
                false -> list_items(S5, Kind, Next1, End, P, SPos1, MarkerChar,
                                    PrevEmptyEnd1, Tight1)
            end
    end.

list_offset(#b{src = Src} = S, L, P, Max, Offset) when P < Max ->
    case c(Src, P) of
        $\t -> list_offset(S, L, P + 1, Max, Offset + 4 - (Offset + bsc(S, L)) rem 4);
        $\s -> list_offset(S, L, P + 1, Max, Offset + 1);
        _ -> {P, Offset}
    end;
list_offset(_, _, P, _, Offset) -> {P, Offset}.

%% markTightParagraphs: hide the paragraphs directly inside the list items.
mark_tight(#b{toks = Rev, ntok = N, level = Level0} = S, Idx) ->
    {NewRev, OldRev} = lists:split(N - Idx, Rev),
    T0 = list_to_tuple(lists:reverse(NewRev)),
    T = mark_tight(T0, 3, tuple_size(T0) - 2, Level0 + 2),
    S#b{toks = lists:reverse(tuple_to_list(T), OldRev)}.

%% 1-based positions: token Idx+2 is element 3.
mark_tight(T, P, Last, Level) when P =< Last ->
    case element(P, T) of
        #{level := Level, type := paragraph_open} = Tok ->
            T1 = setelement(P, T, Tok#{hidden := true}),
            T2 = setelement(P + 2, T1, (element(P + 2, T1))#{hidden := true}),
            mark_tight(T2, P + 3, Last, Level);
        _ -> mark_tight(T, P + 1, Last, Level)
    end;
mark_tight(T, _, _, _) -> T.

%%% reference

r_reference(#b{src = Src} = S, Start, Silent) ->
    Pos = bm(S, Start) + ts(S, Start),
    Max = em(S, Start),
    case sc(S, Start) - S#b.blk >= 4 orelse c(Src, Pos) =/= $[ of
        true -> false;
        false ->
            Str = slice(Src, Pos, Max + 1),
            case ref_label(S, Str, 1, Start + 1) of
                false -> false;
                {LabelEnd, Str1, Next1} ->
                    case c(Str1, LabelEnd + 1) =/= $: of
                        true -> false;
                        false -> ref_rest(S, Str1, LabelEnd, Next1, Silent)
                    end
            end
    end.

%% The next line of a multi-line reference definition, or null.
ref_next_line(#b{src = Src} = S, Next) ->
    case Next >= S#b.line_max orelse is_empty(S, Next) of
        true -> null;
        false ->
            IsCont = sc(S, Next) - S#b.blk > 3 orelse sc(S, Next) < 0,
            Term = not IsCont andalso
                terminates(?TERM_PARAGRAPH, S#b{parent = reference}, Next, S#b.line_max),
            case Term of
                true -> null;
                false -> slice(Src, bm(S, Next) + ts(S, Next), em(S, Next) + 1)
            end
    end.

ref_append(S, Str, Next) ->
    case ref_next_line(S, Next) of
        null -> {Str, Next};
        Line -> {<<Str/binary, Line/binary>>, Next + 1}
    end.

ref_label(S, Str, Pos, Next) when Pos < byte_size(Str) ->
    case c(Str, Pos) of
        $[ -> false;
        $] -> {Pos, Str, Next};
        $\n ->
            {Str1, Next1} = ref_append(S, Str, Next),
            ref_label(S, Str1, Pos + 1, Next1);
        $\\ ->
            P1 = Pos + 1,
            case P1 < byte_size(Str) andalso c(Str, P1) =:= $\n of
                true ->
                    {Str1, Next1} = ref_append(S, Str, Next),
                    ref_label(S, Str1, P1 + 1, Next1);
                false -> ref_label(S, Str, P1 + 1, Next)
            end;
        _ -> ref_label(S, Str, Pos + 1, Next)
    end;
ref_label(_, _, _, _) -> false.

%% Skip spaces and newlines, pulling in the next lines at each newline.
ref_skip(S, Str, Pos, Next) when Pos < byte_size(Str) ->
    case c(Str, Pos) of
        $\n ->
            {Str1, Next1} = ref_append(S, Str, Next),
            ref_skip(S, Str1, Pos + 1, Next1);
        Ch ->
            case is_space(Ch) of
                true -> ref_skip(S, Str, Pos + 1, Next);
                false -> {Pos, Str, Next}
            end
    end;
ref_skip(_, Str, Pos, Next) -> {Pos, Str, Next}.

ref_rest(S, Str0, LabelEnd, Next0, Silent) ->
    {Pos0, Str1, Next1} = ref_skip(S, Str0, LabelEnd + 2, Next0),
    case parse_link_destination(Str1, Pos0, byte_size(Str1)) of
        error -> false;
        {ok, DestPos, DestStr} ->
            Href = normalize_link(DestStr),
            case validate_link(Href) of
                false -> false;
                true ->
                    DestEndLine = Next1,
                    {Pos1, Str2, Next2} = ref_skip(S, Str1, DestPos, Next1),
                    {TitleRes, Pos2, Str3, Next3} =
                        ref_title(S, parse_link_title(Str2, Pos1, byte_size(Str2), undefined),
                                  Pos1, Str2, Next2),
                    Max = byte_size(Str3),
                    {Title, Pos3, Next4} =
                        case Pos2 < Max andalso DestPos =/= Pos2 andalso maps:get(ok, TitleRes) of
                            true -> {maps:get(str, TitleRes), maps:get(pos, TitleRes), Next3};
                            false -> {<<>>, DestPos, DestEndLine}
                        end,
                    Pos4 = skip_spaces_in(Str3, Pos3),
                    {Title1, Pos5, Next5} =
                        case Pos4 < Max andalso c(Str3, Pos4) =/= $\n andalso Title =/= <<>> of
                            true -> {<<>>, skip_spaces_in(Str3, DestPos), DestEndLine};
                            false -> {Title, Pos4, Next4}
                        end,
                    case Pos5 < Max andalso c(Str3, Pos5) =/= $\n of
                        true -> false;
                        false ->
                            case normalize_reference(slice(Str3, 1, LabelEnd)) of
                                <<>> -> false;
                                _ when Silent -> {true, S};
                                Label ->
                                    Refs0 = case S#b.refs of undefined -> #{}; R -> R end,
                                    Refs = case maps:is_key(Label, Refs0) of
                                               true -> Refs0;
                                               false -> Refs0#{Label => {Href, Title1}}
                                           end,
                                    {true, S#b{refs = Refs, line = Next5}}
                            end
                    end
            end
    end.

%% The title may continue over several lines.
ref_title(S, #{can_continue := true} = Res, Pos, Str, Next) ->
    case ref_next_line(S, Next) of
        null -> {Res, Pos, Str, Next};
        Line ->
            Max = byte_size(Str),
            Str1 = <<Str/binary, Line/binary>>,
            ref_title(S, parse_link_title(Str1, Max, byte_size(Str1), Res), Max, Str1, Next + 1)
    end;
ref_title(_, Res, Pos, Str, Next) -> {Res, Pos, Str, Next}.

skip_spaces_in(Str, P) ->
    case P < byte_size(Str) andalso is_space(c(Str, P)) of
        true -> skip_spaces_in(Str, P + 1);
        false -> P
    end.

%%% heading (ATX)

r_heading(#b{src = Src} = S, Start, Silent) ->
    Pos = bm(S, Start) + ts(S, Start),
    Max = em(S, Start),
    case sc(S, Start) - S#b.blk >= 4 orelse c(Src, Pos) =/= $# orelse Pos >= Max of
        true -> false;
        false ->
            {P, Level} = heading_level(Src, Pos + 1, Max, 1),
            case Level > 6 orelse (P < Max andalso not is_space(c(Src, P))) of
                true -> false;
                false when Silent -> {true, S};
                false ->
                    Max1 = skip_spaces_back(Src, Max, P),
                    Tmp = skip_chars_back(Src, Max1, $#, P),
                    Max2 = case Tmp > P andalso is_space(c(Src, Tmp - 1)) of
                               true -> Tmp;
                               false -> Max1
                           end,
                    Tag = <<"h", (integer_to_binary(Level))/binary>>,
                    Markup = binary:copy(<<"#">>, Level),
                    S1 = push(heading_open, Tag, 1, #{markup => Markup},
                              S#b{line = Start + 1}),
                    S2 = push(inline, <<>>, 0,
                              #{content => ascii_trim(slice(Src, P, Max2))}, S1),
                    {true, push(heading_close, Tag, -1, #{markup => Markup}, S2)}
            end
    end.

heading_level(Src, P, Max, Level) ->
    case c(Src, P) =:= $# andalso P < Max andalso Level =< 6 of
        true -> heading_level(Src, P + 1, Max, Level + 1);
        false -> {P, Level}
    end.

%%% lheading (setext)

r_lheading(#b{blk = Blk} = S, Start, End) ->
    case sc(S, Start) - Blk >= 4 of
        true -> false;
        false ->
            case lheading_scan(S#b{parent = paragraph}, Start + 1, End) of
                {_, 0, _} -> false;
                {Next, Level, Marker} ->
                    Content = ascii_trim(get_lines(S, Start, Next, Blk, false)),
                    Tag = <<"h", (integer_to_binary(Level))/binary>>,
                    S1 = push(heading_open, Tag, 1, #{markup => <<Marker>>},
                              S#b{line = Next + 1}),
                    S2 = push(inline, <<>>, 0, #{content => Content}, S1),
                    {true, push(heading_close, Tag, -1, #{markup => <<Marker>>}, S2)}
            end
    end.

lheading_scan(#b{src = Src, blk = Blk} = S, Next, End) ->
    case Next < End andalso not is_empty(S, Next) of
        false -> {Next, 0, 0};
        true ->
            case sc(S, Next) - Blk > 3 of
                true -> lheading_scan(S, Next + 1, End);
                false ->
                    Underline =
                        case sc(S, Next) >= Blk of
                            true ->
                                Pos = bm(S, Next) + ts(S, Next),
                                Max = em(S, Next),
                                M = c(Src, Pos),
                                case Pos < Max andalso (M =:= $- orelse M =:= $=) andalso
                                    skip_spaces(Src, skip_chars(Src, Pos, M)) >= Max of
                                    true -> M;
                                    false -> false
                                end;
                            false -> false
                        end,
                    case Underline of
                        $= -> {Next, 1, $=};
                        $- -> {Next, 2, $-};
                        false ->
                            case sc(S, Next) < 0 of
                                true -> lheading_scan(S, Next + 1, End);
                                false ->
                                    case terminates(?TERM_PARAGRAPH, S, Next, End) of
                                        true -> {Next, 0, 0};
                                        false -> lheading_scan(S, Next + 1, End)
                                    end
                            end
                    end
            end
    end.

%%% paragraph

r_paragraph(#b{blk = Blk} = S, Start, End) ->
    Next = paragraph_end(S#b{parent = paragraph}, Start + 1, End),
    Content = ascii_trim(get_lines(S, Start, Next, Blk, false)),
    S1 = push(paragraph_open, <<"p">>, 1, #{}, S#b{line = Next}),
    S2 = push(inline, <<>>, 0, #{content => Content}, S1),
    {true, push(paragraph_close, <<"p">>, -1, #{}, S2)}.

paragraph_end(#b{blk = Blk} = S, Next, End) ->
    case Next < End andalso not is_empty(S, Next) of
        false -> Next;
        true ->
            case sc(S, Next) - Blk > 3 orelse sc(S, Next) < 0 of
                true -> paragraph_end(S, Next + 1, End);
                false ->
                    case terminates(?TERM_PARAGRAPH, S, Next, End) of
                        true -> Next;
                        false -> paragraph_end(S, Next + 1, End)
                    end
            end
    end.

%%% table (GFM)

-define(MAX_AUTOCOMPLETED_CELLS, 16#10000).

get_line_text(#b{src = Src} = S, L) ->
    slice(Src, bm(S, L) + ts(S, L), em(S, L)).

r_table(#b{src = Src, blk = Blk} = S0, Start, End, Silent) ->
    Next = Start + 1,
    Ok = Start + 2 =< End andalso sc(S0, Next) >= Blk andalso sc(S0, Next) - Blk < 4
        andalso table_delim_line(Src, bm(S0, Next) + ts(S0, Next), em(S0, Next)),
    case Ok of
        false -> false;
        true ->
            case table_aligns(binary:split(get_line_text(S0, Next), <<"|">>, [global])) of
                false -> false;
                Aligns ->
                    Head = js_trim(get_line_text(S0, Start)),
                    Columns = trim_edges(escaped_split(Head)),
                    NCols = length(Columns),
                    case binary:match(Head, <<"|">>) =:= nomatch
                        orelse sc(S0, Start) - Blk >= 4
                        orelse NCols =:= 0 orelse NCols =/= length(Aligns) of
                        true -> false;
                        false when Silent -> {true, S0};
                        false ->
                            OldParent = S0#b.parent,
                            S1 = push(tr_open, <<"tr">>, 1, #{},
                                      push(thead_open, <<"thead">>, 1, #{},
                                           push(table_open, <<"table">>, 1, #{},
                                                S0#b{parent = table}))),
                            S2 = lists:foldl(
                                   fun({Col, Al}, Acc) ->
                                           cell(th_open, th_close, <<"th">>, js_trim(Col), Al, Acc)
                                   end, S1, lists:zip(Columns, Aligns)),
                            S3 = push(thead_close, <<"thead">>, -1, #{},
                                      push(tr_close, <<"tr">>, -1, #{}, S2)),
                            {NextLine, HasBody, S4} =
                                table_rows(S3, Start, Start + 2, End, Aligns, NCols, 0, false),
                            S5 = case HasBody of
                                     true -> push(tbody_close, <<"tbody">>, -1, #{}, S4);
                                     false -> S4
                                 end,
                            S6 = push(table_close, <<"table">>, -1, #{}, S5),
                            {true, S6#b{parent = OldParent, line = NextLine}}
                    end
            end
    end.

%% The delimiter row: | - : and spaces only, starting with | - or :.
table_delim_line(Src, Pos, Max) ->
    First = c(Src, Pos),
    Second = c(Src, Pos + 1),
    Pos < Max andalso (First =:= $| orelse First =:= $- orelse First =:= $:)
        andalso Pos + 1 < Max
        andalso (Second =:= $| orelse Second =:= $- orelse Second =:= $: orelse is_space(Second))
        andalso not (First =:= $- andalso is_space(Second))
        andalso lists:all(fun(Ch) -> Ch =:= $| orelse Ch =:= $- orelse Ch =:= $: orelse is_space(Ch) end,
                          binary_to_list(slice(Src, Pos + 2, Max))).

table_aligns(Columns) ->
    Last = length(Columns) - 1,
    table_aligns(lists:zip(lists:seq(0, Last), Columns), Last, []).

table_aligns([], _, Acc) -> lists:reverse(Acc);
table_aligns([{I, Col} | Rest], Last, Acc) ->
    case js_trim(Col) of
        <<>> when I =:= 0; I =:= Last -> table_aligns(Rest, Last, Acc);
        <<>> -> false;
        T ->
            case re:run(T, <<"^:?-+:?$">>) of
                nomatch -> false;
                _ ->
                    A = case {binary:first(T), binary:last(T)} of
                            {$:, $:} -> <<"center">>;
                            {_, $:} -> <<"right">>;
                            {$:, _} -> <<"left">>;
                            _ -> <<>>
                        end,
                    table_aligns(Rest, Last, [A | Acc])
            end
    end.

trim_edges(Cols0) ->
    Cols1 = case Cols0 of [<<>> | R] -> R; _ -> Cols0 end,
    case Cols1 =/= [] andalso lists:last(Cols1) =:= <<>> of
        true -> lists:droplast(Cols1);
        false -> Cols1
    end.

cell(Open, Close, Tag, Content, Align, S) ->
    Attrs = case Align of <<>> -> []; _ -> [{<<"style">>, <<"text-align:", Align/binary>>}] end,
    S1 = push(Open, Tag, 1, #{attrs => Attrs}, S),
    push(Close, Tag, -1, #{}, push(inline, <<>>, 0, #{content => Content}, S1)).

table_rows(S, _Start, Next, End, _Aligns, _NCols, _Auto, HasBody) when Next >= End ->
    {Next, HasBody, S};
table_rows(#b{blk = Blk} = S, Start, Next, End, Aligns, NCols, Auto, HasBody) ->
    Stop = sc(S, Next) < Blk orelse terminates(?TERM_BLOCKQUOTE, S, Next, End),
    Text = case Stop of true -> <<>>; false -> js_trim(get_line_text(S, Next)) end,
    case Stop orelse Text =:= <<>> orelse sc(S, Next) - Blk >= 4 of
        true -> {Next, HasBody, S};
        false ->
            Columns = trim_edges(escaped_split(Text)),
            Auto1 = Auto + NCols - length(Columns),
            case Auto1 > ?MAX_AUTOCOMPLETED_CELLS of
                true -> {Next, HasBody, S};
                false ->
                    S1 = case Next =:= Start + 2 of
                             true -> push(tbody_open, <<"tbody">>, 1, #{}, S);
                             false -> S
                         end,
                    S2 = push(tr_open, <<"tr">>, 1, #{}, S1),
                    Padded = Columns ++ lists:duplicate(max(0, NCols - length(Columns)), <<>>),
                    S3 = lists:foldl(
                           fun({Col, Al}, Acc) ->
                                   cell(td_open, td_close, <<"td">>, js_trim(Col), Al, Acc)
                           end, S2, lists:zip(lists:sublist(Padded, NCols), Aligns)),
                    S4 = push(tr_close, <<"tr">>, -1, #{}, S3),
                    table_rows(S4, Start, Next + 1, End, Aligns, NCols, Auto1,
                               HasBody orelse Next =:= Start + 2)
            end
    end.

%% Split a row at | not preceded by a backslash; \| becomes |.
escaped_split(Str) -> escaped_split(Str, 0, 0, false, <<>>, []).

escaped_split(Str, Pos, LastPos, IsEscaped, Current, Acc) when Pos < byte_size(Str) ->
    Ch = binary:at(Str, Pos),
    {LastPos1, Current1, Acc1} =
        case Ch of
            $| when not IsEscaped ->
                {Pos + 1, <<>>, [<<Current/binary, (slice(Str, LastPos, Pos))/binary>> | Acc]};
            $| ->
                {Pos, <<Current/binary, (slice(Str, LastPos, Pos - 1))/binary>>, Acc};
            _ -> {LastPos, Current, Acc}
        end,
    escaped_split(Str, Pos + 1, LastPos1, Ch =:= $\\, Current1, Acc1);
escaped_split(Str, _Pos, LastPos, _, Current, Acc) ->
    lists:reverse([<<Current/binary, (slice(Str, LastPos))/binary>> | Acc]).

%%%===================================================================
%%% Inline parser (ParserInline)
%%%===================================================================

inline_parse(Src, Refs) ->
    S0 = tokenize_inline(#i{src = Src, max = byte_size(Src), refs = Refs}),
    Lists = [S0#i.dl | lists:reverse(S0#i.metas)],
    S1 = lists:foldl(fun(Id, Acc) -> balance_pairs(Acc, Id) end, S0, Lists),
    S2 = strike_post(S1, Lists),
    S3 = lists:foldl(fun(Id, Acc) -> emphasis_post(Acc, Id) end, S2, Lists),
    fragments_join([maps:get(K, S3#i.toks) || K <- lists:seq(0, S3#i.ntok - 1)]).

-define(INLINE_RULES, [text, newline, escape, backticks, strikethrough, emphasis, link,
                       image, autolink, entity]).

tokenize_inline(#i{max = End} = S) ->
    S1 = tokenize_inline(S, End),
    case S1#i.pending of
        <<>> -> S1;
        _ -> push_pending(S1)
    end.

tokenize_inline(#i{pos = Pos} = S, End) when Pos < End ->
    Res = case S#i.level < ?MAX_NESTING of
              true -> try_inline(?INLINE_RULES, S, false);
              false -> {false, S}
          end,
    case Res of
        {true, S1} when S1#i.pos >= End -> S1;
        {true, S1} -> tokenize_inline(S1, End);
        {false, S1} ->
            #i{src = Src, pos = P, pending = Pend} = S1,
            tokenize_inline(S1#i{pending = <<Pend/binary, (binary:at(Src, P))>>, pos = P + 1},
                            End)
    end;
tokenize_inline(S, _) -> S.

try_inline([], S, _) -> {false, S};
try_inline([R | Rs], S, Silent) ->
    case inline_rule(R, S, Silent) of
        {true, S1} -> {true, S1};
        {false, S1} -> try_inline(Rs, S1, Silent)
    end.

inline_rule(text, S, Silent) -> i_text(S, Silent);
inline_rule(newline, S, Silent) -> i_newline(S, Silent);
inline_rule(escape, S, Silent) -> i_escape(S, Silent);
inline_rule(backticks, S, Silent) -> i_backticks(S, Silent);
inline_rule(strikethrough, S, Silent) -> i_strikethrough(S, Silent);
inline_rule(emphasis, S, Silent) -> i_emphasis(S, Silent);
inline_rule(link, S, Silent) -> i_link(S, Silent);
inline_rule(image, S, Silent) -> i_image(S, Silent);
inline_rule(autolink, S, Silent) -> i_autolink(S, Silent);
inline_rule(entity, S, Silent) -> i_entity(S, Silent).

%% ParserInline.skipToken: where the token at pos ends (memoised).
skip_token(#i{pos = Pos, cache = Cache} = S) ->
    case Cache of
        #{Pos := P} -> S#i{pos = P};
        _ ->
            {Ok, S1} =
                case S#i.level < ?MAX_NESTING of
                    true -> skip_rules(?INLINE_RULES, S);
                    false -> {false, S#i{pos = S#i.max}}
                end,
            S2 = case Ok of true -> S1; false -> S1#i{pos = S1#i.pos + 1} end,
            S2#i{cache = maps:put(Pos, S2#i.pos, S2#i.cache)}
    end.

skip_rules([], S) -> {false, S};
skip_rules([R | Rs], #i{level = L} = S) ->
    case inline_rule(R, S#i{level = L + 1}, true) of
        {true, S1} -> {true, S1#i{level = L}};
        {false, S1} -> skip_rules(Rs, S1#i{level = L})
    end.

push_pending(#i{pending = P, plevel = L, toks = T, ntok = N} = S) ->
    Tok = (tok(text, <<>>, 0))#{content := P, level := L},
    S#i{toks = T#{N => Tok}, ntok = N + 1, pending = <<>>}.

push_i(Type, Tag, Nesting, Extra, S0) ->
    S1 = case S0#i.pending of <<>> -> S0; _ -> push_pending(S0) end,
    S2 = case Nesting < 0 of
             true ->
                 [D | Prev] = S1#i.prev,
                 S1#i{level = S1#i.level - 1, dl = D, prev = Prev};
             false -> S1
         end,
    Tok = maps:merge((tok(Type, Tag, Nesting))#{level := S2#i.level}, Extra),
    S3 = case Nesting > 0 of
             true ->
                 Id = S2#i.next_dl,
                 S2#i{level = S2#i.level + 1, prev = [S2#i.dl | S2#i.prev], dl = Id,
                      next_dl = Id + 1, dls = maps:put(Id, {#{}, 0}, S2#i.dls),
                      metas = [Id | S2#i.metas]};
             false -> S2
         end,
    S3#i{plevel = S3#i.level, toks = maps:put(S3#i.ntok, Tok, S3#i.toks),
         ntok = S3#i.ntok + 1}.

add_delim(D, #i{dl = Id, dls = Dls} = S) ->
    {M, N} = maps:get(Id, Dls),
    S#i{dls = Dls#{Id := {M#{N => D}, N + 1}}}.

%% StateInline.scanDelims
scan_delims(#i{src = Src, max = Max}, Start, CanSplitWord) ->
    Marker = c(Src, Start),
    LastChar = case Start of 0 -> 16#20; _ -> prev_cp(Src, Start) end,
    Pos = count_marker(Src, Start, Max, Marker),
    Count = Pos - Start,
    NextChar = case Pos < Max of true -> cp_at(Src, Pos); false -> 16#20 end,
    LastPunct = is_punct(LastChar),
    NextPunct = is_punct(NextChar),
    LastWS = is_ws(LastChar),
    NextWS = is_ws(NextChar),
    Left = not NextWS andalso (not NextPunct orelse LastWS orelse LastPunct),
    Right = not LastWS andalso (not LastPunct orelse NextWS orelse NextPunct),
    CanOpen = Left andalso (CanSplitWord orelse not Right orelse LastPunct),
    CanClose = Right andalso (CanSplitWord orelse not Left orelse NextPunct),
    {CanOpen, CanClose, Count}.

count_marker(Src, P, Max, Marker) ->
    case P < Max andalso c(Src, P) =:= Marker of
        true -> count_marker(Src, P + 1, Max, Marker);
        false -> P
    end.

%%% text

i_text(#i{src = Src, pos = Pos, max = Max} = S, Silent) ->
    case text_end(Src, Pos, Max) of
        Pos -> {false, S};
        P when Silent -> {true, S#i{pos = P}};
        P -> {true, S#i{pos = P, pending = <<(S#i.pending)/binary, (slice(Src, Pos, P))/binary>>}}
    end.

text_end(Src, P, Max) when P < Max ->
    case is_terminator(binary:at(Src, P)) of
        true -> P;
        false -> text_end(Src, P + 1, Max)
    end;
text_end(_, P, _) -> P.

is_terminator(C) ->
    lists:member(C, "\n!#$%&*+-:<=>@[\\]^_`{}~").

%%% newline

i_newline(#i{src = Src, pos = Pos, max = Max} = S0, Silent) ->
    case c(Src, Pos) of
        $\n ->
            S1 = case Silent of
                     true -> S0;
                     false ->
                         P = S0#i.pending,
                         PMax = byte_size(P) - 1,
                         case PMax >= 0 andalso binary:at(P, PMax) =:= $\s of
                             true ->
                                 case PMax >= 1 andalso binary:at(P, PMax - 1) =:= $\s of
                                     true ->
                                         Ws = trailing_ws(P, PMax - 1),
                                         push_i(hardbreak, <<"br">>, 0, #{},
                                                S0#i{pending = binary:part(P, 0, Ws)});
                                     false ->
                                         push_i(softbreak, <<"br">>, 0, #{},
                                                S0#i{pending = binary:part(P, 0, PMax)})
                                 end;
                             false -> push_i(softbreak, <<"br">>, 0, #{}, S0)
                         end
                 end,
            {true, S1#i{pos = skip_spaces_max(Src, Pos + 1, Max)}};
        _ -> {false, S0}
    end.

trailing_ws(P, Ws) when Ws >= 1 ->
    case binary:at(P, Ws - 1) of $\s -> trailing_ws(P, Ws - 1); _ -> Ws end;
trailing_ws(_, Ws) -> Ws.

skip_spaces_max(Src, P, Max) ->
    case P < Max andalso is_space(c(Src, P)) of
        true -> skip_spaces_max(Src, P + 1, Max);
        false -> P
    end.

%%% escape

i_escape(#i{src = Src, pos = Pos0, max = Max} = S, Silent) ->
    Pos = Pos0 + 1,
    case c(Src, Pos0) =:= $\\ andalso Pos < Max of
        false -> {false, S};
        true ->
            case c(Src, Pos) of
                $\n ->
                    S1 = case Silent of
                             true -> S;
                             false -> push_i(hardbreak, <<"br">>, 0, #{}, S)
                         end,
                    {true, S1#i{pos = skip_spaces_max(Src, Pos + 1, Max)}};
                $\s ->
                    S1 = case Silent of
                             true -> S;
                             false -> push_i(text_special, <<>>, 0,
                                             #{content => <<"\\">>, markup => <<"\\">>,
                                               info => <<"escape">>}, S)
                         end,
                    {true, S1#i{pos = Pos}};
                Ch ->
                    %% one byte, as markdown-it takes one UTF-16 unit: the
                    %% rest of a multi-byte character follows as text and
                    %% text_join puts it back together
                    Orig = <<"\\", Ch>>,
                    S1 = case Silent of
                             true -> S;
                             false ->
                                 Content = case lists:member(Ch, "\\!\"#$%&'()*+,./:;<=>?@[]^_`{|}~-") of
                                               true -> <<Ch>>;
                                               false -> Orig
                                           end,
                                 push_i(text_special, <<>>, 0,
                                        #{content => Content, markup => Orig,
                                          info => <<"escape">>}, S)
                         end,
                    {true, S1#i{pos = Pos + 1}}
            end
    end.

%%% backticks

i_backticks(#i{src = Src, pos = Start, max = Max} = S, Silent) ->
    case c(Src, Start) of
        $` ->
            Pos = count_marker(Src, Start + 1, Max, $`),
            Marker = slice(Src, Start, Pos),
            OpenLen = Pos - Start,
            case S#i.bt_scanned andalso maps:get(OpenLen, S#i.bt, 0) =< Start of
                true -> {true, bt_literal(S, Marker, Silent)};
                false -> bt_match(S, Start, Pos, Pos, Marker, OpenLen, Silent)
            end;
        _ -> {false, S}
    end.

bt_literal(#i{pos = P, pending = Pend} = S, Marker, Silent) ->
    S1 = case Silent of true -> S; false -> S#i{pending = <<Pend/binary, Marker/binary>>} end,
    S1#i{pos = P + byte_size(Marker)}.

bt_match(#i{src = Src, max = Max} = S, Start, Pos, MatchEnd0, Marker, OpenLen, Silent) ->
    case binary:match(Src, <<"`">>, [{scope, {MatchEnd0, byte_size(Src) - MatchEnd0}}]) of
        nomatch ->
            {true, bt_literal(S#i{bt_scanned = true}, Marker, Silent)};
        {MatchStart, _} ->
            MatchEnd = count_marker(Src, MatchStart + 1, Max, $`),
            CloseLen = MatchEnd - MatchStart,
            case CloseLen =:= OpenLen of
                true ->
                    S1 = case Silent of
                             true -> S;
                             false ->
                                 C0 = binary:replace(slice(Src, Pos, MatchStart), <<"\n">>,
                                                     <<" ">>, [global]),
                                 push_i(code_inline, <<"code">>, 0,
                                        #{markup => Marker, content => strip_one_space(C0)}, S)
                         end,
                    {true, S1#i{pos = MatchEnd}};
                false ->
                    bt_match(S#i{bt = maps:put(CloseLen, MatchStart, S#i.bt)}, Start, Pos,
                             MatchEnd, Marker, OpenLen, Silent)
            end
    end.

%% .replace(/^ (.+) $/, '$1'): `.' does not match line terminators.
strip_one_space(<<" ", Rest/binary>> = C) when byte_size(Rest) >= 2 ->
    Inner = binary:part(Rest, 0, byte_size(Rest) - 1),
    case binary:last(Rest) =:= $\s andalso
        binary:match(Inner, [<<"\n">>, <<"\r">>, <<16#2028/utf8>>, <<16#2029/utf8>>]) =:= nomatch of
        true -> Inner;
        false -> C
    end;
strip_one_space(C) -> C.

%%% strikethrough

i_strikethrough(S, true) -> {false, S};
i_strikethrough(#i{src = Src, pos = Start} = S, false) ->
    case c(Src, Start) of
        $~ ->
            {CanOpen, CanClose, Len} = scan_delims(S, Start, true),
            case Len < 2 of
                true -> {false, S};
                false ->
                    S1 = case Len rem 2 of
                             1 -> push_i(text, <<>>, 0, #{content => <<"~">>}, S);
                             0 -> S
                         end,
                    S2 = lists:foldl(
                           fun(_, Acc) ->
                                   Acc1 = push_i(text, <<>>, 0, #{content => <<"~~">>}, Acc),
                                   add_delim(#{marker => $~, length => 0, token => Acc1#i.ntok - 1,
                                               'end' => -1, open => CanOpen, close => CanClose},
                                             Acc1)
                           end, S1, lists:seq(1, Len div 2)),
                    {true, S2#i{pos = Start + Len}}
            end;
        _ -> {false, S}
    end.

%%% emphasis

i_emphasis(S, true) -> {false, S};
i_emphasis(#i{src = Src, pos = Start} = S, false) ->
    case c(Src, Start) of
        M when M =:= $_; M =:= $* ->
            {CanOpen, CanClose, Len} = scan_delims(S, Start, M =:= $*),
            S1 = lists:foldl(
                   fun(_, Acc) ->
                           Acc1 = push_i(text, <<>>, 0, #{content => <<M>>}, Acc),
                           add_delim(#{marker => M, length => Len, token => Acc1#i.ntok - 1,
                                       'end' => -1, open => CanOpen, close => CanClose}, Acc1)
                   end, S, lists:seq(1, Len)),
            {true, S1#i{pos = Start + Len}};
        _ -> {false, S}
    end.

%%% link

i_link(#i{src = Src, pos = OldPos, max = Max} = S0, Silent) ->
    case c(Src, OldPos) of
        $[ ->
            LabelStart = OldPos + 1,
            {LabelEnd, S1} = parse_link_label(S0, OldPos, true),
            case LabelEnd < 0 of
                true -> {false, S1};
                false -> link_rest(S1, OldPos, Max, LabelStart, LabelEnd, Silent)
            end;
        _ -> {false, S0}
    end.

link_rest(#i{src = Src} = S1, OldPos, Max, LabelStart, LabelEnd, Silent) ->
    Pos0 = LabelEnd + 1,
    Inline =
        case Pos0 < Max andalso c(Src, Pos0) =:= $( of
            false -> {ref, Pos0, <<>>, <<>>};
            true ->
                P1 = skip_ws_nl(Src, Pos0 + 1, Max),
                case P1 >= Max of
                    true -> fail;
                    false ->
                        {P4, Href0, Title0} =
                            case parse_link_destination(Src, P1, Max) of
                                error -> {P1, <<>>, <<>>};
                                {ok, DPos, DStr} ->
                                    H0 = normalize_link(DStr),
                                    {P2, H} = case validate_link(H0) of
                                                  true -> {DPos, H0};
                                                  false -> {P1, <<>>}
                                              end,
                                    P3 = skip_ws_nl(Src, P2, Max),
                                    Res = parse_link_title(Src, P3, Max, undefined),
                                    case P3 < Max andalso P2 =/= P3 andalso maps:get(ok, Res) of
                                        true ->
                                            {skip_ws_nl(Src, maps:get(pos, Res), Max), H,
                                             maps:get(str, Res)};
                                        false -> {P3, H, <<>>}
                                    end
                            end,
                        case P4 >= Max orelse c(Src, P4) =/= $) of
                            true -> {ref, P4 + 1, Href0, Title0};
                            false -> {inline, P4 + 1, Href0, Title0}
                        end
                end
        end,
    case Inline of
        fail -> {false, S1};
        {inline, Pos, Href, Title} ->
            link_emit(S1, Pos, Max, LabelStart, LabelEnd, Href, Title, Silent);
        {ref, _, _, _} when S1#i.refs =:= undefined -> {false, S1};
        {ref, Pos, _, _} ->
            {Label, Pos1, S2} = ref_label_inline(S1, Pos, Max, LabelStart, LabelEnd),
            case maps:find(normalize_reference(Label), S2#i.refs) of
                error -> {false, S2#i{pos = OldPos}};
                {ok, {Href, Title}} ->
                    link_emit(S2, Pos1, Max, LabelStart, LabelEnd, Href, Title, Silent)
            end
    end.

%% A reference link's label: [text][label], [text][] or [text].
ref_label_inline(#i{src = Src} = S, Pos, Max, LabelStart, LabelEnd) ->
    {Label0, Pos1, S1} =
        case Pos < Max andalso c(Src, Pos) =:= $[ of
            true ->
                case parse_link_label(S, Pos, false) of
                    {P, S2} when P >= 0 -> {slice(Src, Pos + 1, P), P + 1, S2};
                    {_, S2} -> {<<>>, LabelEnd + 1, S2}
                end;
            false -> {<<>>, LabelEnd + 1, S}
        end,
    Label = case Label0 of <<>> -> slice(Src, LabelStart, LabelEnd); _ -> Label0 end,
    {Label, Pos1, S1}.

link_emit(S, Pos, Max, _LabelStart, _LabelEnd, _Href, _Title, true) ->
    {true, S#i{pos = Pos, max = Max}};
link_emit(S0, Pos, Max, LabelStart, LabelEnd, Href, Title, false) ->
    Attrs = [{<<"href">>, Href} | [{<<"title">>, Title} || Title =/= <<>>]],
    S1 = push_i(link_open, <<"a">>, 1, #{attrs => Attrs},
                S0#i{pos = LabelStart, max = LabelEnd}),
    S2 = tokenize_inline(S1#i{link_level = S1#i.link_level + 1}),
    S3 = push_i(link_close, <<"a">>, -1, #{}, S2#i{link_level = S2#i.link_level - 1}),
    {true, S3#i{pos = Pos, max = Max}}.

skip_ws_nl(Src, P, Max) ->
    case P < Max andalso (is_space(c(Src, P)) orelse c(Src, P) =:= $\n) of
        true -> skip_ws_nl(Src, P + 1, Max);
        false -> P
    end.

%% helpers/parse_link_label: the position of the closing ], or -1.
parse_link_label(#i{pos = OldPos, max = Max} = S0, Start, DisableNested) ->
    case pll(S0#i{pos = Start + 1}, 1, Max, DisableNested) of
        {found, S1} -> {S1#i.pos, S1#i{pos = OldPos}};
        {notfound, S1} -> {-1, S1#i{pos = OldPos}}
    end.

pll(#i{src = Src, pos = Pos} = S, Level, Max, DisableNested) when Pos < Max ->
    Marker = c(Src, Pos),
    Level1 = case Marker of $] -> Level - 1; _ -> Level end,
    case Marker =:= $] andalso Level1 =:= 0 of
        true -> {found, S};
        false ->
            S1 = skip_token(S),
            case Marker of
                $[ when Pos =:= S1#i.pos - 1 -> pll(S1, Level1 + 1, Max, DisableNested);
                $[ when DisableNested -> {notfound, S1};
                _ -> pll(S1, Level1, Max, DisableNested)
            end
    end;
pll(S, _, _, _) -> {notfound, S}.

%% helpers/parse_link_destination
parse_link_destination(Str, Start, Max) ->
    case c(Str, Start) of
        $< -> pld_angle(Str, Start, Start + 1, Max);
        _ -> pld_plain(Str, Start, Start, Max, 0)
    end.

pld_angle(Str, Start, Pos, Max) when Pos < Max ->
    case c(Str, Pos) of
        $\n -> error;
        $< -> error;
        $> -> {ok, Pos + 1, unescape_all(slice(Str, Start + 1, Pos))};
        $\\ when Pos + 1 < Max -> pld_angle(Str, Start, Pos + 2, Max);
        _ -> pld_angle(Str, Start, Pos + 1, Max)
    end;
pld_angle(_, _, _, _) -> error.

pld_plain(Str, Start, Pos, Max, Level) when Pos < Max ->
    Code = c(Str, Pos),
    if
        Code =:= $\s -> pld_done(Str, Start, Pos, Level);
        Code < 16#20; Code =:= 16#7F -> pld_done(Str, Start, Pos, Level);
        Code =:= $\\, Pos + 1 < Max ->
            case c(Str, Pos + 1) of
                $\s -> pld_plain(Str, Start, Pos + 1, Max, Level);
                _ -> pld_plain(Str, Start, Pos + 2, Max, Level)
            end;
        Code =:= $( ->
            case Level + 1 > 32 of
                true -> error;
                false -> pld_plain(Str, Start, Pos + 1, Max, Level + 1)
            end;
        Code =:= $), Level =:= 0 -> pld_done(Str, Start, Pos, Level);
        Code =:= $) -> pld_plain(Str, Start, Pos + 1, Max, Level - 1);
        true -> pld_plain(Str, Start, Pos + 1, Max, Level)
    end;
pld_plain(Str, Start, Pos, _, Level) -> pld_done(Str, Start, Pos, Level).

pld_done(_, Start, Start, _) -> error;
pld_done(_, _, _, Level) when Level =/= 0 -> error;
pld_done(Str, Start, Pos, _) -> {ok, Pos, unescape_all(slice(Str, Start, Pos))}.

%% helpers/parse_link_title (Prev: the state of a title continued from
%% the previous line).
parse_link_title(Str, Start0, Max, Prev) ->
    Init = case Prev of
               undefined ->
                   case Start0 >= Max of
                       true -> stop;
                       false ->
                           case c(Str, Start0) of
                               M when M =:= $"; M =:= $' ->
                                   {Start0 + 1, Start0 + 1, <<>>, M};
                               $( -> {Start0 + 1, Start0 + 1, <<>>, $)};
                               _ -> stop
                           end
                   end;
               #{str := PStr, marker := PM} -> {Start0, Start0, PStr, PM}
           end,
    case Init of
        stop -> #{ok => false, can_continue => false, pos => 0, str => <<>>, marker => 0};
        {Start, Pos, Acc, Marker} -> plt(Str, Start, Pos, Max, Acc, Marker)
    end.

plt(Str, Start, Pos, Max, Acc, Marker) when Pos < Max ->
    case c(Str, Pos) of
        Marker ->
            #{ok => true, can_continue => false, pos => Pos + 1, marker => Marker,
              str => <<Acc/binary, (unescape_all(slice(Str, Start, Pos)))/binary>>};
        $( when Marker =:= $) ->
            #{ok => false, can_continue => false, pos => 0, str => Acc, marker => Marker};
        $\\ when Pos + 1 < Max -> plt(Str, Start, Pos + 2, Max, Acc, Marker);
        _ -> plt(Str, Start, Pos + 1, Max, Acc, Marker)
    end;
plt(Str, Start, Pos, _, Acc, Marker) ->
    #{ok => false, can_continue => true, pos => 0, marker => Marker,
      str => <<Acc/binary, (unescape_all(slice(Str, Start, Pos)))/binary>>}.

%%% image

i_image(#i{src = Src, pos = OldPos, max = Max} = S0, Silent) ->
    case c(Src, OldPos) =:= $! andalso c(Src, OldPos + 1) =:= $[ of
        false -> {false, S0};
        true ->
            LabelStart = OldPos + 2,
            {LabelEnd, S1} = parse_link_label(S0, OldPos + 1, false),
            case LabelEnd < 0 of
                true -> {false, S1};
                false -> image_rest(S1, OldPos, Max, LabelStart, LabelEnd, Silent)
            end
    end.

image_rest(#i{src = Src} = S1, OldPos, Max, LabelStart, LabelEnd, Silent) ->
    Pos0 = LabelEnd + 1,
    Found =
        case Pos0 < Max andalso c(Src, Pos0) =:= $( of
            true ->
                P1 = skip_ws_nl(Src, Pos0 + 1, Max),
                case P1 >= Max of
                    true -> {fail, S1};
                    false ->
                        {P2, Href0} =
                            case parse_link_destination(Src, P1, Max) of
                                error -> {P1, <<>>};
                                {ok, DPos, DStr} ->
                                    H0 = normalize_link(DStr),
                                    case validate_link(H0) of
                                        true -> {DPos, H0};
                                        false -> {P1, <<>>}
                                    end
                            end,
                        P3 = skip_ws_nl(Src, P2, Max),
                        Res = parse_link_title(Src, P3, Max, undefined),
                        {P4, Title0} =
                            case P3 < Max andalso P2 =/= P3 andalso maps:get(ok, Res) of
                                true -> {skip_ws_nl(Src, maps:get(pos, Res), Max),
                                         maps:get(str, Res)};
                                false -> {P3, <<>>}
                            end,
                        case P4 >= Max orelse c(Src, P4) =/= $) of
                            true -> {fail, S1#i{pos = OldPos}};
                            false -> {ok, P4 + 1, Href0, Title0, S1}
                        end
                end;
            false when S1#i.refs =:= undefined -> {fail, S1};
            false ->
                {Label, Pos1, SR} = ref_label_inline(S1, Pos0, Max, LabelStart, LabelEnd),
                case maps:find(normalize_reference(Label), SR#i.refs) of
                    error -> {fail, SR#i{pos = OldPos}};
                    {ok, {RHref, RTitle}} -> {ok, Pos1, RHref, RTitle, SR}
                end
        end,
    case Found of
        {fail, S} -> {false, S};
        {ok, Pos, Href, Title, S2} ->
            S3 = case Silent of
                     true -> S2;
                     false ->
                         Content = slice(Src, LabelStart, LabelEnd),
                         Children = inline_parse(Content, S2#i.refs),
                         Attrs = [{<<"src">>, Href}, {<<"alt">>, <<>>}
                                  | [{<<"title">>, Title} || Title =/= <<>>]],
                         push_i(image, <<"img">>, 0,
                                #{attrs => Attrs, children => Children, content => Content}, S2)
                 end,
            {true, S3#i{pos = Pos, max = Max}}
    end.

%%% autolink

i_autolink(#i{src = Src, pos = Start, max = Max} = S, Silent) ->
    case c(Src, Start) of
        $< ->
            case autolink_end(Src, Start + 1, Max) of
                -1 -> {false, S};
                End ->
                    Url = slice(Src, Start + 1, End),
                    Full = case re:run(Url, <<"^([a-zA-Z][a-zA-Z0-9+.\\-]{1,31}):([^<>\\x00-\\x20]*)$">>) of
                               nomatch ->
                                   case re:run(Url, <<"^([a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9]"
                                                      "(?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9]"
                                                      "(?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*)$">>) of
                                       nomatch -> false;
                                       _ -> normalize_link(<<"mailto:", Url/binary>>)
                                   end;
                               _ -> normalize_link(Url)
                           end,
                    case Full =/= false andalso validate_link(Full) of
                        false -> {false, S};
                        true ->
                            S1 = case Silent of
                                     true -> S;
                                     false ->
                                         Auto = #{markup => <<"autolink">>, info => <<"auto">>},
                                         A = push_i(link_open, <<"a">>, 1,
                                                    Auto#{attrs => [{<<"href">>, Full}]}, S),
                                         B = push_i(text, <<>>, 0,
                                                    #{content => normalize_link_text(Url)}, A),
                                         push_i(link_close, <<"a">>, -1, Auto, B)
                                 end,
                            {true, S1#i{pos = Start + byte_size(Url) + 2}}
                    end
            end;
        _ -> {false, S}
    end.

autolink_end(Src, P, Max) when P < Max ->
    case c(Src, P) of
        $< -> -1;
        $> -> P;
        _ -> autolink_end(Src, P + 1, Max)
    end;
autolink_end(_, _, _) -> -1.

%%% entity

i_entity(#i{src = Src, pos = Pos, max = Max} = S, Silent) ->
    case c(Src, Pos) =:= $& andalso Pos + 1 < Max of
        false -> {false, S};
        true ->
            Rest = slice(Src, Pos),
            case c(Src, Pos + 1) of
                $# ->
                    case re:run(Rest, <<"^&#((?:x[a-f0-9]{1,6}|[0-9]{1,7}));">>,
                                [caseless, {capture, [0, 1], binary}]) of
                        {match, [Match, Num]} ->
                            Code = case Num of
                                       <<X, Hex/binary>> when X =:= $x; X =:= $X ->
                                           binary_to_integer(Hex, 16);
                                       _ -> binary_to_integer(Num)
                                   end,
                            Text = case is_valid_entity_code(Code) of
                                       true -> <<Code/utf8>>;
                                       false -> <<16#FFFD/utf8>>
                                   end,
                            entity_token(S, Match, Text, Silent);
                        nomatch -> {false, S}
                    end;
                _ ->
                    case re:run(Rest, <<"^&([a-z][a-z0-9]{1,31});">>,
                                [caseless, {capture, [0, 1], binary}]) of
                        {match, [Match, Name]} ->
                            case ?D:entity(Name) of
                                undefined -> {false, S};
                                Text -> entity_token(S, Match, Text, Silent)
                            end;
                        nomatch -> {false, S}
                    end
            end
    end.

entity_token(#i{pos = Pos} = S, Match, Text, Silent) ->
    S1 = case Silent of
             true -> S;
             false -> push_i(text_special, <<>>, 0,
                             #{content => Text, markup => Match, info => <<"entity">>}, S)
         end,
    {true, S1#i{pos = Pos + byte_size(Match)}}.

is_valid_entity_code(C) ->
    not ((C >= 16#D800 andalso C =< 16#DFFF)
         orelse (C >= 16#FDD0 andalso C =< 16#FDEF)
         orelse (C band 16#FFFF) =:= 16#FFFF orelse (C band 16#FFFF) =:= 16#FFFE
         orelse (C >= 0 andalso C =< 8) orelse C =:= 16#0B
         orelse (C >= 16#0E andalso C =< 16#1F)
         orelse (C >= 16#7F andalso C =< 16#9F)
         orelse C > 16#10FFFF).

%%%===================================================================
%%% Inline post-processing (ruler2)
%%%===================================================================

dl_get(S, Id) -> maps:get(Id, S#i.dls).

%% balance_pairs: match emphasis/strikethrough openers with closers.
balance_pairs(S, Id) ->
    {Ds, N} = dl_get(S, Id),
    case N of
        0 -> S;
        _ ->
            Ds1 = bp(Ds, 0, N, 0, -2, #{}, #{}),
            S#i{dls = maps:put(Id, {Ds1, N}, S#i.dls)}
    end.

bp(Ds, CloserIdx, N, _HeaderIdx, _LastTok, _Bottom, _Jumps) when CloserIdx >= N -> Ds;
bp(Ds, CloserIdx, N, HeaderIdx0, LastTok0, Bottom, Jumps0) ->
    Closer = maps:get(CloserIdx, Ds),
    Jumps = Jumps0#{CloserIdx => 0},
    #{marker := CM, token := CT} = Closer,
    HeaderIdx = case maps:get(marker, maps:get(HeaderIdx0, Ds)) =/= CM
                    orelse LastTok0 =/= CT - 1 of
                    true -> CloserIdx;
                    false -> HeaderIdx0
                end,
    LastTok = CT,
    case maps:get(close, Closer) of
        false -> bp(Ds, CloserIdx + 1, N, HeaderIdx, LastTok, Bottom, Jumps);
        true ->
            Bottoms = maps:get(CM, Bottom, {-1, -1, -1, -1, -1, -1}),
            Slot = case maps:get(open, Closer) of true -> 3; false -> 0 end
                + maps:get(length, Closer) rem 3,
            MinOpener = element(Slot + 1, Bottoms),
            OpenerIdx = HeaderIdx - maps:get(HeaderIdx, Jumps) - 1,
            case bp_find(Ds, Jumps, Closer, CloserIdx, OpenerIdx, MinOpener) of
                {matched, Ds1, Jumps1} ->
                    bp(Ds1, CloserIdx + 1, N, HeaderIdx, -2, Bottom, Jumps1);
                nomatch when OpenerIdx =:= -1 ->
                    bp(Ds, CloserIdx + 1, N, HeaderIdx, LastTok, Bottom, Jumps);
                nomatch ->
                    Bottom1 = Bottom#{CM => setelement(Slot + 1, Bottoms, OpenerIdx)},
                    bp(Ds, CloserIdx + 1, N, HeaderIdx, LastTok, Bottom1, Jumps)
            end
    end.

bp_find(Ds, Jumps, Closer, CloserIdx, OpenerIdx, MinOpener) when OpenerIdx > MinOpener ->
    Opener = maps:get(OpenerIdx, Ds),
    Next = OpenerIdx - maps:get(OpenerIdx, Jumps) - 1,
    case maps:get(marker, Opener) =:= maps:get(marker, Closer) andalso
        maps:get(open, Opener) andalso maps:get('end', Opener) < 0 of
        false -> bp_find(Ds, Jumps, Closer, CloserIdx, Next, MinOpener);
        true ->
            OL = maps:get(length, Opener),
            CL = maps:get(length, Closer),
            IsOddMatch = (maps:get(close, Opener) orelse maps:get(open, Closer))
                andalso (OL + CL) rem 3 =:= 0
                andalso (OL rem 3 =/= 0 orelse CL rem 3 =/= 0),
            case IsOddMatch of
                true -> bp_find(Ds, Jumps, Closer, CloserIdx, Next, MinOpener);
                false ->
                    LastJump = case OpenerIdx > 0 andalso
                                   not maps:get(open, maps:get(OpenerIdx - 1, Ds)) of
                                   true -> maps:get(OpenerIdx - 1, Jumps) + 1;
                                   false -> 0
                               end,
                    Jumps1 = Jumps#{CloserIdx := CloserIdx - OpenerIdx + LastJump,
                                    OpenerIdx := LastJump},
                    Ds1 = Ds#{CloserIdx := Closer#{open := false},
                              OpenerIdx := Opener#{'end' := CloserIdx, close := false}},
                    {matched, Ds1, Jumps1}
            end
    end;
bp_find(_, _, _, _, _, _) -> nomatch.

tok_get(S, I) -> maps:get(I, S#i.toks).
tok_put(S, I, T) -> S#i{toks = maps:put(I, T, S#i.toks)}.

retag(T, Type, Tag, Nesting, Markup) ->
    T#{type := Type, tag := Tag, nesting := Nesting, markup := Markup, content := <<>>}.

%% strikethrough's postProcess, over every delimiter list in turn.
strike_post(S, Lists) ->
    lists:foldl(fun(Id, Acc) -> strike_list(Acc, Id) end, S, Lists).

strike_list(S0, Id) ->
    {Ds, N} = dl_get(S0, Id),
    {S1, Lone} =
        lists:foldl(
          fun(I, {S, LoneAcc}) ->
                  D = maps:get(I, Ds),
                  case maps:get(marker, D) =:= $~ andalso maps:get('end', D) =/= -1 of
                      false -> {S, LoneAcc};
                      true ->
                          E = maps:get(maps:get('end', D), Ds),
                          ST = maps:get(token, D),
                          ET = maps:get(token, E),
                          Sa = tok_put(S, ST, retag(tok_get(S, ST), s_open, <<"s">>, 1, <<"~~">>)),
                          Sb = tok_put(Sa, ET, retag(tok_get(Sa, ET), s_close, <<"s">>, -1, <<"~~">>)),
                          case tok_get(Sb, ET - 1) of
                              #{type := text, content := <<"~">>} -> {Sb, [ET - 1 | LoneAcc]};
                              _ -> {Sb, LoneAcc}
                          end
                  end
          end, {S0, []}, lists:seq(0, N - 1)),
    %% loneMarkers.pop(): the last pushed first
    lists:foldl(fun(I, S) -> lone_marker(S, I) end, S1, Lone).

lone_marker(#i{ntok = N} = S, I) ->
    J = lone_end(S, I + 1, N) - 1,
    case I =/= J of
        true ->
            TI = tok_get(S, I),
            TJ = tok_get(S, J),
            tok_put(tok_put(S, J, TI), I, TJ);
        false -> S
    end.

lone_end(S, J, N) when J < N ->
    case tok_get(S, J) of
        #{type := s_close} -> lone_end(S, J + 1, N);
        _ -> J
    end;
lone_end(_, J, _) -> J.

%% emphasis's postProcess: <em> and <strong> from matched delimiters.
emphasis_post(S, Id) ->
    {Ds, N} = dl_get(S, Id),
    em_loop(S, Ds, N - 1).

em_loop(S, _Ds, I) when I < 0 -> S;
em_loop(S, Ds, I) ->
    D = maps:get(I, Ds),
    M = maps:get(marker, D),
    End = maps:get('end', D),
    case (M =:= $_ orelse M =:= $*) andalso End =/= -1 of
        false -> em_loop(S, Ds, I - 1);
        true ->
            E = maps:get(End, Ds),
            ST = maps:get(token, D),
            ET = maps:get(token, E),
            IsStrong = I > 0 andalso
                begin
                    P = maps:get(I - 1, Ds),
                    maps:get('end', P) =:= End + 1 andalso maps:get(marker, P) =:= M
                        andalso maps:get(token, P) =:= ST - 1
                        andalso maps:get(token, maps:get(End + 1, Ds)) =:= ET + 1
                end,
            {Type, Tag, Markup} = case IsStrong of
                                      true -> {strong, <<"strong">>, <<M, M>>};
                                      false -> {em, <<"em">>, <<M>>}
                                  end,
            OpenT = case Type of strong -> strong_open; em -> em_open end,
            CloseT = case Type of strong -> strong_close; em -> em_close end,
            S1 = tok_put(S, ST, retag(tok_get(S, ST), OpenT, Tag, 1, Markup)),
            S2 = tok_put(S1, ET, retag(tok_get(S1, ET), CloseT, Tag, -1, Markup)),
            case IsStrong of
                true ->
                    PT = maps:get(token, maps:get(I - 1, Ds)),
                    NT = maps:get(token, maps:get(End + 1, Ds)),
                    S3 = tok_put(S2, PT, (tok_get(S2, PT))#{content := <<>>}),
                    S4 = tok_put(S3, NT, (tok_get(S3, NT))#{content := <<>>}),
                    em_loop(S4, Ds, I - 2);
                false -> em_loop(S2, Ds, I - 1)
            end
    end.

%% fragments_join: merge adjacent text tokens, recompute levels.
fragments_join(Toks) -> fj(Toks, 0, []).

fj([], _, Acc) -> lists:reverse(Acc);
fj([#{nesting := Nest} = T0 | Rest], Level0, Acc) ->
    Level = case Nest < 0 of true -> Level0 - 1; false -> Level0 end,
    T = T0#{level := Level},
    Level1 = case Nest > 0 of true -> Level + 1; false -> Level end,
    case {T, Rest} of
        {#{type := text, content := C}, [#{type := text, content := C2} = Next | Rest1]} ->
            fj([Next#{content := <<C/binary, C2/binary>>} | Rest1], Level1, Acc);
        _ -> fj(Rest, Level1, [T | Acc])
    end.

%% core text_join: text_special becomes text, adjacent texts merge.
text_join(Toks) ->
    merge_text([case T of #{type := text_special} -> T#{type := text}; _ -> T end
                || T <- Toks]).

merge_text([#{type := text, content := A}, #{type := text, content := B} = T2 | Rest]) ->
    merge_text([T2#{content := <<A/binary, B/binary>>} | Rest]);
merge_text([T | Rest]) -> [T | merge_text(Rest)];
merge_text([]) -> [].

%%%===================================================================
%%% Task lists (the editor's markdown-it rule)
%%%===================================================================

%% A bullet list whose direct items all start with "[ ]" or "[x]" becomes
%% a task list (sigil's install-task-list-rule!, as in markdown_editor.ts).
task_lists(T) -> task_lists(T, 1).

task_lists(T, I) when I > tuple_size(T) -> T;
task_lists(T, I) ->
    case element(I, T) of
        #{type := bullet_list_open} ->
            case task_scan(T, I + 1, 1, []) of
                {Close, Items} when Items =/= [] ->
                    Inlines = [task_inline(T, K + 1, Close) || K <- Items],
                    case lists:all(fun(M) -> M > 0 andalso
                                                 task_re(maps:get(content, element(M, T))) =/= nomatch
                                   end, Inlines) of
                        true ->
                            T1 = setelement(I, T, (element(I, T))#{type := task_list_open}),
                            T2 = setelement(Close, T1, (element(Close, T1))#{type := task_list_close}),
                            T3 = lists:foldl(fun({K, M}, Acc) -> task_item(Acc, K, M, Close) end,
                                             T2, lists:zip(Items, Inlines)),
                            task_lists(T3, Close + 1);
                        false -> task_lists(T, I + 1)
                    end;
                _ -> task_lists(T, I + 1)
            end;
        _ -> task_lists(T, I + 1)
    end.

%% {close index, [item indexes]} of the list opened before J.
task_scan(T, J, Depth, Items) when J =< tuple_size(T) ->
    Type = atom_to_binary(maps:get(type, element(J, T))),
    case {suffix(Type, <<"_list_open">>), suffix(Type, <<"_list_close">>)} of
        {true, _} -> task_scan(T, J + 1, Depth + 1, Items);
        {_, true} when Depth =:= 1 -> {J, lists:reverse(Items)};
        {_, true} -> task_scan(T, J + 1, Depth - 1, Items);
        _ when Type =:= <<"list_item_open">>, Depth =:= 1 ->
            task_scan(T, J + 1, Depth, [J | Items]);
        _ -> task_scan(T, J + 1, Depth, Items)
    end;
task_scan(_, _, _, _) -> nomatch.

suffix(B, Suf) ->
    byte_size(B) >= byte_size(Suf) andalso
        binary:part(B, byte_size(B) - byte_size(Suf), byte_size(Suf)) =:= Suf.

task_inline(T, M, Close) when M < Close ->
    case maps:get(type, element(M, T)) of
        inline -> M;
        list_item_close -> -1;
        _ -> task_inline(T, M + 1, Close)
    end;
task_inline(_, _, _) -> -1.

%% /^\[([ xX])\]\s?/: {checked, length of the match}.
task_re(<<"[", C, "]", Rest/binary>>) when C =:= $\s; C =:= $x; C =:= $X ->
    WS = case Rest of
             <<Ch/utf8, _/binary>> -> case is_js_ws(Ch) of
                                          true -> byte_size(<<Ch/utf8>>);
                                          false -> 0
                                      end;
             _ -> 0
         end,
    {C =/= $\s, 3 + WS};
task_re(_) -> nomatch.

task_item(T, K, M, Close) ->
    T1 = setelement(K, T, (element(K, T))#{type := task_item_open}),
    T2 = task_item_close(T1, K + 1, Close, 0),
    #{content := Content, children := Children} = Inl = element(M, T2),
    {Checked, Len} = task_re(Content),
    T3 = setelement(K, T2, (element(K, T2))#{checked => Checked}),
    Children1 = case Children of
                    [#{type := text, content := C} = First | Rest] ->
                        [First#{content := case task_re(C) of
                                               nomatch -> C;
                                               {_, L} -> slice(C, L)
                                           end} | Rest];
                    _ -> Children
                end,
    setelement(M, T3, Inl#{content := slice(Content, Len), children := Children1}).

task_item_close(T, M, Close, D) when M < Close ->
    case maps:get(type, element(M, T)) of
        list_item_open -> task_item_close(T, M + 1, Close, D + 1);
        list_item_close when D =:= 0 ->
            setelement(M, T, (element(M, T))#{type := task_item_close});
        list_item_close -> task_item_close(T, M + 1, Close, D - 1);
        _ -> task_item_close(T, M + 1, Close, D)
    end;
task_item_close(T, _, _, _) -> T.

%% The checkbox of each task item goes first in its first inline.
task_checkboxes(T) ->
    lists:foldl(fun(I, Acc) ->
                        case element(I, Acc) of
                            #{type := task_item_open, checked := Checked} ->
                                task_checkbox(Acc, I + 1, Checked);
                            _ -> Acc
                        end
                end, T, lists:seq(1, tuple_size(T))).

task_checkbox(T, M, Checked) when M =< tuple_size(T) ->
    case element(M, T) of
        #{type := inline, children := Ch} = Inl ->
            Box = (tok(task_checkbox, <<>>, 0))#{info => Checked},
            setelement(M, T, Inl#{children := [Box | Ch]});
        #{type := Type} ->
            case suffix(atom_to_binary(Type), <<"_item_close">>) of
                true -> T;
                false -> task_checkbox(T, M + 1, Checked)
            end
    end;
task_checkbox(T, _, _) -> T.

%%%===================================================================
%%% Renderer
%%%===================================================================

esc(B) -> beamai_html_escape:escape(B).

render_tokens(T) ->
    [render_block(T, I) || I <- lists:seq(1, tuple_size(T))].

render_block(T, I) ->
    case element(I, T) of
        #{type := inline, children := Ch} ->
            CT = list_to_tuple(Ch),
            [render_inline(CT, J) || J <- lists:seq(1, tuple_size(CT))];
        Tok -> render_rule(T, I, Tok)
    end.

render_inline(T, I) -> render_rule(T, I, element(I, T)).

render_rule(_T, _I, #{type := code_inline, content := C}) ->
    [<<"<code>">>, esc(C), <<"</code>">>];
render_rule(_T, _I, #{type := code_block, content := C}) ->
    [<<"<pre><code>">>, esc(C), <<"</code></pre>\n">>];
render_rule(_T, _I, #{type := fence, content := C, info := Info0}) ->
    Info = js_trim(unescape_all(Info0)),
    case Info of
        <<>> -> [<<"<pre><code>">>, esc(C), <<"</code></pre>\n">>];
        _ ->
            Lang = first_word(Info),
            [<<"<pre><code class=\"">>, esc(<<"language-", Lang/binary>>), <<"\">">>, esc(C),
             <<"</code></pre>\n">>]
    end;
render_rule(T, I, #{type := image, attrs := Attrs, children := Ch} = Tok) ->
    Alt = iolist_to_binary(render_as_text(Ch)),
    render_token(T, I, Tok#{attrs := lists:keyreplace(<<"alt">>, 1, Attrs, {<<"alt">>, Alt})});
render_rule(_T, _I, #{type := hardbreak}) -> <<"<br>\n">>;
render_rule(_T, _I, #{type := softbreak}) -> <<"\n">>;
render_rule(_T, _I, #{type := text, content := C}) -> esc(C);
render_rule(_T, _I, #{type := task_checkbox, info := Checked}) ->
    [<<"<input type=\"checkbox\" class=\"task-list-item-checkbox\" disabled">>,
     [<<" checked">> || Checked], <<">">>];
render_rule(T, I, #{type := task_list_open} = Tok) ->
    render_token(T, I, Tok#{attrs := [{<<"class">>, <<"contains-task-list">>}]});
render_rule(T, I, #{type := task_item_open} = Tok) ->
    render_token(T, I, Tok#{attrs := [{<<"class">>, <<"task-list-item">>}]});
render_rule(T, I, Tok) -> render_token(T, I, Tok).

%% Renderer.renderToken
render_token(_T, _I, #{hidden := true}) -> <<>>;
render_token(T, I, #{block := Block, nesting := Nesting, tag := Tag, attrs := Attrs}) ->
    Lead = case Block andalso Nesting =/= -1 andalso I > 1 andalso
               maps:get(hidden, element(I - 1, T)) of
               true -> <<"\n">>;
               false -> <<>>
           end,
    Open = case Nesting of -1 -> <<"</">>; _ -> <<"<">> end,
    NeedLf = Block andalso
        not (Nesting =:= 1 andalso I < tuple_size(T) andalso
             case element(I + 1, T) of
                 #{type := inline} -> true;
                 #{hidden := true} -> true;
                 #{nesting := -1, tag := Tag} -> true;
                 _ -> false
             end),
    [Lead, Open, Tag, [[$\s, K, <<"=\"">>, esc(V), $"] || {K, V} <- Attrs],
     case NeedLf of true -> <<">\n">>; false -> <<">">> end].

%% Renderer.renderInlineAsText (image alt texts).
render_as_text(Toks) ->
    [case T of
         #{type := text, content := C} -> C;
         #{type := image, children := Ch} -> render_as_text(Ch);
         #{type := softbreak} -> <<"\n">>;
         #{type := hardbreak} -> <<"\n">>;
         _ -> <<>>
     end || T <- Toks].

first_word(Info) ->
    case [I || {I, Ch} <- lists:zip(lists:seq(0, length(cps(Info)) - 1), cps(Info)),
               is_js_ws(Ch)] of
        [] -> Info;
        [I | _] -> unicode:characters_to_binary(lists:sublist(cps(Info), I))
    end.

cps(B) -> unicode:characters_to_list(B).

%%%===================================================================
%%% Characters
%%%===================================================================

%% The code point that ends just before byte P (UTF-8).
prev_cp(Src, P) ->
    Start = cp_start(Src, P - 1, 3),
    cp_at(Src, Start).

cp_start(Src, P, N) when N > 0, P > 0 ->
    case binary:at(Src, P) band 16#C0 of
        16#80 -> cp_start(Src, P - 1, N - 1);
        _ -> P
    end;
cp_start(_, P, _) -> P.

cp_at(Src, P) ->
    case slice(Src, P) of
        <<C/utf8, _/binary>> -> C;
        <<C, _/binary>> -> C;
        <<>> -> 16#20
    end.

%% markdown-it's isMdAsciiPunct(c) || isPunctChar(c) (Unicode P and S).
is_punct(C) when C < 128 ->
    (C >= 33 andalso C =< 47) orelse (C >= 58 andalso C =< 64)
        orelse (C >= 91 andalso C =< 96) orelse (C >= 123 andalso C =< 126);
is_punct(C) ->
    R = ranges(),
    in_ranges(R, C, 1, tuple_size(R)).

ranges() ->
    case persistent_term:get(?MODULE, undefined) of
        undefined ->
            R = ?D:punct_ranges(),
            persistent_term:put(?MODULE, R),
            R;
        R -> R
    end.

in_ranges(_R, _C, Lo, Hi) when Lo > Hi -> false;
in_ranges(R, C, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    {A, B} = element(Mid, R),
    if
        C < A -> in_ranges(R, C, Lo, Mid - 1);
        C > B -> in_ranges(R, C, Mid + 1, Hi);
        true -> true
    end.

%% markdown-it's isWhiteSpace.
is_ws(C) ->
    (C >= 16#2000 andalso C =< 16#200A) orelse (C >= 9 andalso C =< 13)
        orelse C =:= 16#20 orelse C =:= 16#A0 orelse C =:= 16#1680 orelse C =:= 16#202F
        orelse C =:= 16#205F orelse C =:= 16#3000.

%% JavaScript's \s and String.prototype.trim.
is_js_ws(C) ->
    (C >= 9 andalso C =< 13) orelse C =:= 16#20 orelse C =:= 16#A0 orelse C =:= 16#1680
        orelse (C >= 16#2000 andalso C =< 16#200A) orelse C =:= 16#2028 orelse C =:= 16#2029
        orelse C =:= 16#202F orelse C =:= 16#205F orelse C =:= 16#3000 orelse C =:= 16#FEFF.

js_trim(B) ->
    case trim_needed(B) of
        false -> B;
        true ->
            L = lists:dropwhile(fun is_js_ws/1, cps(B)),
            unicode:characters_to_binary(
              lists:reverse(lists:dropwhile(fun is_js_ws/1, lists:reverse(L))))
    end.

trim_needed(<<>>) -> false;
trim_needed(B) ->
    First = binary:first(B),
    Last = binary:last(B),
    First < 16#21 orelse First >= 16#80 orelse Last < 16#21 orelse Last >= 16#80.

%% asciiTrim: spaces, tabs, \n and \r only.
ascii_trim(B) ->
    L = byte_size(B),
    S = ascii_start(B, 0, L),
    E = ascii_end(B, L - 1, S),
    binary:part(B, S, E + 1 - S).

ascii_start(B, P, L) when P < L ->
    case is_ascii_trimmable(binary:at(B, P)) of
        true -> ascii_start(B, P + 1, L);
        false -> P
    end;
ascii_start(_, P, _) -> P.

ascii_end(B, P, S) when P >= S ->
    case is_ascii_trimmable(binary:at(B, P)) of
        true -> ascii_end(B, P - 1, S);
        false -> P
    end;
ascii_end(_, P, _) -> P.

is_ascii_trimmable(C) -> C =:= $\s orelse C =:= $\t orelse C =:= $\n orelse C =:= $\r.

%% normalizeReference: trim, collapse whitespace, case-fold.
normalize_reference(Str) ->
    L = collapse_ws(cps(js_trim(Str))),
    unicode:characters_to_binary(string:uppercase(string:lowercase(L))).

collapse_ws([]) -> [];
collapse_ws([C | Rest]) ->
    case is_js_ws(C) of
        true -> [$\s | collapse_ws(lists:dropwhile(fun is_js_ws/1, Rest))];
        false -> [C | collapse_ws(Rest)]
    end.

%% unescapeAll: backslash escapes and entities (with decodeHTML's
%% legacy references: "&copyx;" is "©x;").
unescape_all(Str) ->
    case binary:match(Str, [<<"\\">>, <<"&">>]) of
        nomatch -> Str;
        _ -> iolist_to_binary(ua(Str))
    end.

ua(<<"\\", C, Rest/binary>>) ->
    case lists:member(C, "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~") of
        true -> [C | ua(Rest)];
        false -> [$\\ | ua(<<C, Rest/binary>>)]
    end;
ua(<<"&", Rest/binary>> = All) ->
    case re:run(Rest, <<"^([a-z#][a-z0-9]{1,31});">>, [caseless, {capture, [1], binary}]) of
        {match, [Name]} ->
            Match = <<"&", Name/binary, ";">>,
            [replace_entity(Match, Name) | ua(slice(All, byte_size(Match)))];
        nomatch -> [$& | ua(Rest)]
    end;
ua(<<C, Rest/binary>>) -> [C | ua(Rest)];
ua(<<>>) -> [].

replace_entity(Match, <<"#", Num/binary>> = Name) ->
    case re:run(Name, <<"^#((?:x[a-f0-9]{1,8}|[0-9]{1,8}))$">>, [caseless]) of
        {match, _} ->
            Code = numeric(Num),
            case is_valid_entity_code(Code) of
                true -> <<Code/utf8>>;
                false -> Match
            end;
        nomatch ->
            %% decodeHTML decodes longer numbers itself
            case re:run(Name, <<"^#(x[a-f0-9]+|[0-9]+)$">>, [caseless]) of
                {match, _} -> <<(replace_code_point(numeric(Num)))/utf8>>;
                nomatch -> Match
            end
    end;
replace_entity(Match, Name) ->
    case ?D:entity(Name) of
        undefined ->
            case legacy_prefix(Name) of
                none -> Match;
                Prefix ->
                    <<(?D:entity(Prefix))/binary, (slice(Name, byte_size(Prefix)))/binary, ";">>
            end;
        Text -> Text
    end.

numeric(<<X, Hex/binary>>) when X =:= $x; X =:= $X -> binary_to_integer(Hex, 16);
numeric(Dec) -> binary_to_integer(Dec).

%% entities' replaceCodePoint.
replace_code_point(C) when C >= 16#D800, C =< 16#DFFF; C > 16#10FFFF; C =:= 0 -> 16#FFFD;
replace_code_point(C) ->
    C1 = #{128 => 8364, 130 => 8218, 131 => 402, 132 => 8222, 133 => 8230, 134 => 8224,
           135 => 8225, 136 => 710, 137 => 8240, 138 => 352, 139 => 8249, 140 => 338,
           142 => 381, 145 => 8216, 146 => 8217, 147 => 8220, 148 => 8221, 149 => 8226,
           150 => 8211, 151 => 8212, 152 => 732, 153 => 8482, 154 => 353, 155 => 8250,
           156 => 339, 158 => 382, 159 => 376},
    maps:get(C, C1, C).

legacy_prefix(Name) ->
    case [P || P <- ?D:legacy(), byte_size(P) < byte_size(Name),
               binary:part(Name, 0, byte_size(P)) =:= P] of
        [] -> none;
        Ps -> hd(lists:sort(fun(A, B) -> byte_size(A) >= byte_size(B) end, Ps))
    end.

%%%===================================================================
%%% URLs (markdown-it's normalizeLink / normalizeLinkText, mdurl, punycode)
%%%===================================================================

%% Only http:, https:, mailto: and URLs without a scheme.
validate_link(Url) ->
    case re:run(js_trim(Url), <<"^([a-zA-Z][a-zA-Z0-9+.\\-]*):">>, [{capture, [1], binary}]) of
        nomatch -> true;
        {match, [Scheme]} ->
            lists:member(string:lowercase(Scheme), [<<"http">>, <<"https">>, <<"mailto">>])
    end.

normalize_link(Url) ->
    P = url_parse(Url),
    P1 = recode_host(P, fun puny_to_ascii/1),
    mdurl_encode(url_format(P1)).

normalize_link_text(Url) ->
    P = url_parse(Url),
    P1 = recode_host(P, fun puny_to_unicode/1),
    mdurl_decode(url_format(P1), <<";/?:@&=+$,#%">>).

recode_host(#{hostname := H, protocol := Proto} = P, F) when H =/= undefined, H =/= <<>> ->
    case Proto =:= undefined orelse
        lists:member(Proto, [<<"http:">>, <<"https:">>, <<"mailto:">>]) of
        true ->
            try P#{hostname := F(H)}
            catch throw:puny -> P
            end;
        false -> P
    end;
recode_host(P, _) -> P.

url0() -> #{protocol => undefined, slashes => false, auth => undefined, port => undefined,
                hostname => undefined, hash => undefined, search => undefined,
                pathname => undefined}.

%% mdurl.parse(url, true)
url_parse(Url) ->
    Rest0 = js_trim(Url),
    {Proto, Rest1} =
        case re:run(Rest0, <<"^([a-z0-9.+-]+:)">>, [caseless, {capture, [1], binary}]) of
            {match, [Pr]} -> {Pr, slice(Rest0, byte_size(Pr))};
            nomatch -> {undefined, Rest0}
        end,
    LowerProto = case Proto of undefined -> undefined; _ -> string:lowercase(Proto) end,
    Hostless = Proto =:= <<"javascript:">>,
    Slashed = fun(Pr) -> lists:member(Pr, [<<"http:">>, <<"https:">>, <<"ftp:">>, <<"gopher:">>,
                                           <<"file:">>]) end,
    Slashes = binary:longest_common_prefix([Rest1, <<"//">>]) =:= 2,
    {U1, Rest2} = case Slashes andalso not Hostless of
                      true -> {(url0())#{protocol := Proto, slashes := true}, slice(Rest1, 2)};
                      false -> {(url0())#{protocol := Proto}, Rest1}
                  end,
    {U2, Rest3} =
        case not Hostless andalso (Slashes orelse (Proto =/= undefined andalso not Slashed(Proto))) of
            true -> url_host(U1, Rest2);
            false -> {U1, Rest2}
        end,
    {U3, Rest4} = case binary:match(Rest3, <<"#">>) of
                      {H, _} -> {U2#{hash := slice(Rest3, H)}, slice(Rest3, 0, H)};
                      nomatch -> {U2, Rest3}
                  end,
    {U4, Rest5} = case binary:match(Rest4, <<"?">>) of
                      {Q, _} -> {U3#{search := slice(Rest4, Q)}, slice(Rest4, 0, Q)};
                      nomatch -> {U3, Rest4}
                  end,
    U5 = case Rest5 of <<>> -> U4; _ -> U4#{pathname := Rest5} end,
    case LowerProto =/= undefined andalso Slashed(LowerProto) andalso
        not empty(maps:get(hostname, U5)) andalso empty(maps:get(pathname, U5)) of
        true -> U5#{pathname := <<>>};
        false -> U5
    end.

empty(undefined) -> true;
empty(<<>>) -> true;
empty(_) -> false.

first_index(Bin, Chars) ->
    case binary:match(Bin, [<<C>> || C <- Chars]) of
        {P, _} -> P;
        nomatch -> -1
    end.

url_host(U0, Rest0) ->
    HostEnd0 = first_index(Rest0, "/?#"),
    AtSign = last_at(Rest0, case HostEnd0 of -1 -> byte_size(Rest0); _ -> HostEnd0 end),
    {U1, Rest1} = case AtSign of
                      -1 -> {U0, Rest0};
                      _ -> {U0#{auth := slice(Rest0, 0, AtSign)}, slice(Rest0, AtSign + 1)}
                  end,
    HostEnd1 = case first_index(Rest1, "%/?;#'{}|\\^`<>\" \r\n\t") of
                   -1 -> byte_size(Rest1);
                   E -> E
               end,
    HostEnd = case HostEnd1 > 0 andalso c(Rest1, HostEnd1 - 1) =:= $: of
                  true -> HostEnd1 - 1;
                  false -> HostEnd1
              end,
    Host = slice(Rest1, 0, HostEnd),
    Rest2 = slice(Rest1, HostEnd),
    U2 = parse_host(U1, Host),
    Hostname0 = case maps:get(hostname, U2) of undefined -> <<>>; Hn -> Hn end,
    IPv6 = byte_size(Hostname0) >= 1 andalso binary:first(Hostname0) =:= $[
        andalso binary:last(Hostname0) =:= $],
    {Hostname1, Rest3} = case IPv6 of
                             true -> {Hostname0, Rest2};
                             false -> host_parts(Hostname0, Rest2)
                         end,
    Hostname2 = case utf16_length(Hostname1) > 255 of true -> <<>>; false -> Hostname1 end,
    Hostname = case IPv6 of
                   true when byte_size(Hostname2) >= 2 ->
                       binary:part(Hostname2, 1, byte_size(Hostname2) - 2);
                   true -> <<>>;
                   false -> Hostname2
               end,
    {U2#{hostname := Hostname}, Rest3}.

%% rest.lastIndexOf('@', HostEnd)
last_at(Bin, Upto) ->
    Scope = min(Upto + 1, byte_size(Bin)),
    case binary:matches(binary:part(Bin, 0, Scope), <<"@">>) of
        [] -> -1;
        Ms -> element(1, lists:last(Ms))
    end.

parse_host(U, Host) ->
    {U1, Host1} = case re:run(Host, <<":[0-9]*$">>, [{capture, first, index}]) of
                      {match, [{P, L}]} ->
                          Port = binary:part(Host, P, L),
                          U0 = case Port of <<":">> -> U; _ -> U#{port := slice(Port, 1)} end,
                          {U0, binary:part(Host, 0, byte_size(Host) - L)};
                      nomatch -> {U, Host}
                  end,
    case Host1 of <<>> -> U1; _ -> U1#{hostname := Host1} end.

host_parts(Hostname, Rest) ->
    Parts = binary:split(Hostname, <<".">>, [global]),
    host_parts(Parts, 0, Hostname, Rest).

host_parts([], _, Hostname, Rest) -> {Hostname, Rest};
host_parts([<<>> | More], I, Hostname, Rest) -> host_parts(More, I + 1, Hostname, Rest);
host_parts([Part | More], I, Hostname, Rest) ->
    case valid_host_part(Part) orelse valid_host_part(x_non_ascii(Part)) of
        true -> host_parts(More, I + 1, Hostname, Rest);
        false ->
            All = binary:split(Hostname, <<".">>, [global]),
            Valid0 = lists:sublist(All, I),
            NotHost0 = lists:nthtail(I + 1, All),
            {Valid, NotHost} =
                case re:run(Part, <<"^([+a-z0-9A-Z_-]{0,63})(.*)$">>,
                            [unicode, {capture, [1, 2], binary}]) of
                    {match, [A, B]} -> {Valid0 ++ [A], [B | NotHost0]};
                    nomatch -> {Valid0, NotHost0}
                end,
            Rest1 = case NotHost of
                        [] -> Rest;
                        _ -> <<(iolist_to_binary(lists:join(<<".">>, NotHost)))/binary, Rest/binary>>
                    end,
            {iolist_to_binary(lists:join(<<".">>, Valid)), Rest1}
    end.

valid_host_part(P) -> re:run(P, <<"^[+a-z0-9A-Z_-]{0,63}$">>) =/= nomatch.

%% Every UTF-16 unit above 127 becomes an x (astral characters two).
x_non_ascii(Part) ->
    << <<(if C < 128 -> <<C>>; C > 16#FFFF -> <<"xx">>; true -> <<"x">> end)/binary>>
       || <<C/utf8>> <= Part >>.

utf16_length(B) ->
    lists:sum([case C > 16#FFFF of true -> 2; false -> 1 end || C <- cps(B)]).

%% mdurl.format
url_format(#{protocol := Proto, slashes := Slashes, auth := Auth, hostname := Host,
             port := Port, pathname := Path, search := Search, hash := Hash}) ->
    Opt = fun(undefined) -> <<>>; (V) -> V end,
    HostPart = case Host of
                   undefined -> <<>>;
                   _ -> case binary:match(Host, <<":">>) of
                            nomatch -> Host;
                            _ -> <<"[", Host/binary, "]">>
                        end
               end,
    iolist_to_binary([Opt(Proto), case Slashes of true -> <<"//">>; false -> <<>> end,
                      case Auth of undefined -> <<>>; <<>> -> <<>>; _ -> [Auth, $@] end,
                      HostPart,
                      case Port of undefined -> <<>>; <<>> -> <<>>; _ -> [$:, Port] end,
                      Opt(Path), Opt(Search), Opt(Hash)]).

%% mdurl.encode with its default exclusions, keeping %XX escapes.
mdurl_encode(Str) -> iolist_to_binary(enc(Str)).

enc(<<"%", A, B, Rest/binary>>) when ?IS_HEX(A), ?IS_HEX(B) -> [$%, A, B | enc(Rest)];
enc(<<C, Rest/binary>>) ->
    case (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse (C >= $0 andalso C =< $9)
        orelse lists:member(C, ";/?:@&=+$,-_.!~*'()#") of
        true -> [C | enc(Rest)];
        false -> [pct(C) | enc(Rest)]
    end;
enc(<<>>) -> [].

pct(C) -> [$% | string:to_upper(lists:flatten(io_lib:format("~2.16.0B", [C])))].

%% mdurl.decode(str, exclude): %XX sequences to characters (the excluded
%% ASCII characters stay encoded, with upper case hex).
mdurl_decode(Str, Exclude) ->
    iolist_to_binary(dec(Str, Exclude)).

dec(<<"%", A, B, _/binary>> = Str, Ex) when ?IS_HEX(A), ?IS_HEX(B) ->
    {Bytes, Rest} = pct_run(Str, []),
    [dec_seq(Bytes, Ex) | dec(Rest, Ex)];
dec(<<C, Rest/binary>>, Ex) -> [C | dec(Rest, Ex)];
dec(<<>>, _) -> [].

pct_run(<<"%", A, B, Rest/binary>>, Acc) when ?IS_HEX(A), ?IS_HEX(B) ->
    pct_run(Rest, [binary_to_integer(<<A, B>>, 16) | Acc]);
pct_run(Rest, Acc) -> {lists:reverse(Acc), Rest}.

-define(FFFD, <<16#FFFD/utf8>>).
dec_seq([], _) -> [];
dec_seq([B1 | Rest], Ex) when B1 < 16#80 ->
    case lists:member(B1, binary_to_list(Ex)) of
        true -> [pct(B1) | dec_seq(Rest, Ex)];
        false -> [B1 | dec_seq(Rest, Ex)]
    end;
dec_seq([B1, B2 | Rest], Ex) when B1 band 16#E0 =:= 16#C0, B2 band 16#C0 =:= 16#80 ->
    Chr = ((B1 bsl 6) band 16#7C0) bor (B2 band 16#3F),
    case Chr < 16#80 of
        true -> [?FFFD, ?FFFD | dec_seq(Rest, Ex)];
        false -> [<<Chr/utf8>> | dec_seq(Rest, Ex)]
    end;
dec_seq([B1, B2, B3 | Rest], Ex) when B1 band 16#F0 =:= 16#E0, B2 band 16#C0 =:= 16#80,
                                      B3 band 16#C0 =:= 16#80 ->
    Chr = ((B1 bsl 12) band 16#F000) bor ((B2 bsl 6) band 16#FC0) bor (B3 band 16#3F),
    case Chr < 16#800 orelse (Chr >= 16#D800 andalso Chr =< 16#DFFF) of
        true -> [?FFFD, ?FFFD, ?FFFD | dec_seq(Rest, Ex)];
        false -> [<<Chr/utf8>> | dec_seq(Rest, Ex)]
    end;
dec_seq([B1, B2, B3, B4 | Rest], Ex) when B1 band 16#F8 =:= 16#F0, B2 band 16#C0 =:= 16#80,
                                          B3 band 16#C0 =:= 16#80, B4 band 16#C0 =:= 16#80 ->
    Chr = ((B1 bsl 18) band 16#1C0000) bor ((B2 bsl 12) band 16#3F000)
        bor ((B3 bsl 6) band 16#FC0) bor (B4 band 16#3F),
    case Chr < 16#10000 orelse Chr > 16#10FFFF of
        true -> [?FFFD, ?FFFD, ?FFFD, ?FFFD | dec_seq(Rest, Ex)];
        false -> [<<Chr/utf8>> | dec_seq(Rest, Ex)]
    end;
dec_seq([_ | Rest], Ex) -> [?FFFD | dec_seq(Rest, Ex)].

%% punycode.toASCII / toUnicode on a host name (throws puny on errors,
%% which leave the host name as it is, as markdown-it's try/catch).
puny_to_ascii(Host) ->
    map_domain(Host, fun(L) ->
                             case lists:any(fun(C) -> C > 16#7F end, L) of
                                 true -> "xn--" ++ puny_encode(L);
                                 false -> L
                             end
                     end).

puny_to_unicode(Host) ->
    map_domain(Host, fun("xn--" ++ Rest) -> puny_decode(cps(string:lowercase(
                                                                unicode:characters_to_binary(Rest))));
                        (L) -> L
                     end).

map_domain(Host, F) ->
    L0 = [case C of 16#3002 -> $.; 16#FF0E -> $.; 16#FF61 -> $.; _ -> C end || C <- cps(Host)],
    Labels = string:split(L0, ".", all),
    Out = lists:join($., [F(L) || L <- Labels]),
    case unicode:characters_to_binary(Out) of
        B when is_binary(B) -> B;
        _ -> throw(puny)
    end.

-define(P_BASE, 36).
-define(P_TMIN, 1).
-define(P_TMAX, 26).
-define(P_SKEW, 38).
-define(P_DAMP, 700).
-define(P_MAXINT, 2147483647).

puny_adapt(Delta0, NumPoints, FirstTime) ->
    Delta1 = case FirstTime of true -> Delta0 div ?P_DAMP; false -> Delta0 bsr 1 end,
    Delta2 = Delta1 + Delta1 div NumPoints,
    puny_adapt_loop(Delta2, 0).

puny_adapt_loop(Delta, K) when Delta > ((?P_BASE - ?P_TMIN) * ?P_TMAX) bsr 1 ->
    puny_adapt_loop(Delta div (?P_BASE - ?P_TMIN), K + ?P_BASE);
puny_adapt_loop(Delta, K) ->
    K + ((?P_BASE - ?P_TMIN + 1) * Delta) div (Delta + ?P_SKEW).

puny_t(K, Bias) ->
    if K =< Bias -> ?P_TMIN;
       K >= Bias + ?P_TMAX -> ?P_TMAX;
       true -> K - Bias
    end.

digit_to_basic(D) when D < 26 -> D + $a;
digit_to_basic(D) -> D - 26 + $0.

puny_encode(Input) ->
    Basic = [C || C <- Input, C < 16#80],
    B = length(Basic),
    Out0 = case B of 0 -> Basic; _ -> Basic ++ "-" end,
    puny_enc_loop(Input, 128, 0, 72, B, B, length(Input), lists:reverse(Out0)).

puny_enc_loop(_Input, _N, _Delta, _Bias, H, _B, Len, Out) when H >= Len -> lists:reverse(Out);
puny_enc_loop(Input, N, Delta0, Bias0, H, B, Len, Out0) ->
    M = lists:min([C || C <- Input, C >= N]),
    (M - N) > (?P_MAXINT - Delta0) div (H + 1) andalso throw(puny),
    Delta1 = Delta0 + (M - N) * (H + 1),
    {Delta2, Bias2, H2, Out2} =
        lists:foldl(
          fun(C, {D, Bias, HH, Out}) when C < M ->
                  D1 = D + 1,
                  D1 > ?P_MAXINT andalso throw(puny),
                  {D1, Bias, HH, Out};
             (C, {D, Bias, HH, Out}) when C =:= M ->
                  Digits = puny_digits(D, ?P_BASE, Bias, []),
                  {0, puny_adapt(D, H + 1, HH =:= B), HH + 1, lists:reverse(Digits, Out)};
             (_, Acc) -> Acc
          end, {Delta1, Bias0, H, Out0}, Input),
    puny_enc_loop(Input, M + 1, Delta2 + 1, Bias2, H2, B, Len, Out2).

puny_digits(Q, K, Bias, Acc) ->
    T = puny_t(K, Bias),
    case Q < T of
        true -> lists:reverse([digit_to_basic(Q) | Acc]);
        false ->
            D = T + (Q - T) rem (?P_BASE - T),
            puny_digits((Q - T) div (?P_BASE - T), K + ?P_BASE, Bias, [digit_to_basic(D) | Acc])
    end.

basic_to_digit(C) when C >= $0, C =< $9 -> 26 + C - $0;
basic_to_digit(C) when C >= $A, C =< $Z -> C - $A;
basic_to_digit(C) when C >= $a, C =< $z -> C - $a;
basic_to_digit(_) -> ?P_BASE.

puny_decode(Input) ->
    Basic = case string:rchr(Input, $-) of 0 -> 0; P -> P - 1 end,
    Out0 = lists:sublist(Input, Basic),
    lists:any(fun(C) -> C >= 16#80 end, Out0) andalso throw(puny),
    Rest = case Basic > 0 of true -> lists:nthtail(Basic + 1, Input); false -> Input end,
    puny_dec_loop(Rest, 0, 128, 72, Out0).

puny_dec_loop([], _I, _N, _Bias, Out) ->
    lists:any(fun(C) -> C > 16#10FFFF orelse (C >= 16#D800 andalso C =< 16#DFFF) end, Out)
        andalso throw(puny),
    Out;
puny_dec_loop(Input, I0, N0, Bias0, Out) ->
    {I1, Rest} = puny_dec_digits(Input, I0, 1, ?P_BASE, Bias0),
    Len = length(Out) + 1,
    Bias1 = puny_adapt(I1 - I0, Len, I0 =:= 0),
    I1 div Len > ?P_MAXINT - N0 andalso throw(puny),
    N1 = N0 + I1 div Len,
    I2 = I1 rem Len,
    {Before, After} = lists:split(I2, Out),
    puny_dec_loop(Rest, I2 + 1, N1, Bias1, Before ++ [N1 | After]).

puny_dec_digits([], _I, _W, _K, _Bias) -> throw(puny);
puny_dec_digits([C | Rest], I, W, K, Bias) ->
    Digit = basic_to_digit(C),
    Digit >= ?P_BASE andalso throw(puny),
    Digit > (?P_MAXINT - I) div W andalso throw(puny),
    I1 = I + Digit * W,
    T = puny_t(K, Bias),
    case Digit < T of
        true -> {I1, Rest};
        false ->
            W > ?P_MAXINT div (?P_BASE - T) andalso throw(puny),
            puny_dec_digits(Rest, I1, W * (?P_BASE - T), K + ?P_BASE, Bias)
    end.
