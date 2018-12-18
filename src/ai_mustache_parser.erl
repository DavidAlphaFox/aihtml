-module(ai_mustache_parser).

-export([parse/1]).

-define(PARSE_ERROR,                incorrect_format).

-define(IIF(Cond, TValue, FValue),
        case Cond of true -> TValue; false -> FValue end).
-define(ADD(X, Y), ?IIF(X =:= <<>>, Y, [X | Y])).

-define(START_TAG, <<"{{">>).
-define(STOP_TAG,  <<"}}">>).
-type source() :: binary().
-type key()    :: binary().
-type tag()    :: {n,   [key()]}
                | {'&', [key()]}
                | {'#', [key()], [tag()], source()}
                | {'^', [key()], [tag()]}
                | {'>', key(), Indent :: source()}
                | binary(). % plain text
-record(state,{
          start    = ?START_TAG :: binary(),
          stop     = ?STOP_TAG  :: binary(),
          standalone = true     :: boolean()
        }).
-type state() :: #state{}.
-type endtag()    :: {endtag, {state(), [key()], LastTagSize :: non_neg_integer(), Rest :: binary(), Result :: [tag()]}}.

parse(Body)->
    {IR,_State} = parse(#state{},Body),
    IR.
-spec parse(state(),binary()) -> {[tag()],#state{}}.
parse(State0,Bin) ->
    case parse1(State0,Bin,[]) of
        {endtag, {_, Keys, _, _, _}} ->
            error({?PARSE_ERROR, {section_is_incorrect, binary_join(Keys, <<".">>)}});
        {State,Tags} ->
            {lists:reverse(Tags),State}
    end.

%% pase的第一阶段
%% 找出StartTag或者该分段中的文本
-spec parse1(state(),Input :: binary(), Result :: [tag()]) -> {state(), [tag()]} | endtag().
parse1(#state{start = StartTag} = State,Bin, Result) ->
    case binary:match(Bin, [StartTag, <<"\n">>]) of %% 找StartTag，或者\n
        nomatch -> {State, ?ADD(Bin, Result)}; %% 整个就是个binary
        {S, L}  -> %% 找到StartTag或者\n了
            Pos = S + L, %%binary的切开点，Pos是未匹配字串的第一个字符
            B2  = binary:part(Bin, Pos, byte_size(Bin) - Pos),
            case binary:at(Bin, S) of
                $\n -> parse1(State#state{standalone = true},B2,
                        ?ADD(binary:part(Bin, 0, Pos), Result)); % \n,\n前面是个文本,此处切割出来的字符串包含\n
                _   -> parse2(State, split_tag(State, Bin), Result) %% 找到标签了，整个文本向前找标签
            end
    end.
%% @doc Part of the `parse/1'
%%
%% ATTENTION: The result is a list that is inverted.
-spec parse2(state(), iolist(), Result :: [tag()]) -> {state(), [tag()]} | endtag().
parse2(State, [B1, B2, B3], Result) ->
    case remove_space_from_head(B2) of %% 清除\s和\t，然后匹配tag类型
        <<T, Tag/binary>> when T =:= $&; T =:= ${ ->
            parse1(State#state{standalone = false}, B3, [{'&', keys(Tag)} | ?ADD(B1, Result)]);
        <<T, Tag/binary>> when T =:= $#; T =:= $^ ->
            parse_section(State, ?IIF(T =:= $#, '#', '^'), keys(Tag), B3, [B1 | Result]);
        <<"=", Tag0/binary>> ->
            Tag1 = remove_space_from_tail(Tag0),
            Size = byte_size(Tag1) - 1,
            case Size >= 0 andalso Tag1 of
                <<Tag2:Size/binary, "=">> -> parse_delimiter(State, Tag2, B3, [B1 | Result]);
                _                         -> error({?PARSE_ERROR, {unsupported_tag, <<"=", Tag0/binary>>}})
            end;
        <<"!", _/binary>> ->
            parse3(State, B3, [B1 | Result]);
        <<"/", Tag/binary>> ->
            EndTagSize = byte_size(B2) + byte_size(State#state.start) + byte_size(State#state.stop),
            {endtag, {State, keys(Tag), EndTagSize, B3, [B1 | Result]}};
        <<">", Tag/binary>> ->
            parse_partial(State, keys(Tag), B3, [B1 | Result]);
        Tag ->
            parse1(State#state{standalone = false}, B3, [{n, keys(Tag)} | ?ADD(B1, Result)])
    end;
parse2(_, _, _) -> error({?PARSE_ERROR, unclosed_tag}).

%% @doc Part of the `parse/1'
%%
%% it is end processing of tag that need to be considered the standalone.
-spec parse3(#state{}, binary(), [tag()]) -> {state(), [tag()]} | endtag().
parse3(State0, Post0, [Tag | Result0]) when is_tuple(Tag) ->
    {State1,_,Post1, Result1} = standalone(State0, Post0, Result0),
    parse1(State1, Post1, [Tag | Result1]);
parse3(State0, Post0, Result0) ->
    {State1, _,Post1, Result1} = standalone(State0, Post0, Result0),
    parse1(State1, Post1, Result1).

%% @doc Loop processing part of the `parse/1'
%%
%% `{{# Tag}}' or `{{^ Tag}}' corresponds to this.
-spec parse_section(state(), '#' | '^', [key()], Input :: binary(), Result :: [tag()]) -> {state(), [tag()]} | endtag().
parse_section(State0, Mark, Keys, Input0, Result0) ->
    {State1,_, Input1, Result1} = standalone(State0, Input0, Result0),
    case parse1(State1, Input1, []) of
        {endtag, {State2, Keys, _LastTagSize, Rest0, LoopResult0}} ->
            {State3, _, Rest1, LoopResult1} = standalone(State2, Rest0, LoopResult0),
            case Mark of
                '#' -> 
                       parse1(State3, Rest1, [{'#', Keys, lists:reverse(LoopResult1)} | Result1]);
                '^' -> parse1(State3, Rest1, [{'^', Keys, lists:reverse(LoopResult1)} | Result1])
            end;
        {endtag, {_, OtherKeys, _, _, _}} ->
            error({?PARSE_ERROR, {section_is_incorrect, binary_join(OtherKeys, <<".">>)}});
        _ ->
            error({?PARSE_ERROR, {section_end_tag_not_found, <<"/", (binary_join(Keys, <<".">>))/binary>>}})
    end.
-spec parse_partial(state(), Tag :: binary(), NextBin :: binary(), Result :: [tag()]) -> {state(), [tag()]} | endtag().
parse_partial(State0, Tag, NextBin0, Result0) ->
    {State1, Indent, NextBin1, Result1} = standalone(State0, NextBin0, Result0),
    parse1(State1, NextBin1, [{'>', Tag, Indent} | Result1]).
%% ParseDelimiterBin :: e.g. `{{=%% %%=}}' -> `%% %%'
-spec parse_delimiter(state(), ParseDelimiterBin :: binary(), NextBin :: binary(), Result :: [tag()]) -> {state(), [tag()]} | endtag().
parse_delimiter(State0, ParseDelimiterBin, NextBin, Result) ->
    case binary:match(ParseDelimiterBin, <<"=">>) of
        nomatch ->
            case [X || X <- binary:split(ParseDelimiterBin, <<" ">>, [global]), X =/= <<>>] of
                [Start, Stop] -> parse3(State0#state{start = Start, stop = Stop}, NextBin, Result);
                _             -> error({?PARSE_ERROR, delimiters_may_not_contain_whitespaces})
            end;
        _ ->
            error({?PARSE_ERROR, delimiters_may_not_contain_equals})
    end.
%% @doc Split by the tag, it returns a list of the split binary.
%%
%% e.g.
%% ```
%% 1> split_tag(State, <<"...{{hoge}}...">>).
%% [<<"...">>, <<"hoge">>, <<"...">>]
%%
%% 2> split_tag(State, <<"...{{hoge ...">>).
%% [<<"...">>, <<"hoge ...">>]
%%
%% 3> split_tag(State, <<"...">>)
%% [<<"...">>]
%% '''
-spec split_tag(state(), binary()) -> [binary(), ...].
split_tag(#state{start = StartTag, stop = StopTag}, Bin) ->
    case binary:match(Bin, StartTag) of
        nomatch -> [Bin]; %% 未找到开始标签
        {StartPos, StartTagLen} ->
            PosLimit = byte_size(Bin) - StartTagLen, 
            ShiftNum = ai_function:while({true, StartPos + 1}, %% 在下一个StartTag之前一直向前推进
                             fun(Pos) ->  %% {{{ ,startPos 0, StartDelimiterLen = 2 ShitNum = 1
                                     ?IIF(Pos =< PosLimit
                                          andalso binary:part(Bin, Pos, StartTagLen) =:= StartTag,
                                          {true, Pos + 1}, {false, Pos})
                             end) - StartPos - 1,
            %% PreTag是StartTag之前的文本，X是包含StarTag的文本
            {PreTag, X} = erlang:split_binary(Bin, StartPos + ShiftNum), 
            Tag0  = part(X, StartTagLen, 0), %%  去掉StartTag
            case binary:split(Tag0, StopTag) of
                [_] -> [PreTag, Tag0]; % 这段文本里面没有StopTag
                [TagName, Rest]  -> %% 找到TagName了
                    IncludeStartTag = binary:part(X, 0, byte_size(TagName) + StartTagLen),%%切出包含StartTag的tag
                    E = ?IIF(repeatedly_binary(StopTag, $}), %% 判断是否是重复的}}
                             ?IIF(byte_size(Rest) > 0 andalso binary:first(Rest) =:= $}, 1, 0),%% 检查剩余部分第一个元素是否是}
                             ?IIF(byte_size(TagName) > 0 andalso binary:last(TagName) =:= $}, -1, 0)),%% 检查tag最后的一个元素是否是}
                    S = ?IIF(repeatedly_binary(StartTag, ${), %% 判断是否是重复的{{
                             ?IIF(ShiftNum > 0, -1, 0),%% 找到{{之前的ShiftNum大于0就为-1否则为0
                             ?IIF(byte_size(TagName) > 0 andalso binary:first(TagName) =:= ${, 1, 0)),%% 判断第一个元素是否是{
                    case E =:= 0 orelse S =:= 0 of %% 如果 S = 0 代表{{之前没有东西或者Tag中第一个元素不是{,
                        true ->  % {{ ... }}
                            [PreTag, TagName, Rest]; %% 这个是一个存粹的Tag
                        false -> % {{{ ... }}}
                            [part(PreTag, 0, erlang:min(0, S)), %% 去掉 {
                             part(IncludeStartTag, erlang:max(0, S) + StartTagLen - 1, 
                                erlang:min(0, E)), %% 获取真正的tag,其中包含了类型{tag
                             part(Rest, max(0, E), 0)] %% 去掉 }
                    end
            end
    end.

-spec keys(binary()) -> [key()].
keys(Bin0) ->
    Bin1 = << <<X:8>> || <<X:8>> <= Bin0, X =/= $  >>,
    case Bin1 =:= <<>> orelse Bin1 =:= <<".">> of
        true  -> [Bin1];
        false -> [X || X <- binary:split(Bin1, <<".">>, [global]), X =/= <<>>]
    end.
-spec remove_space_from_head(binary()) -> binary().
remove_space_from_head(<<X:8, Rest/binary>>) 
    when X =:= $\t; X =:= $  -> 
            remove_space_from_head(Rest);
remove_space_from_head(Bin) -> Bin.

-spec remove_space_from_tail(binary()) -> binary().
remove_space_from_tail(<<>>) -> <<>>;
remove_space_from_tail(Bin) ->
    PosList = binary:matches(Bin, <<" ">>),
    LastPos = remove_space_from_tail_impl(lists:reverse(PosList), byte_size(Bin)),
    binary:part(Bin, 0, LastPos).

-spec remove_space_from_tail_impl([{non_neg_integer(), pos_integer()}], non_neg_integer()) -> non_neg_integer().
remove_space_from_tail_impl([{X, Y} | T], Size) when Size =:= X + Y -> remove_space_from_tail_impl(T, X);
remove_space_from_tail_impl(_, Size) ->Size.

%% standalone模式, standalone并不影响一般的tag，只影响section和comment
%% 如果\r\n{{ ... }}\r\n，tag后面的\r\n被视为tag的一部分
%% 如果\s{{ ... }}\n,\s{{ ... }}\r\n tag后面的\n和\r\n被视为tag的一部分
%% 如果是partials,Ident需要被保留
-spec standalone(#state{}, binary(), [tag()]) -> {#state{}, StashPre :: binary(), Post :: binary(), [tag()]}.
standalone(#state{standalone = false} = State, Post, [Pre | Result]) ->
    {State, <<>>, Post, ?ADD(Pre, Result)};
standalone(#state{standalone = false} = State, Post, Result) ->
    {State, <<>>, Post, Result};
standalone(State, Post0, Result0) ->
    {Pre, Result1} = case Result0 =/= [] andalso hd(Result0) of
                         Pre0 when is_binary(Pre0) -> {Pre0, tl(Result0)};
                         _                         -> {<<>>, Result0}
                     end,
    case remove_space_from_head(Pre) =:= <<>> andalso remove_space_from_head(Post0) of
        <<"\r\n", Post1/binary>> ->
            {State, Pre, Post1, Result1};
        <<"\n", Post1/binary>> ->
            {State, Pre, Post1, Result1};
        <<>> ->
            {State, Pre, <<>>, Result1};
        _ ->
            {State#state{standalone = false}, <<>>, Post0, ?ADD(Pre, Result1)}
    end.


%% @doc If the binary is repeatedly the character, return true. Otherwise, return false.
-spec repeatedly_binary(binary(), byte()) -> boolean().
repeatedly_binary(<<X, Rest/binary>>, X) -> repeatedly_binary(Rest, X);
repeatedly_binary(<<>>, _) -> true;
repeatedly_binary(_, _) -> false.

%% @equiv binary:part(X, Start, byte_size(X) - Start + End)
-spec part(binary(), non_neg_integer(), 0 | neg_integer()) -> binary().
part(X, Start, End) when End =< 0 -> binary:part(X, Start, byte_size(X) - Start + End).

-spec binary_join(BinaryList :: [binary()], Separator :: binary()) -> binary().
binary_join([], _) -> <<>>;
binary_join(Bins, Sep) ->
    [Hd | Tl] = [ [Sep, B] || B <- Bins ],
    erlang:iolist_to_binary([erlang:tl(Hd) | Tl]).