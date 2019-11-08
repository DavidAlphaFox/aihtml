-module(ai_markdown_parser).
-export([parse/1]).
-export([scan_block/1]).
-record(node,{
              kind,
              attrs = [],
              value = <<>>
             }).

parse(Markdown) ->
    Tokens = tokenize(Markdown,<<>>,[]),
    parse(Tokens,[],[]).


tokenize(<<>>,<<>>,Acc)-> lists:reverse(Acc);
tokenize(<<>>,Buffer, Acc)-> lists:reverse(maybe_buffer(Buffer,Acc));

tokenize(<<"-" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest,<<>>, [minus | maybe_buffer(Buffer,Acc)]);
tokenize(<<"#" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [hash |  maybe_buffer(Buffer,Acc)]);
tokenize(<<">" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [gt | maybe_buffer(Buffer,Acc)]);
tokenize(<<"+" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [plus | maybe_buffer(Buffer,Acc)]);
tokenize(<<"*" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [star | maybe_buffer(Buffer,Acc)]);
tokenize(<<"_" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [underscore |  maybe_buffer(Buffer,Acc)]);
tokenize(<<"1" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{num, "1"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"2" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{num, "2"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"3" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{num, "3"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"4" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{num, "4"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"5" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{num, "5"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"6" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{num, "6"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"7" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{num, "7"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"8" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{num, "8"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"9" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{num, "9"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"0" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{num, "0"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"." , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [dot | maybe_buffer(Buffer,Acc)]);
tokenize(<<":" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [colon | maybe_buffer(Buffer,Acc)]);
tokenize(<<"'" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [single_quote | maybe_buffer(Buffer,Acc)]);
tokenize(<<"\"" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [double_quote | maybe_buffer(Buffer,Acc)]);
tokenize(<<"`" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [back_tick | maybe_buffer(Buffer,Acc)]);
tokenize(<<"!" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [bang | maybe_buffer(Buffer,Acc)]);
tokenize(<<"\\" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [back_slash | maybe_buffer(Buffer,Acc)]);
tokenize(<<"/" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [slash | maybe_buffer(Buffer,Acc)]); 
tokenize(<<"(" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{parentheses, open} | maybe_buffer(Buffer,Acc)]);
tokenize(<<")" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{parentheses, close} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"[" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{bracket, open} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"]" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{bracket, close} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"\s" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, maybe_combine([{{ws, sp}, " "} | maybe_buffer(Buffer,Acc)]));
tokenize(<<"\t" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, maybe_combine([{{ws, tab}, "\t"} | maybe_buffer(Buffer,Acc)]));
tokenize(<<"\r\n", Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{lf, crlf} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"\n", Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{lf, lf} | maybe_buffer(Buffer,Acc)]);
tokenize(<<C/utf8,Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<Buffer/binary,C/utf8>> , Acc).

maybe_buffer(<<>>,Acc) -> Acc;
maybe_buffer(Buffer,Acc)-> [{binary,Buffer}|Acc].


maybe_combine([{{ws,_},W1},{{ws,_},W2}|Acc]) -> [{{ws,comb},W1 ++ W2}|Acc];
maybe_combine(Acc) -> Acc.

parse([],[],Acc) -> reduce(Acc);
parse([],Stack,Acc) ->
    Lines = reduce(lists:reverse(Stack)),
    reduce(Lines ++ Acc);
parse([{lf,_}|T],[],Acc)->
    parse(T,[],[blank|Acc]);
parse([{lf,_}|T],Stack,Acc)->
    Lines = reduce(lists:reverse(Stack)),
    parse(T,[],Lines ++ Acc);
parse([{{ws,tab},_} | T ],[],Acc)->
    {Content,Rest} = binary_line(T,[{binary,<<>>}]),
    parse(Rest,[],[{pre,Content}|Acc]);
parse([{{ws,comb},[$\s,$\s,$\s,$\s|W]}|T],[],Acc)->
    {Content,Rest} = binary_line([{{ws,comb},W} | T],[{binary,<<>>}]),
    parse(Rest,[],[{pre,Content}|Acc]);
parse([{{ws,comb},[$\t|W]} | T ],[],Acc)->
    {Content,Rest} = binary_line([{{ws,comb},W} | T],[{binary,<<>>}]),
    parse(Rest,[],[{pre,Content}|Acc]);
parse([gt|T],[],Acc)->
    {Line,Rest} = strip_line(T,[]),
    Content = parse(Line,[],[]),
    parse(Rest,[],[{quote,Content}|Acc]);
parse([{num,N2}|T],[{num,N1}|ST],Acc)->
    parse(T,[{num,N1 ++ N2}|ST],Acc);
parse([dot|T],[{num,_}],[{ol,N,_}|_T] = Acc)->
    parse(T,[{ol,N+1}],Acc);
parse([dot|T],[{num,_}],Acc)->
    parse(T,[{ol,1}],Acc);

parse([minus|T],[],Acc)->
    parse(T,[li],Acc);
parse([star|T],[],Acc)->
    parse(T,[li],Acc);

parse([back_tick|T],[],Acc)->
    parse(T,[back_tick],Acc);
parse([back_tick|T],Stack,Acc)->
    NewStack = reduce(Stack,[],back_tick),
    parse(T,NewStack,Acc);

parse([underscore|T],[],Acc)->
    parse(T,[underscore],Acc);
parse([underscore|T],Stack,Acc)->
    NewStack = reduce(Stack,[],underscore),
    parse(T,NewStack,Acc);

parse([hash|T],[],Acc) ->
    parse(T,[{hash,1}], Acc);
parse([hash|T],[{hash,N}],Acc) ->
    parse(T,[{hash,N+1}],Acc);


parse([H|T],Stack,Acc) ->
    parse(T,[H|Stack],Acc).


strip_line([],Acc)->{lists:reverse(Acc),[]};
strip_line([{lf,_}|T],Acc)->{lists:reverse(Acc),T};
strip_line([H|T],Acc) -> strip_line(T,[H|Acc]).

binary_line([],[Acc])->{Acc,[]};
binary_line([{lf,_}|T],[Acc])->{Acc,T};
binary_line([H|T],Acc) ->
    NewAcc = token_to_string(H,Acc),
    binary_line(T,NewAcc).


reduce([],Stack,Expect)->[Expect|lists:reverse(Stack)];
reduce([Expect|T],Stack,Expect)-> [{Expect,lists:reverse(reduce(Stack))}|T];
reduce([H|T],Stack,Expect)-> reduce(T,[H|Stack],Expect).

reduce([li| T])->
    Stack = lists:foldl(fun token_to_string/2,[],T),
    [{li,Stack}];
reduce([{ol,N}|T])->
    Stack = lists:foldl(fun token_to_string/2,[],T),
    [{ol,N,Stack}];
reduce([{hash,N}|T]) ->
    Stack = lists:foldl(fun token_to_string/2,[],T),
    [{h,N,Stack}];
reduce(L)-> lists:foldl(fun token_to_string/2,[],L).

token_to_string({num,N},Acc) ->
    BinNum = ai_string:to_string(N),
    maybe_append(Acc,BinNum);
token_to_string(minus,Acc) -> maybe_append(Acc,<<$->>);
token_to_string(plus,Acc) -> maybe_append(Acc,<<$+>>);
token_to_string(star,Acc) -> maybe_append(Acc,<<$*>>);
token_to_string(gt,Acc) ->  maybe_append(Acc,<< $> >>);
token_to_string(underscore,Acc)-> maybe_append(Acc,<<$_>>);
token_to_string(dot,Acc) -> maybe_append(Acc,<<$.>>);
token_to_string(colon,Acc)-> maybe_append(Acc,<<$:>>);
token_to_string(single_quote,Acc) -> maybe_append(Acc,<< $'>>);
token_to_string(double_quote,Acc) -> maybe_append(Acc,<<$">>);
token_to_string(back_tick,Acc) -> maybe_append(Acc,<<$`>>);
token_to_string(bang,Acc) -> maybe_append(Acc,<<$!>>);
token_to_string(back_slash,Acc) -> maybe_append(Acc,<<$\\>>);
token_to_string(slash,Acc) -> maybe_append(Acc,<<$/>>);
token_to_string(hash,Acc)-> maybe_append(Acc,<<$#>>);
token_to_string({parentheses,open},Acc) -> maybe_append(Acc,<< $(>>);
token_to_string({parentheses,close},Acc) -> maybe_append(Acc,<<$)>>);
token_to_string({bracket,open},Acc) -> maybe_append(Acc,<<$[>>);
token_to_string({bracket,close},Acc) -> maybe_append(Acc,<<$]>>);
token_to_string({{ws,sp},_},Acc) -> maybe_append(Acc,<<$\s>>);
token_to_string({{ws,tab},_},Acc) -> maybe_append(Acc,<<$\t>>);
token_to_string({{ws,comb},SP},Acc) ->
    SPbin = ai_string:to_string(SP),
    maybe_append(Acc,<<SPbin/binary>>);
token_to_string({lf,crlf},Acc) -> maybe_append(Acc,<<"\r\n">>);
token_to_string({lf,lf},Acc) -> maybe_append(Acc,<<"\n">>);
token_to_string({hash,N},Acc) ->
    Hash = lists:duplicate(N,$#),
    HashBin = ai_string:to_string(Hash),
    maybe_append(Acc,HashBin);
token_to_string({binary,Bin},Acc) -> maybe_append(Acc,Bin);
token_to_string(Other,Acc) ->[Other|Acc].


maybe_append([{binary,Bin}|T],Other)-> [{binary,<<Bin/binary,Other/binary>>}|T];
maybe_append(T,Other) ->[{binary,Other}|T].


remove_space(<<"\s",Line/binary>>)-> remove_space(Line);
remove_space(<<"\t",Line/binary>>) ->remove_space(Line);
remove_space(Rest) -> Rest.

-define(BLOCK_SCANER,[
                      fun scan_newline/1,
                      fun scan_indented/1,
                      fun scan_code/1,
                      fun scan_atx/1
                     ]).

scan_block(Line) -> scan_block(?BLOCK_SCANER,Line,[]).
scan_block(_,eof,Acc)-> lists:reverse(Acc);
scan_block(_,<<>>,Acc)-> lists:reverse(Acc);
scan_block([],Line,Acc)-> scan_block(?BLOCK_SCANER,Line,Acc);
scan_block([H|T],Line,Acc) ->
    MethodSize = erlang:length(T),
    case erlang:apply(H,[Line]) of
        stop ->
            if
                MethodSize == 0 -> {error,lists:reverse(Acc),Line};
                true -> scan_block(T,Line,Acc)
            end;
        {Node,Rest} -> scan_block(?BLOCK_SCANER,Rest,[Node|Acc])
end.

scan_newline(Line)-> scan_newline(remove_space(Line),#node{kind=newline}).
scan_newline(<<"\n",Line/binary>>,Node)-> {Node,Line};
scan_newline(_Line,_Node) -> stop.

scan_indented(Line) -> scan_indented(Line,#node{kind=code},start).
scan_indented(<<"\s\s\s\s",Line/binary>>,Node,start)-> scan_indented(Line,Node,content);
scan_indented(<<"\t",Line/binary>>,Node,start) -> scan_indented(Line,Node,content);
scan_indented(<<"\n",Line/binary>>,Node,content) -> {Node,Line};
scan_indented(<<C/utf8,Line/binary>>,#node{value = Acc} = Node,content) ->
    scan_indented(Line,Node#node{value= <<Acc/binary, C/utf8>>},content);
scan_indented(_Line,_Node,_State) -> stop.

scan_code(Line)-> scan_code(remove_space(Line),<<>>,#node{kind = code},start).

scan_code(<<"```",Line/binary>>,Buffer,Node,start)-> scan_code(Line,Buffer,Node,flag);
scan_code(<<"~~~",Line/binary>>,Buffer,Node,start)-> scan_code(Line,Buffer,Node,flag);
scan_code(_Line,_Buffer,_Node,start) -> stop;
scan_code(<<"\n",Line/binary>>,Buffer, #node{attrs = Attrs} = Node,flag) ->
    case string:trim(Buffer) of
        <<>> -> scan_code(Line,<<>>,Node,content);
        Lang -> scan_code(Line,<<>>,Node#node{ attrs = [{lang,Lang} | Attrs] },content)
    end;
scan_code(<<C/utf8,Line/binary>>,Buffer,Node,flag) ->
    scan_code(Line,<<Buffer/binary,C/utf8>>,Node,flag);
scan_code(<<"```",Line/binary>>,Buffer,Node,content)-> scan_code(remove_space(Line),Buffer,Node,space);
scan_code(<<"~~~",Line/binary>>,Buffer,Node,content)-> scan_code(remove_space(Line),Buffer,Node,space);
scan_code(<<"\n",Line/binary>>,Buffer,Node,space)-> {Node#node{value = Buffer},Line};
scan_code(<<C/utf8,Line/binary>>,Buffer,Node,content) -> scan_code(Line,<<Buffer/binary,C/utf8>>,Node,content).



scan_atx(Line)-> scan_atx(remove_space(Line),<<>>,#node{kind = heading},start).

%% 扫描 #号
scan_atx(<<"#",Line/binary>>,Buffer,Node,start)-> scan_atx(Line,<<Buffer/binary,"#">>,Node,hash);
%% 如果start不是#号，直接停止
scan_atx(_Line,_Buffer,_Node,start)-> stop;
scan_atx(<<"#",Line/binary>>,Buffer,Node,hash)->
    NewBuffer = <<Buffer/binary,"#">>,
    if
        erlang:byte_size(NewBuffer) > 6 -> stop;
        true -> scan_atx(Line,NewBuffer,Node,start)
    end;
scan_atx(Line,Buffer,#node{ attrs = Attrs } = Node,hash) ->
    AtxSize = erlang:byte_size(Buffer),
    if
        AtxSize > 6 -> stop;
        true -> scan_atx(Line,<<>>,
                         Node#node{
                           attrs = [{depth,AtxSize}|Attrs]
                          },{content,start})
    end;
%% #后面的空格
scan_atx(<<"\s",Line/binary>>, Buffer,Node,{content,start} = S)-> scan_atx(Line,Buffer,Node,S);
scan_atx(<<"\t",Line/binary>>, Buffer,Node,{content,start} = S)-> scan_atx(Line,Buffer,Node,S);

%% 内容中的空格
scan_atx(<<"\s",Line/binary>>,Buffer,Node,{content,content})-> scan_atx(Line,<<Buffer/binary,"\s">>,Node,{content,space});
scan_atx(<<"\t",Line/binary>>,Buffer,Node,{content,content})-> scan_atx(Line,<<Buffer/binary,"\t">>,Node,{content,space});

%% 扫描内容中空格
scan_atx(<<"\s",Line/binary>>, Buffer,Node,{content,space})-> scan_atx(Line,<<Buffer/binary,"\s">>,Node,{content,space});
scan_atx(<<"\t",Line/binary>>, Buffer,Node,{content,space})-> scan_atx(Line,<<Buffer/binary,"\t">>,Node,{content,space});
scan_atx(<<"#",Line/binary>>, Buffer,Node,{content,space})-> scan_atx(Line,<<Buffer/binary,"#">>,Node,{content,space});

scan_atx(<<>>,_Buffer,Node,_S)-> {Node,eof};
scan_atx(<<"\n",Line/binary>>,_Buffer,Node,_S) -> {Node,Line};
%% 内容
scan_atx(<<C/utf8,Line/binary>>,Buffer,#node{value = Acc } = Node,{content, T} = S)->
    case T of
        space ->
            scan_atx(Line,<<>>,
                     Node#node{ value = <<Acc/binary,Buffer/binary,C/utf8>> },
                     {content, content});
        hash ->
            scan_atx(Line,<<>>,
                     Node#node{ value =  <<Acc/binary,Buffer/binary,C/utf8>>} ,
                     {content,content});
        start ->
            scan_atx(Line,Buffer,
                     Node#node{ value = <<Acc/binary,C/utf8>>},
                     {content,content});
        _ ->
            scan_atx(Line,Buffer,Node#node{ value = <<Acc/binary,C/utf8>> },S)
    end.
