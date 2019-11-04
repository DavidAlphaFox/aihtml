-module(ai_markdown_parser).
-export([parse/1]).

parse(Markdown) ->
    Tokens = tokenize(Markdown,<<>>,[]),
    combine(Tokens,[]).


tokenize(<<>>,<<>>,Acc)-> lists:reverse(Acc);
tokenize(<<>>,Buffer, Acc)-> lists:reverse(maybe_buffer(Buffer,Acc));

tokenize(<<"-" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest,<<>>, [{{md, dash}, "-"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"#" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{md, atx}, "#"} |  maybe_buffer(Buffer,Acc)]);
tokenize(<<">" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{md, gt}, ">"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"+" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{md, plus}, "+"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"*" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{md, star}, "*"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"_" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{md, underscore}, "_"} |  maybe_buffer(Buffer,Acc)]);
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
tokenize(<<"." , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{punc, fullstop}, "."} | maybe_buffer(Buffer,Acc)]);
tokenize(<<":" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{punc, colon}, ":"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"'" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{punc, singleq}, "'"} | maybe_buffer(Buffer,Acc)]); %'
tokenize(<<"\"" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{punc, doubleq}, "\""} | maybe_buffer(Buffer,Acc)]); %"
tokenize(<<"`" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{punc, backtick}, "`"} | maybe_buffer(Buffer,Acc)]); %"
tokenize(<<"!" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{punc, bang}, "!"} | maybe_buffer(Buffer,Acc)]); %"
tokenize(<<"\\" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{punc, bslash}, "\\"} | maybe_buffer(Buffer,Acc)]); %"
tokenize(<<"/" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{punc, fslash}, "/"} | maybe_buffer(Buffer,Acc)]); %"
tokenize(<<"(" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{bra, "("} | maybe_buffer(Buffer,Acc)]);
tokenize(<<")" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{ket, ")"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"[" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{inline, open}, "["} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"]" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{inline, close}, "]"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"\s" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{ws, sp}, " "} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"\t" , Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{ws, tab}, "\t"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"\r\n", Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{lf, crlf}, "\r\n"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<"\n", Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<>>, [{{lf, lf}, "\n"} | maybe_buffer(Buffer,Acc)]);
tokenize(<<C/utf8,Rest/binary>>, Buffer, Acc)-> tokenize(Rest, <<Buffer/binary,C/utf8>> , Acc).

maybe_buffer(<<>>,Acc) -> Acc;
maybe_buffer(Buffer,Acc)-> [{binary,Buffer}|Acc].

combine([],Acc) -> lists:reverse(Acc);
combine([{num,N1},{num,N2} | T],Acc) ->
    combine([{num,N1 ++ N2} | T],Acc);
combine([{{ws, _}, W1}, {{ws, _}, W2} | T], Acc) ->
    combine([{{ws, comp}, W1 ++ W2} | T], Acc);
combine([H | T], Acc) -> combine(T, [H | Acc]).
