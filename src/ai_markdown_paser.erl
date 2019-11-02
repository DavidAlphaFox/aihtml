-module(ai_markdown_paser).
-export([parse/1]).

parse(Markdown) -> scan_block(Markdown,undefined, [], p).

scan_block(<<>>,undefined,Acc,p) -> lists:reverse(Acc);
scan_block(<<>>,Buffer,Acc,p) ->
    Span = scan_span(Buffer,<<"">>,[],text),
    lists:reverse([{p,Span} | Acc]);

%% code block use indente
scan_block(<<"\s\s\s\s",Rest/binary>>, undefined, Acc,p) -> scan_block(Rest,<<>>,Acc,{pre,tab});
%% remove
scan_block(<<"\s\s\s",Rest/binary>>, undefined, Acc,p) -> scan_block(Rest,undefined,Acc,p);
scan_block(<<"\s\s",Rest/binary>>, undefined, Acc,p) -> scan_block(Rest,undefined,Acc,p);

scan_block(<<"\t",Rest/binary>>, undefined, Acc,p) -> scan_block(Rest,<<>>,Acc,{pre,tab});
scan_block(<<"\r\n",Rest/binary>>, Buffer,Acc,{pre,tab})-> scan_block(Rest,undefined,[{pre,Buffer}|Acc],p);
scan_block(<<"\n",Rest/binary>>, Buffer,Acc,{pre,tab})-> scan_block(Rest,undefined,[{pre,Buffer}|Acc],p);

scan_block(<<"```",Rest/bits>>,undefined,Acc,p)->
    {Tag,Rest1} = scan_line(Rest,<<"">>),
    scan_block(Rest1,<<"">>,[{pre_open,Tag}|Acc],pre);
scan_block(<<"```",Rest/bits>>,Buffer,Acc,pre) ->
    Rest1 = remove_eof(Rest),
    scan_block(Rest1,undefined,[{pre_close,Buffer}|Acc],p);

scan_block(<<"######\s",Rest/binary>>,undefined,Acc,p) ->
    {Line,Rest1} = scan_line(Rest,<<"">>),
    Span = scan_span(Line,undefined,[],text),
    scan_block(Rest1,undefined,[{h,6,Span}|Acc],p);
scan_block(<<"#####\s",Rest/binary>>,undefined,Acc,p) ->
    {Line,Rest1} = scan_line(Rest,<<"">>),
    Span = scan_span(Line,undefined,[],text),
    scan_block(Rest1,undefined,[{h,5,Span}|Acc],p);


scan_block(<<"####\s",Rest/binary>>,undefined,Acc,p) ->
    {Line,Rest1} = scan_line(Rest,<<"">>),
    Span = scan_span(Line,undefined,[],text),
    scan_block(Rest1,undefined,[{h,4,Span}|Acc],p);

scan_block(<<"###\s",Rest/binary>>,undefined,Acc,p) ->
    {Line,Rest1} = scan_line(Rest,<<"">>),
    Span = scan_span(Line,undefined,[],text),
    scan_block(Rest1,undefined,[{h,3,Span}|Acc],p);

scan_block(<<"##\s",Rest/binary>>,undefined,Acc,p) ->
    {Line,Rest1} = scan_line(Rest,<<"">>),
    Span = scan_span(Line,undefined,[],text),
    scan_block(Rest1,undefined,[{h,2,Span}|Acc],p);
scan_block(<<"#\s",Rest/binary>>,undefined,Acc,p) ->
    {Line,Rest1} = scan_line(Rest,<<"">>),
    Span = scan_span(Line,undefined,[],text),
    scan_block(Rest1,undefined,[{h,1,Span}|Acc],p);

scan_block(<<"\r\n",Rest/binary>>,undefined,Acc,p) -> scan_block(Rest,undefined,[empty_line|Acc],p);
scan_block(<<"\n",Rest/binary>>,undefined,Acc,p) -> scan_block(Rest,undefined,[empty_line|Acc],p);

scan_block(<<"\r\n",Rest/binary>>,Buffer,Acc,p) ->
    Span = scan_span(Buffer,undefined,[],text),
    scan_block(Rest,undefined,[empty_line,{p,Span}|Acc],p);
scan_block(<<"\n",Rest/binary>>,Buffer,Acc,p) ->
    Span = scan_span(Buffer,undefined,[],text),
    scan_block(Rest,undefined,[empty_line,{p,Span}|Acc],p);
scan_block(<<C/utf8,Rest/binary>>,undefined,Acc,p) ->
    {Line,Rest1} = scan_line(Rest,<<"">>),
    scan_block(Rest1,<<C/utf8,Line/binary>>,Acc,p);
%% 默认增加字符串
scan_block(<<C/utf8,Rest/binary>>,Buffer,Acc,S) -> scan_block(Rest, <<Buffer/binary,C/utf8>>,Acc,S).


remove_eof(<<"\s", Rest/binary>>) -> remove_eof(Rest);
remove_eof(<<"\t", Rest/binary>>) -> remove_eof(Rest);
remove_eof(<<"\r\n", Rest/binary>>) -> Rest;
remove_eof(<<"\n", Rest/binary>>) -> Rest.

scan_line(<<"\r\n", Rest/binary>>, Acc)-> {Acc,Rest};
scan_line(<<"\n", Rest/binary>>, Acc)-> {Acc,Rest};
scan_line(<<C/utf8, Rest/binary>>,Acc)-> scan_line(Rest,<<Acc/binary,C/utf8>>).

scan_span(<<>>,undefined,Acc,text)->lists:reverse(Acc);
scan_span(<<>>,Buffer,Acc,_S)->lists:reverse([{text,Buffer}|Acc]);
scan_span(<<"`",Rest/binary>>,undefined,Acc,text)-> scan_span(Rest,undefined,Acc,single_quote);
scan_span(<<"`",Rest/binary>>,Buffer,Acc,text)-> scan_span(Rest,<<"">>,[{text,Buffer} | Acc],single_quote);
scan_span(<<"`",Rest/binary>>,Buffer,Acc,single_quote) ->scan_span(Rest,undefined,[{single_quote,Buffer}|Acc],text);
scan_span(<<C/utf8,Rest/binary>>,undefined,Acc,text) -> scan_span(Rest,<<C/utf8>>,Acc,text);
scan_span(<<C/utf8,Rest/binary>>,Buffer,Acc,S) -> scan_span(Rest,<<Buffer/binary,C/utf8>>,Acc,S).
