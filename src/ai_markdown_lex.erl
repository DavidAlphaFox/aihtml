-module(ai_markdown_lex).
-export([scan/1]).

%%-record(state,{
%%               tag = block,
%%               prev_tag = []
%%              }).

scan(Markdown) ->
    Lines = split_line(Markdown),
    Lines.


split_line(Markdown)->
    split_line(Markdown, <<"">>,[]).

split_line(<<>>,BinAcc,Acc)->
    lists:reverse([BinAcc|Acc]);
split_line(<<C,Rest/bits>>,BinAcc,Acc) ->
    case C of
        $\n ->
            split_line(Rest, <<"">>, [BinAcc|Acc]);
        _ ->
            split_line(Rest,<<BinAcc/binary,C>>,Acc)
    end.
