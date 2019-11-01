-module(ai_markdown_lex).
-export([scan/1]).

-record(state,{
               block = paragraph,
               acc = <<"">>,
               attr = none
              }).

scan(Markdown) ->
    Lines = split_line(Markdown),
    scan(Lines,[],#state{}).


split_line(Markdown)->
    split_line(Markdown, <<"">>,[]).

split_line(<<>>,BinAcc,Acc)->
    lists:reverse([BinAcc|Acc]);
split_line(<<C,Rest/bits>>,BinAcc,Acc) ->
    case C of
        $\n ->
            split_line(Rest, <<"">>, [BinAcc|Acc]);
        $\r ->
            split_line(Rest, BinAcc,Acc);
        _ ->
            split_line(Rest,<<BinAcc/binary,C>>,Acc)
    end.

scan([],Acc, #state{block = paragraph}) ->
    lists:reverse(Acc);
scan([], _Acc, _State) ->
    throw({error,unclosed_block});
scan([H|T],Acc,State) ->
    case scan_line(H, State) of
        {tag,Tag,NewState} -> scan(T,[Tag|Acc],NewState);
        {more,NewState} -> scan(T,Acc,NewState);
        NewState -> scan(T,Acc,NewState)
    end.


scan_line(<<>>,#state{block=paragraph} = State) -> State;
scan_line(<<$`,$`,$`,Rest/bits>>,#state{block = paragraph})->
    case erlang:byte_size(Rest) of
        0 -> {more,#state{
                      block = code,
                      acc = <<"">>,
                      attr = none
                     }};
        _ ->
            {more,#state{
                     block = code,
                     acc = <<"">>,
                     attr = Rest
                    }}
    end;
scan_line(<<$`,$`,$`, _Rest/bits>>,#state{block = code, acc = Acc,attr = Attr })->
    {tag,{code,Attr,Acc},#state{}};
scan_line(Bin,#state{block = code, acc = Acc} = State)->
    {more,State#state{acc = <<Acc/binary,Bin/binary>>}}.
