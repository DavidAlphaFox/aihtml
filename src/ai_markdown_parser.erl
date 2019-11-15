-module(ai_markdown_parser).
-export([scan_block/1]).
-record(node,{
              kind,
              attrs = [],
              children = [],
              value = <<>>
             }).

-define(BLOCK_SCANER,[
                      fun scan_newline/1,
                      fun scan_indented/1,
                      fun scan_code/1,
                      fun scan_blockquote/1,
                      fun scan_thematic/1,
                      fun scan_atx/1,
                      fun scan_list/1,
                      fun scan_paragraph/1
                     ]).



%accumulate_line(Line) -> accumulate_line(Line,<<>>).
%accumulate_line(<<>>,Acc)->{Acc,eof};
%accumulate_line(<<"\n",Line/binary>>,Acc)-> {<<Acc/binary>>,Line};
%accumulate_line(<<C/utf8,Line/binary>>,Acc) -> accumulate_line(Line,<<Acc/binary,C/utf8>>).

combine_backquote(Block)->
    combine_backquote(Block,[]).
combine_backquote([],Acc)-> lists:reverse(Acc);
combine_backquote([I],Acc)-> lists:reverse([I|Acc]);
combine_backquote([#node{kind = quote} = N1,
                   #node{kind = quote} = N2 | T],Acc)->
    N1Depth = proplists:get_value(depth,N1#node.attrs,0),
    N2Depth = proplists:get_value(depth,N2#node.attrs,0),
    if
        N1Depth >= N2Depth ->
            N1Value = N1#node.value,
            N2Value = N2#node.value,
            combine_backquote([#node{
                                  kind = quote,
                                  attrs = N1#node.attrs,
                                  value = <<N1Value/binary,N2Value/binary>>,
                                  children = N1#node.children ++ N2#node.children
                                 } | T],Acc);
        true ->
            Diff = N2Depth - N1Depth,
            Children = combine_backquote(
                         N1#node.children ++ [N2#node{attrs = [{depth,Diff}]}],
                         []),
            combine_backquote([N1#node{children = Children}|T], Acc)
    end;
combine_backquote([H|T],Acc) -> combine_backquote(T,[H|Acc]).

remove_space(<<"\s",Line/binary>>)-> remove_space(Line);
remove_space(<<"\t",Line/binary>>) ->remove_space(Line);
remove_space(Rest) -> Rest.


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
        {Node,Rest} when erlang:is_tuple(Node)-> scan_block(?BLOCK_SCANER,Rest,[Node|Acc]);
        {Nodes,Rest} -> scan_block(?BLOCK_SCANER,Rest,Nodes ++ Acc)
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

scan_blockquote(Line)-> scan_blockquote(Line,[]).

scan_blockquote(Line,Acc)->
    case scan_blockquote(remove_space(Line),<<>>,#node{kind = quote},start) of
        {Node, eof} -> {combine_backquote(lists:reverse([Node|Acc])),eof};
        {Node,Rest} -> scan_blockquote(Rest,[Node|Acc]);
        stop ->
            if erlang:length(Acc) > 0 ->
                    {combine_backquote(lists:reverse(Acc)),Line};
               true -> stop
            end
    end.

scan_blockquote(<<">",Line/binary>>,Buffer,Node,start)-> scan_blockquote(Line,Buffer,Node,{content,start});
scan_blockquote(_Line,_Buffer,_Node,start)-> stop;
scan_blockquote(<<"\s",Line/binary>>,Buffer,Node, {content,start})->
    scan_blockquote(Line,<<Buffer/binary,"\s">>,Node,{content,start});
scan_blockquote(<<"\t",Line/binary>>,Buffer,Node, {content,start})->
    scan_blockquote(Line,<<Buffer/binary,"\t">>,Node,{content,start});
scan_blockquote(<<">",Line/binary>>,_Buffer, #node{ attrs = Attrs} = Node, {content,start})->
    case proplists:get_value(depth,Attrs) of
        undefined -> scan_blockquote(Line,<<>>,Node#node{attrs = [{depth,1}| Attrs]},{content,start});
        N ->
            NewAttrs = lists:filter(fun({Key,_Attr}) -> Key /= depth end,Attrs),
            scan_blockquote(Line,<<>>,Node#node{attrs = [{depth,N + 1}| NewAttrs]},{content,start})
    end;
scan_blockquote(<<>>,Buffer,Node,{content,content})->
    {Node#node{value = <<Buffer/binary>>},eof};
scan_blockquote(<<"\n",Line/binary>>,Buffer,Node,{content,content})->
    {Node#node{value = <<Buffer/binary,"\n">>},Line};
scan_blockquote(<<C/utf8,Line/binary>>,Buffer,Node,{content,_}) ->
    scan_blockquote(Line,<<Buffer/binary,C/utf8>>,Node,{content,content}).
%% <hr />标签

scan_thematic(Line)-> scan_thematic(remove_space(Line),<<>>,#node{kind = hr},start).
scan_thematic(<<"_",Line/binary>>,Buffer,Node,start)->
  scan_thematic(Line,<<Buffer/binary,"_">>,Node,{content,underscore});
scan_thematic(<<"*",Line/binary>>,Buffer,Node,start)->
  scan_thematic(Line,<<Buffer/binary,"*">>,Node,{content,star});
scan_thematic(<<"-",Line/binary>>,Buffer,Node,start)->
  scan_thematic(Line,<<Buffer/binary,"-">>,Node,{content,dash});
scan_thematic(_Line,_Buffer,_Node,start) -> stop;

scan_thematic(<<"\s",Line/binary>>,Buffer,Node,{content,_} = S) ->
  scan_thematic(Line,Buffer,Node,S);
scan_thematic(<<"\t",Line/binary>>,Buffer,Node,{content,_} = S) ->
  scan_thematic(Line,Buffer,Node,S);
scan_thematic(<<"\n",Line/binary>>,Buffer,Node,{content,_} )->
  if 
    erlang:byte_size(Buffer) >= 3 -> {Node,Line};
    true -> stop
  end;
scan_thematic(<<C/utf8,Line/binary>>,Buffer,Node,{content,T} = S) ->
  case {C,T} of
    {$*, star} -> scan_thematic(Line,<<Buffer/binary,"*">>,Node,S);
    {$-, dash} -> scan_thematic(Line,<<Buffer/binary,"-">>,Node,S);
    {$_, underscore} -> scan_thematic(Line,<<Buffer/binary,"_">>,Node,S);
    _ -> stop
  end.
scan_atx(Line)-> scan_atx(remove_space(Line),<<>>,#node{kind = heading},start).

%% 扫描 #号
scan_atx(<<"#",Line/binary>>,Buffer,Node,start)-> scan_atx(Line,<<Buffer/binary,"#">>,Node,hash);
%% 如果start不是#号，直接停止
scan_atx(_Line,_Buffer,_Node,start)-> stop;
scan_atx(<<"#",Line/binary>>,Buffer,Node,hash)->
    NewBuffer = <<Buffer/binary,"#">>,
    if
        erlang:byte_size(NewBuffer) > 6 -> stop;
        true -> scan_atx(Line,NewBuffer,Node,hash)
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

scan_list(Line)-> scan_list(Line,<<>>,[],start).
scan_list(<<"\s",Line/binary>>,Buffer,Nodes,start)->
  scan_list(Line,<<Buffer/binary,"\s">>,Nodes,start);
%% 第一个li标签，空格不应该大于3
scan_list(<<"-",Line/binary>>,Buffer,[],start) ->
  Depth = erlang:byte_size(Buffer),
  if
    Depth > 3 -> stop;
    true ->
      scan_list(Line,<<>>,[#node{kind = ul}],content)
  end;
scan_list(<<"-",Line/binary>>,Buffer,Nodes,start) ->
  scan_list(Line,<<>>,[#node{kind = ul, attrs = [{depth,erlang:byte_size(Buffer)}]} | Nodes],content);
scan_list(_Line,_Buffer,[],start)-> stop;
scan_list(Line, Buffer, [Node|_] = Nodes, start) ->
  Depth = proplists:get_value(depth,Node#node.attrs),
  BufferSize = erlang:byte_size(Buffer),
  if 
    (BufferSize >= Depth) ->
      scan_list(Line,Buffer,Nodes,{content,block});
    true ->
      {Nodes,<<Buffer/binary,Line/binary>>}
  end;
scan_list(<<"\n",Line/binary>>,Buffer,[Node|T],content)->
  scan_list(Line,<<>>,[Node#node{value = <<Buffer/binary,"\n">>} | T],start);
scan_list(<<C/utf8,Line/binary>>,Buffer,Nodes,content) ->
  scan_list(Line,<<Buffer/binary,C/utf8>>,Nodes,content).
      

scan_paragraph(Line)->
    scan_paragraph(remove_space(Line),<<>>,#node{kind = paragraph}).
scan_paragraph(<<>>,Buffer,Node)-> {Node#node{value = Buffer},eof};
scan_paragraph(<<"\t",Line/binary>>,Buffer,Node)-> {Node#node{value = Buffer},Line};
scan_paragraph(<<"\s\s\s\s",Line/binary>>,Buffer,Node) -> {Node#node{value = Buffer},Line};
scan_paragraph(<<C/utf8,Line/binary>>,Buffer,Node) -> scan_paragraph(Line,<<Buffer/binary,C/utf8>>,Node).
