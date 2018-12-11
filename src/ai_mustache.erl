-module(ai_mustache).

-export([compile/1,render/2]).

-record(state, {
  section_regex = undefined,
  tag_regex = undefined,
  count = 0,
  blocks = []
}).

render(Body,Ctx)->
    case compile(Body) of 
        {ok,Fun}-> Fun(Ctx);
        Error -> Error
    end.
compile(Body) ->
  State = state(),
  CompiledTemplate = replace(Body),
  compile(CompiledTemplate,State).


state()->
    %% 匹配以下模式
    %% {{# Section}} Content {{/ Section}}
    SectionRegex = "^(.*?){{(#|\\^)([^}]*)}}\\s*(.+?){{/\\3}}\\s*(.*)",
	{ok, CompiledSectionRegex} = re:compile(SectionRegex, [dotall]),
	%% 匹配
	%% {{ Content }},{{{ Content }}},{{& Content }},{{! Content }}
	%% {{> Content }}
    TagRegex = "^(.*?){{(!|>|{|&)?(.+?)\\2?}}+(.*)",
    {ok, CompiledTagRegex} = re:compile(TagRegex, [dotall]),
    #state{ section_regex = CompiledSectionRegex, 
            tag_regex = CompiledTagRegex,
            count = 0,
            blocks = []
        }.

replace(T) ->
  re:replace(T, "\"", "\\\"", [global,{return,binary}]).

compile(T,State)-> 
    State1 = sections(T,State),
    Blocks = lists:reverse(State1#state.blocks),
    io:format("Blocks ~p~n",[Blocks]),
    to_code(Blocks).
    % try
    %     Compiled = sections(T,State),
    %     case Compiled of 
    %         {function,TFun} -> {ok,TFun};
    %         {binary,Bin} -> {ok,fun(_Ctx)-> Bin end};
    %         empty -> {ok,fun(_Ctx)-> <<"">> end}
    %     end
    % catch
    %   _:_ ->
    %     {error,compile_fail}
    % end.

sections(T, State) ->
    section(T,[],State).

section(<<>>,[],State)-> State;
section(<<>>,Acc,State)->
    Sections = lists:reverse(Acc),
    block(Sections,State);
section(T,Acc,State)->
    Res = re:run(T, State#state.section_regex, [{capture, all, binary}]),
    section(Res,T,Acc,State).
section({match, [_,Front,Kind,Name,Content,Back]},_T,Acc,State)->
    %% Front 中一定不会有Section了
    State1 = tags(Front,State),
    State2 = compile_section(Kind,Name,Content,State1),
    section(Back,Acc,State2);
section(nomatch,T,Acc,State)->
    State1 = tags(T, State),
    Sections = lists:reverse(Acc),
    block(Sections,State1).


compile_section(<<"#">>, Name, Content,#state{blocks = Blocks} = State) ->
    State1 = sections(Content, State#state{blocks = []}),
    State1#state{
            blocks = [{section,Name,State1#state.blocks,true}| Blocks]
    };

compile_section(<<"^">>,Name,Content,#state{blocks = Blocks}  = State)->
    State1 = sections(Content, State#state{blocks = []}),
    State1#state{
         blocks = [{section,Name,State1#state.blocks,false}| Blocks]
    }.
    


tags(T, State) ->
    tag(T,[],State).
tag(<<>>,[],State)-> State;
tag(<<>>,Acc,State)->
    Tags = lists:reverse(Acc),
    block(Tags,State);
tag(T,Acc,State)->
	Res = re:run(T,State#state.tag_regex,[{capture,all,binary}]),
	tag(Res,T,Acc,State).
tag({match,[_,Front,K,Content,Back]},_T,Acc,State)->
	Kind = tag_kind(K),
	TagFun = compile_tag(Kind, Content),
    tag(Back,[TagFun,{binary,Front}|Acc],State);  
tag(nomatch,T,Acc,State)->
    Tags = lists:reverse([{binary,T}|Acc]),
	block(Tags,State).

tag_kind(<<>>)-> none;
tag_kind(<<">">>)-> partial;
tag_kind(<<"{">>)-> raw;
tag_kind(<<"&">>)-> raw;
tag_kind(<<"!">>)-> comment.

compile_tag(comment,_Key)->empty;
compile_tag(Kind,Key)->{tag,Kind,Key}.

% compile_tag(raw,Key)->
%     Fun = 
%         fun(Ctx)->
%     		case ai_mustache_context:get_value(Key,Ctx)   of
% 				undefined -> <<"">>;
% 	    		Value -> ai_string:to_string(Value)
% 		 	end
% 		end,
% 	{function,Fun};
% compile_tag(partial,Key)->
%     Fun = 
%         fun(Ctx) ->
%             Partial = ai_mustache_context:get_value(Key,Ctx),
%             Partial(Ctx) 
%         end,
%     {function,Fun};
% compile_tag(_,Key)->
% 	Fun = 
%         fun(Ctx)->
%     		case ai_mustache_context:get_value(Key,Ctx) of
% 				undefined -> <<"">>;
% 	    		Value -> ai_string:html_escape(Value)
% 		 	end
% 		end,
% 	{function,Fun}.
block(List,#state{count = Count, blocks = Blocks} =  State) ->
    N = Count + 1,
    case List of 
        [] -> State#state{count = N};
        _-> State#state{blocks = [{block,N,List}|Blocks],count = N}
    end.
    % case L2 of 
    %     [] -> empty;
    %     [One] -> One;
    %     _ ->
    %         Fun = fun(Ctx)-> 
    %             lists:foldl(fun(Item,Acc)->
    %                 RB = case Item of
    %                         {function,TagFun}-> TagFun(Ctx);
    %                         {binary,Bin}-> Bin
    %                     end,
    %                 <<Acc/binary,RB/binary>>
    %             end,<<"">>,L2)
    %         end,
    %         {function,Fun}
    % end.

to_code(Blocks)->
    to_code(Blocks,[],[],[]).
to_code([],Blocks,Sections,Tags)->
    {Blocks,Sections,Tags};
to_code([{tag,_Kind,_Key} = I|T],Blocks,Sections,Tags)->
    Code = generate(I),
    to_code(T,Blocks,Sections,[Code|Tags]);
to_code([{section,_Name,Inner,_Expect} = I|T],Blocks,Sections,Tags)->
    Code = generate(I),
    to_code(Inner ++T,Blocks,[Code|Sections],Tags);
to_code([{block,_N,Inner} = I|T],Blocks,Sections,Tags)->
    Code = generate(I),
    to_code(Inner ++ T,[Code|Blocks],Sections,Tags);
to_code([_H|T],Blocks,Sections,Tags)->to_code(T,Blocks,Sections,Tags).

generate({tag,_Kind,Key})-> 
    <<"tag(",Key/binary,",Ctx);">>;
generate({section,Name,Blocks,_Expect})->
    Header = <<"section(",Name/binary,",Ctx)->\r\n">>,
    Body = 
        lists:foldl(fun(I,Acc)->
                case I of
                    {tag,_Kind,Key}->
                        Data = <<"tag(",Key/binary,",Ctx);">>,
                        <<Acc/binary,Data/binary>>;
                    {block,N,_Items}->
                        ID = erlang:integer_to_binary(N),
                        Data = <<"block(",ID/binary,",Ctx),\r\n">>,
                        <<Acc/binary,Data/binary>>;
                    {section,SN,_SB,_SE}->
                        Data = <<"block(",SN/binary,",Ctx),\r\n">>,
                        <<Acc/binary,Data/binary>>;
                    {binary,Bin}->
                        <<Acc/binary,Bin/binary,"\r\n">>
                end
            end,<<"">>,Blocks),
     <<Header/binary,Body/binary>>;
generate({block,N,Blocks})->
    BID = erlang:integer_to_binary(N),
    Header = <<"block(",BID/binary,",Ctx)->\r\n">>,
    Body = 
        lists:foldl(fun(I,Acc)->
                case I of
                    {tag,_Kind,Key}->
                        Data = <<"tag(",Key/binary,",Ctx);">>,
                        <<Acc/binary,Data/binary>>;
                    {block,N1,_Items}->
                        ID = erlang:integer_to_binary(N1),
                        Data = <<"block(",ID/binary,",Ctx),\r\n">>,
                        <<Acc/binary,Data/binary>>;
                    {section,SN,_SB,_SE}->
                        Data = <<"block(",SN/binary,",Ctx),\r\n">>,
                        <<Acc/binary,Data/binary>>;
                    {binary,Bin}->
                        <<Acc/binary,Bin/binary,"\r\n">>
                end
            end,<<"">>,Blocks),
    <<Header/binary,Body/binary>>.