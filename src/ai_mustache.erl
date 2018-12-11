-module(ai_mustache).

-export([compile/1,render/2]).

-record(state, {
  section_regex = undefined,
  tag_regex = undefined,
  level = 0
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
            level = 0
        }.

replace(T) ->
  re:replace(T, "\"", "\\\"", [global,{return,binary}]).

compile(T,State)->
    try
        Compiled = sections(T,State),
        case Compiled of 
            {function,TFun} -> {ok,TFun};
            {binary,Bin} -> {ok,fun(_Ctx)-> Bin end};
            empty -> {ok,fun(_Ctx)-> <<"">> end}
        end
    catch
      _:_ ->
        {error,compile_fail}
    end.

sections(T, State) ->
    section(T,[],State#state{level = State#state.level + 1}).
section(<<>>,[],_State)-> empty;
section(<<>>,Acc,_State)->
    Sections = lists:reverse(Acc),
    to_function(Sections);
section(T,Acc,State)->
    Res = re:run(T, State#state.section_regex, [{capture, all, binary}]),
    section(Res,T,Acc,State).
section({match, [_,Front,Kind,Name,Content,Back]},_T,Acc,State)->
    %% Front 中一定不会有Section了
    Tags = tags(Front,State),
    Section = compile_section(Kind,Name,Content,State),
    section(Back,[Section,Tags|Acc],State);
section(nomatch,T,Acc,State)->
    Tags = tags(T, State),
    Sections = lists:reverse([Tags|Acc]),
    to_function(Sections). 


compile_section(<<"#">>, Name, Content, State) ->
    Section = sections(Content, State),
    compile_section(Name,Section,true);
compile_section(<<"^">>,Name,Content,State)->
    Section = sections(Content, State),
    compile_section(Name,Section,false).
compile_section(Name,{function,Section},Expect)->
    Fun = 
        fun(Ctx)->
            case ai_mustache_context:get_value(Name,Ctx) of 
                undefined -> <<"">>;
                Expect -> Section(Ctx);
                MayList when erlang:is_list(MayList)->
                    if ( MayList == []) /= Expect ->
                            lists:foldl(fun(Item,Acc)->
                                TempCtx = ai_mustache_context:merge(Ctx,Item),
                                R = Section(TempCtx),
                                <<Acc/binary,R/binary>>
                            end,<<"">>,MayList);
                        true -> <<"">>
                    end;
                MayFun when erlang:is_function(MayFun) -> 
                    Items = MayFun(Ctx),
                    if  (Items == []) /= Expect -> 
                            lists:foldl(fun(Item,Acc)->
                                TempCtx = ai_mustache_context:merge(Ctx,Item),
                                R = Section(TempCtx),
                                <<Acc/binary,R/binary>>
                            end,<<"">>,Items);
                        true -> <<"">>
                    end;
                _ -> <<"">>
            end
        end,
    {function,Fun};
compile_section(Name,{binary,Section},Expect)->      
    Fun = 
        fun(Ctx)->
            case ai_mustache_context:get_value(Name,Ctx) of 
                undefined -> <<"">>;
                Expect -> Section;
                MayList when erlang:is_list(MayList)->
                    if  ( MayList == []) /= Expect -> Section;
                        true -> <<"">>
                    end;
                MayFun when erlang:is_function(MayFun)->
                    Items = MayFun(Ctx),
                    if (Items == []) /= Expect -> Section;
                        true -> <<"">>
                    end;
                _ -> <<"">>
            end
        end,
  {function,Fun}.

tags(T, State) ->
    tag(T,[],State#state{level = State#state.level + 1}).
tag(<<>>,[],_State)-> empty;
tag(<<>>,Acc,_State)->
    Tags = lists:reverse(Acc),
    to_function(Tags);
tag(T,Acc,State)->
	Res = re:run(T,State#state.tag_regex,[{capture,all,binary}]),
	tag(Res,T,Acc,State).
tag({match,[_,Front,K,Content,Back]},_T,Acc,State)->
	Kind = tag_kind(K),
	TagFun = compile_tag(Kind, Content),
    tag(Back,[TagFun,{binary,Front}|Acc],State);  
tag(nomatch,T,Acc,_State)->
    Tags = lists:reverse([{binary,T}|Acc]),
	to_function(Tags).

tag_kind(<<>>)-> none;
tag_kind(<<">">>)-> partial;
tag_kind(<<"{">>)-> raw;
tag_kind(<<"&">>)-> raw;
tag_kind(<<"!">>)-> comment.
compile_tag(comment,_Key)-> empty;
compile_tag(raw,Key)->
    Fun = 
        fun(Ctx)->
    		case ai_mustache_context:get_value(Key,Ctx)   of
				undefined -> <<"">>;
	    		Value -> ai_string:to_string(Value)
		 	end
		end,
	{function,Fun};
compile_tag(partial,Key)->
    Fun = 
        fun(Ctx) ->
            Partial = ai_mustache_context:get_value(Key,Ctx),
            Partial(Ctx) 
        end,
    {function,Fun};
compile_tag(_,Key)->
	Fun = 
        fun(Ctx)->
    		case ai_mustache_context:get_value(Key,Ctx) of
				undefined -> <<"">>;
	    		Value -> ai_string:html_escape(Value)
		 	end
		end,
	{function,Fun}.
to_function(List) ->
    FilterdList = lists:filter(fun(I)-> I /= empty end,List),
    {L1,Rest} = lists:foldl(fun(I,{Acc,NeedMerge})->
            case I of 
                {function,_}-> 
                    Value = erlang:length(NeedMerge),
                    if
                        Value == 0 -> {Acc ++ [I],NeedMerge};
                        true ->
                            MergeBinary = lists:foldl(fun(Bin,BinAcc)->
                                    <<BinAcc/binary,Bin/binary>>
                                end,<<>>,NeedMerge),
                            {Acc ++ [{binary,MergeBinary},I],[]}
                    end;
                {binary,Bin}->
                    {Acc,NeedMerge ++ [Bin]}
            end
        end,{[],[]},FilterdList),
    L2 = %% 合并binary
        case Rest of 
            [] -> L1;
            _ ->
                MergeBinary = 
                    lists:foldl(fun(Bin,BinAcc)->
                        <<BinAcc/binary,Bin/binary>>
                        end,<<>>,Rest),
                L1 ++ [{binary,MergeBinary}]
        end,
    case L2 of 
        [] -> empty;
        [One] -> One;
        _ ->
            Fun = fun(Ctx)-> 
                lists:foldl(fun(Item,Acc)->
                    RB = case Item of
                            {function,TagFun}-> TagFun(Ctx);
                            {binary,Bin}-> Bin
                        end,
                    <<Acc/binary,RB/binary>>
                end,<<"">>,L2)
            end,
            {function,Fun}
    end.