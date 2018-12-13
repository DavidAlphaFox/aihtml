-module(ai_mustache_compiler).

-export([compile/2]).

-record(state, {
  section_regex = undefined,
  tag_regex = undefined,
  count = 0,
  blocks = []
}).

compile(Name,Body) ->
  State = state(),
  CompiledTemplate = replace(Body),
  compile(Name,CompiledTemplate,State).


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

compile(Name,T,State)-> 
    State1 = sections(T,State),
    Blocks = lists:reverse(State1#state.blocks),
    {N,Code} = to_stack_code([{block,0,Blocks}]),
    FinalStep = erlang:integer_to_binary(N),
    CtxModule = erlang:atom_to_binary(Name,latin1),
    Module = ai_string:atom_suffix(Name,"_ai_mustache",false),
    ModuleName = erlang:atom_to_binary(Module,latin1),
    Acc0 = <<"-module(",ModuleName/binary,").\r\n-export([render/1,render/2,render/3,module/0]).\r\n-define(MODULE_KEY, '__mod__').\r\n">>,
    Acc1 = <<Acc0/binary,"module()->",CtxModule/binary,".\r\n
        render(Ctx) -> block(0,Ctx,<<\"\">>,[],[]).\r\n
        render(_Key,Ctx) -> block(0,Ctx,<<\"\">>,[],[]).\r\n
        render(_Key,Ctx,Acc)-> block(0,Ctx,Acc,[],[]).\r\n">>,
    Acc2 = lists:foldl(fun(I,CAcc)->
            <<CAcc/binary,I/binary>>
        end,Acc1,Code),
    Code0 = <<Acc2/binary,"block(",FinalStep/binary,",_Ctx,Acc,_L,_CtxStack)-> Acc.\r\n">>,
    Code1 = <<Code0/binary,"
            get_value(Key, Ctx) ->
                case maps:get(Key,Ctx,undefined) of
                    undefined -> get_from_module(Key,Ctx);
                    Value -> Value
                end.
            get_from_module(Key, Ctx) ->
                Module = module(),
                Res =
                    try Module:get_value(Key,Ctx) 
                    catch
                        _:_ -> {error,ai_mustache,value_not_set}
                    end,
                case Res of 
                    {error,ai_mustache, value_not_set}->
                        case maps:get(?MODULE_KEY,Ctx) of 
                            undefined-> undefined;
                            Modules -> get_from_parent(Modules,Key,Ctx)
                        end;
                    _-> Res
                end.       
            get_from_parent([],_Key,_Ctx)->undefined;
            get_from_parent([H|T],Key,Ctx)->
                Res = 
                    try H:get_value(Key,Ctx) 
                    catch
                        _:_ -> {error,ai_mustache, value_not_set}
                    end,
                case Res of
                    {error,ai_mustache, value_not_set} -> get_from_parent(T,Key,Ctx);
                    _-> Res
                end.
            add_to_ancestors(Ctx)->
                Modules = maps:get(?MODULE_KEY,Ctx,[]),
                Modules0 = [module()|Modules],
                maps:put(?MODULE_KEY,Modules0,Ctx).
            ">>,
    {module,Module} = ai_string:dynamic_module(erlang:atom_to_list(Module),erlang:binary_to_list(Code1)).

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
            blocks = [{section,Name,lists:reverse(State1#state.blocks),true}| Blocks]
    };

compile_section(<<"^">>,Name,Content,#state{blocks = Blocks}  = State)->
    State1 = sections(Content, State#state{blocks = []}),
    State1#state{
         blocks = [{section,Name,lists:reverse(State1#state.blocks),false}| Blocks]
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

block(List,#state{count = Count, blocks = Blocks} =  State) ->
    FilterdList = lists:filter(fun(I)-> I /= empty end,List),
    {L1,Rest} = lists:foldl(fun(I,{Acc,NeedMerge})->
            case I of 
                {binary,Bin}->
                    {Acc,NeedMerge ++ [Bin]};
                _ -> 
                    case NeedMerge of 
                        [] -> {Acc ++ [I],NeedMerge};
                        _ ->
                            MergeBinary = lists:foldl(fun(Bin,BinAcc)->
                                    <<BinAcc/binary,Bin/binary>>
                                end,<<>>,NeedMerge),
                            {Acc ++ [{binary,MergeBinary},I],[]}
                    end
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
    N = Count + 1,
    case L2 of 
        [] -> State;
        [One] -> State#state{blocks = [One|Blocks],count = N};
        _-> State#state{blocks = [{block,N,L2}|Blocks],count = N}
    end.


to_stack_code(Blocks)->
    to_stack_code(Blocks,[],0).
to_stack_code([],Acc,N)-> {N,lists:reverse(Acc)};
to_stack_code([{tag,_Kind,_Key} = I|T],Acc,N)->
    Code = generate_code(I,N),
    to_stack_code(T,[Code|Acc],N+1);
to_stack_code([{binary,_Bin} = I|T],Acc,N)->
    Code = generate_code(I,N),
    to_stack_code(T,[Code|Acc],N+1);
to_stack_code([{block,_,Inner} |T],Acc,N)->
    {Next,Code} = to_stack_code(Inner,[],N),
    to_stack_code(T, Code ++  Acc,Next);
to_stack_code([{section,_Name,Inner,_Expect} = I|T],Acc,N)->
    %% 如果是判断成功，需要执行的代码
    %% step N 判断是否进InnerCode
    %% step N+1 合并Ctx，并判断退出条件
    %% step Next 回到Step N + 1
    %% step Next+1 是Section 之后的代码
    {Next,InnerCode} = to_stack_code(Inner,[],N+2),
    AfterSection = Next + 1,
    StartSection = N + 1,
    JudgeCode = generate_code(I,N,AfterSection),
    StartCode = generate_code(begin_section,StartSection,AfterSection),
    EndCode = generate_code(end_section,Next,StartSection),
    Append =  [JudgeCode,StartCode| InnerCode] ++ [EndCode],
    NewAcc = Append ++ Acc,
    to_stack_code(T,NewAcc,AfterSection).

generate_code({tag,Kind,Key},N)->
    Step = erlang:integer_to_binary(N),
    NextStep = erlang:integer_to_binary(N+1),
    Header = <<"block(",Step/binary,",Ctx,Acc0,L,CtxStack)->">>,
    Body0 = <<"Acc1 = case get_value(<<\"",Key/binary, "\">>, Ctx) of ">>,
    Body1 = <<"undefined-> Acc0; ">>,
    Body2 = case Kind of 
        none -> <<"Value-> V = ai_string:html_escape(Value),<<Acc0/binary,V/binary>> end,">>;
        raw ->  <<"Value-> V = ai_string:to_string(Value),<<Acc0/binary,V/binary>> end,">>;
        partial-> <<"Value-> Ctx1 = add_to_ancestors(Ctx),Value:render(<<\"",Key/binary, "\">>,Ctx1,Acc0) end,">>;
        _ -> <<"_-> Acc0 end,">>
    end,
    Body3 = <<"block(",NextStep/binary,",Ctx,Acc1,L,CtxStack); \r\n">>,
    <<Header/binary,Body0/binary,Body1/binary,Body2/binary,Body3/binary>>;
generate_code({binary,Bin},N)->
    Step = erlang:integer_to_binary(N),
    NextStep = erlang:integer_to_binary(N+1),
    Header = <<"block(",Step/binary,",Ctx,Acc0,L,CtxStack)->">>,
    Bin1 = escape(Bin),
    Body0 = <<"Acc1 = << Acc0/binary,\"",Bin1/binary,"\">>, ">>,
    Body1 = <<"block(",NextStep/binary,",Ctx,Acc1,L,CtxStack);\r\n">>,
    <<Header/binary,Body0/binary,Body1/binary>>.

generate_code(begin_section,N,AfterSection)->
    Step = erlang:integer_to_binary(N),
    NextStep = erlang:integer_to_binary(N+1),
    EndStep = erlang:integer_to_binary(AfterSection),
    Header1 = <<"block(",Step/binary,",Ctx,Acc0,[],[{OldCtx,L}|CtxStack])->">>,
    Body1 = <<"block(",EndStep/binary,",OldCtx,Acc0,L,CtxStack);\r\n">>,
    Header2 = <<"block(",Step/binary,",Ctx,Acc0,[H|T],CtxStack)->">>,
    Body2 = <<"TempCtx = maps:merge(Ctx,H), ">>,
    Body3 = <<" block(",NextStep/binary,",TempCtx,Acc0,T,CtxStack);\r\n">>,
    <<Header1/binary,Body1/binary,Header2/binary,Body2/binary,Body3/binary>>;
generate_code(end_section,N,StartSection)->
    Step = erlang:integer_to_binary(N),
    LoopBackStep = erlang:integer_to_binary(StartSection),
    Header = <<"block(",Step/binary,",Ctx,Acc0,L,CtxStack)->">>,
    Body = <<"block(",LoopBackStep/binary,",Ctx,Acc0,L,CtxStack);\r\n">>,
    <<Header/binary,Body/binary>>;
generate_code({section,Name,_Blocks,Expect},N,Fail)->
    ExpectStr = ai_string:to_string(Expect),
    Step = erlang:integer_to_binary(N),
    NextStep = erlang:integer_to_binary(N+1),
    FailStep = erlang:integer_to_binary(Fail),

    Acc1 = <<"block(",Step/binary,",Ctx,Acc0,L,CtxStack) -> ">>,
    Acc2 = <<Acc1/binary,"Acc1 = case get_value(<<\"",Name/binary,"\">>,Ctx) of ">>,
    Acc3 = <<Acc2/binary, "undefined -> if false == ",
        ExpectStr/binary,"-> block(",NextStep/binary,",Ctx,Acc0,[Ctx],[{Ctx,L}|CtxStack]); 
        true -> block(",FailStep/binary,",Ctx,Acc0,L,CtxStack) end; ">>,
    Acc4 = <<Acc3/binary, ExpectStr/binary,"-> block(",
        NextStep/binary,",Ctx,Acc0,[Ctx],[{Ctx,L}|CtxStack]); ">>,
    Acc5 = <<Acc4/binary, "List when erlang:is_list(List)-> ">>,
    Acc6 = <<Acc5/binary," if (List == []) /= ",ExpectStr/binary,
        "-> CL = if List == [] -> [Ctx]; true -> List end,
        block(",NextStep/binary,",Ctx,Acc0,CL,[{Ctx,L}|CtxStack]);"
        " true -> block(", FailStep/binary,",Ctx,Acc0,L,CtxStack) end; ">>,
    Acc7 = <<Acc6/binary,"Function when erlang:is_function(Function)-> 
            List = Function(Ctx), if (List == []) /= ",ExpectStr/binary>>,
    <<Acc7/binary,
        "-> CL = if List == [] -> [Ctx]; true -> List end,
        block(",NextStep/binary,",Ctx,Acc0,CL,[{Ctx,L}|CtxStack]);"
        " true -> block(", FailStep/binary,",Ctx,Acc0,L,CtxStack) end; 
        _-> block(", FailStep/binary,",Ctx,Acc0,L,CtxStack) end; \r\n">>.


-define(ESCAPE_CHARS,[
    {"\n","\\\\n"},{"\t","\\\\t"},{"\b","\\\\b"},
    {"\r","\\\\r"},{"\"","\\\\\""}   
]).

escape(Str)->
    BinStr = ai_string:to_string(Str),
	lists:foldl(fun({El,Replace},Acc)->
			re:replace(Acc,El,Replace,[global,{return,binary}])
		end,BinStr,?ESCAPE_CHARS).