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
    {CBlocks,CSections,CTags} = to_code([{block,0,Blocks}]),
    CtxModule = erlang:atom_to_binary(Name,latin1),
    Module = ai_string:atom_suffix(Name,"_ai_mustache",false),
    ModuleName = erlang:atom_to_binary(Module,latin1),
    Acc0 = <<"-module(",ModuleName/binary,").\r\n-export([render/1,render/2,render/3,module/0]).\r\n-define(MODULE_KEY, '__mod__').\r\n">>,
    Acc1 = <<Acc0/binary,"module()->",CtxModule/binary,".\r\n
        render(Ctx) -> block(0,Ctx,<<\"\">>).\r\n
        render(_Key,Ctx) -> block(0,Ctx,<<\"\">>).\r\n
        render(_Key,Ctx,Acc)-> block(0,Ctx,Acc).\r\n">>,
    Acc2 = lists:foldl(fun({_,I},Acc)->
        <<Acc/binary,I/binary,"\r\n">>
        end,Acc1,maps:to_list(CBlocks)),
    Acc3 = <<Acc2/binary,"block(_ID,_Ctx,Acc)-> Acc.\r\n">>,
    Acc4 = lists:foldl(fun({_,I},Acc)->
            <<Acc/binary,I/binary,"\r\n">>
        end,Acc3,maps:to_list(CSections)),
    Acc5 = <<Acc4/binary,"section(_ID,_Ctx,Acc)-> Acc.\r\n">>,
    Acc6 = lists:foldl(fun({_,I},Acc)->
            <<Acc/binary,I/binary,"\r\n">>
        end,Acc5,maps:to_list(CTags)),
    Code = <<Acc6/binary,"tag(_ID,_Ctx,Acc)-> Acc.\r\n">>,
    Code1 = <<Code/binary,"
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

to_code(Blocks)->
    to_code(Blocks,#{},#{},#{}).
to_code([],Blocks,Sections,Tags)->
    {Blocks,Sections,Tags};
to_code([{tag,_Kind,Key} = I|T],Blocks,Sections,Tags)->
    Code = generate(I),
    to_code(T,Blocks,Sections,maps:put(Key,Code,Tags));
to_code([{section,Name,Inner,_Expect} = I|T],Blocks,Sections,Tags)->
    Code = generate(I),
    to_code(Inner ++T,Blocks,maps:put(Name,Code,Sections),Tags);
to_code([{block,N,Inner} = I|T],Blocks,Sections,Tags)->
    Code = generate(I),
    to_code(Inner ++ T,maps:put(N,Code,Blocks),Sections,Tags);
to_code([_H|T],Blocks,Sections,Tags)->to_code(T,Blocks,Sections,Tags).

generate({tag,Kind,Key})-> 
    Header = <<"tag(<<\"",Key/binary,"\">>,Ctx,Acc0)->">>,
    Body0 = <<"case get_value(<<\"",Key/binary, "\">>, Ctx) of ">>,
    Body1 = <<"undefined-> Acc0; ">>,
    Body2 = case Kind of 
            none -> <<"Value-> V = ai_string:html_escape(Value),<<Acc0/binary,V/binary>> end;">>;
            raw ->  <<"Value-> V = ai_string:to_string(Value),<<Acc0/binary,V/binary>> end;">>;
            partial-> <<"Value-> Ctx1 = add_to_ancestors(Ctx),Value:render(<<\"",Key/binary, "\">>,Ctx1,Acc0) end;">>;
            _ -> <<"_-> Acc0">>
        end,
    <<Header/binary,Body0/binary,Body1/binary,Body2/binary>>;
generate({section,Name,Blocks,Expect})->
    ExpectStr = ai_string:to_string(Expect),
    Body = generate_blocks(Blocks),
    Acc1 = <<"section(<<\"",Name/binary,"\">>,Ctx,Acc0) -> ">>,
    Acc2 = <<Acc1/binary,
        "Fun = fun(BlockCtx,BlockAcc0) -> true = erlang:is_map(BlockCtx),true = erlang:is_binary(BlockAcc0), ",
        Body/binary," end, ">>,
     Acc3 = <<Acc2/binary,"case get_value(<<\"",Name/binary,"\">>,Ctx) of ">>,
     Acc4 = <<Acc3/binary, "undefined -> Acc0; ",ExpectStr/binary,"-> Fun(Ctx,Acc0); " >>,
     Acc5 = <<Acc4/binary, "List when erlang:is_list(List)-> ">>,
     Acc6 = <<Acc5/binary," if (List == []) /= ",ExpectStr/binary,
        " -> lists:foldl(fun(Item,Acc)-> TempCtx = maps:merge(Ctx,Item),
                    Fun(TempCtx,Acc) end,Acc0,List); 
        true-> Acc0 end; ">>,
    Acc7 = <<Acc6/binary,"Function when erlang:is_function(Function)-> 
            List = Function(Ctx), if (List == []) /= ",ExpectStr/binary,"-> ">>,
    <<Acc7/binary,
        "lists:foldl(fun(Item,Acc)-> TempCtx = maps:merge(Ctx,Item), 
            R  = Fun(TempCtx,Acc) end,Acc0,List);
            true-> Acc0 end; 
        _-> Acc0 end;">>;
generate({block,ID,Blocks})->
    BID = erlang:integer_to_binary(ID),
    Header = <<"block(",BID/binary,",BlockCtx,BlockAcc0)->\r\n">>,
    Body = generate_blocks(Blocks),
    <<Header/binary,Body/binary,";">>.

generate_blocks(Blocks)->
    {N,Body} = 
        lists:foldl(fun(I,{N,Acc})->
                N1 = N+1,
                NBin = erlang:integer_to_binary(N),
                NBin1 = erlang:integer_to_binary(N1),
                AccStr = <<"BlockAcc",NBin/binary>>,
                AccStr1 = <<"BlockAcc",NBin1/binary>>,
                case I of
                    {tag,_Kind,Key}->
                        Data = <<AccStr1/binary," = tag(<<\"",Key/binary,"\">>,BlockCtx,",AccStr/binary,"),\r\n">>,
                        {N1,<<Acc/binary,Data/binary>>};
                    {block,BlockID,_Items}->
                        IDBin = erlang:integer_to_binary(BlockID),
                        Data = <<AccStr1/binary," = block(",IDBin/binary,",BlockCtx,",AccStr/binary,"),\r\n">>,
                        {N1,<<Acc/binary,Data/binary>>};
                    {section,SN,_SB,_SE}->
                        Data = <<AccStr1/binary," = section(<<\"",SN/binary,"\">>,BlockCtx,",AccStr/binary,"),\r\n">>,
                        {N1,<<Acc/binary,Data/binary>>};
                    {binary,Bin}->
                        Bin1 = escape(Bin),
                        Data = <<AccStr1/binary,"= <<",AccStr/binary,"/binary,\"",Bin1/binary,"\">>,\r\n">>,
                        {N1,<<Acc/binary,Data/binary>>}
                end
            end,{0,<<"">>},Blocks),
    NBin = erlang:integer_to_binary(N),
    Last = <<"BlockAcc",NBin/binary>>,
    <<Body/binary,Last/binary>>.

-define(ESCAPE_CHARS,[
    {"\n","\\\\n"},{"\t","\\\\t"},{"\b","\\\\b"},
    {"\r","\\\\r"},{"\"","\\\\\""}   
]).

escape(Str)->
    BinStr = ai_string:to_string(Str),
	lists:foldl(fun({El,Replace},Acc)->
			re:replace(Acc,El,Replace,[global,{return,binary}])
		end,BinStr,?ESCAPE_CHARS).