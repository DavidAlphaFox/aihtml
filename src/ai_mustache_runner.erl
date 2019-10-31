-module(ai_mustache_runner).
-export([render/2,render/3]).
%% runner是一个栈式执行机器
%% section可以被认为是分支预测工具
-spec render({[term()],map()},map())-> binary().
render({IR,Partials},Ctx)->run(<<>>,IR,[],Partials,Ctx).

-spec render([term()],map(),map())-> binary().
render(IR,Partials,Ctx)-> run(<<>>,IR,[],Partials,Ctx).

run(Acc,[],[],_Partials,_Ctx)-> Acc;

run(Acc,[],[{H,OldCtx}|Stack],Partials,_Ctx)->
    run(Acc,H,Stack,Partials,OldCtx);

run(Acc,[{binary,Value}|IR],Stack,Partials,Ctx)->
    run(<<Acc/binary,Value/binary>>,IR,Stack,Partials,Ctx);
%% 正常的字符串
run(Acc,[{tag,none,Name}|IR],Stack,Partials,Ctx)->
    Value = ai_string:html_escape(ai_maps:get(Name,Ctx,<<"">>)),
    run(<<Acc/binary,Value/binary>>,IR,Stack,Partials,Ctx);
%% 不需要转换的原生字符串
run(Acc,[{tag,raw,Name}|IR],Stack,Partials,Ctx)->
    Value = ai_string:to_string(ai_maps:get(Name,Ctx,<<"">>)),
    run(<<Acc/binary,Value/binary>>,IR,Stack,Partials,Ctx);
%% 局部模版
run(Acc,[{tag,partial,Name }|IR],Stack,Partials,Ctx)->
    case map:get(Name,Partials,undefined) of
        undefined -> run(Acc,IR,Stack,Partials,Ctx);
        NewIR -> run(Acc,NewIR,[IR|Stack],Partials,Ctx)
    end;
%% section
run(Acc,[{section,Name,SectionIR,false}|IR],Stack,Partials,Ctx)->
    case ai_maps:get(Name,Ctx,undefined) of
        undefined ->  run(Acc,SectionIR,[IR|Stack],Partials,Ctx);
        false -> run(Acc,SectionIR,[IR|Stack],Partials,Ctx);
        true -> run(Acc,IR,Stack,Partials,Ctx);
        L when erlang:is_list(L)->
            case L of 
                [] -> run(Acc,SectionIR,[IR|Stack],Partials,Ctx);
                _ -> run(Acc,IR,Stack,Partials,Ctx)
            end;
        %% 此处是扩展，当一个函数返回false的时候，里面的section就会执行
        %% 实现简单的if false操作
        F when erlang:is_function(F,1) ->
            Hide = F(Ctx),
            if
                Hide == true -> run(Acc,IR,Stack,Partials,Ctx);
                true -> run(Acc,SectionIR,[IR|Stack],Partials,Ctx)
            end;
        _ ->
            run(Acc,IR,Stack,Partials,Ctx)
    end;
run(Acc,[{section,Name,SectionIR,true}|IR],Stack,Partials,Ctx)->
    case ai_maps:get(Name,Ctx,undefined) of 
        undefined ->  run(Acc,IR,Stack,Partials,Ctx);
        false -> run(Acc,IR,Stack,Partials,Ctx);
        true ->  run(Acc,SectionIR,[IR|Stack],Partials,Ctx);
        M when erlang:is_map(M)-> run(Acc,SectionIR,[IR|Stack],Partials,Ctx);
        L when erlang:is_list(L)->
            case L of 
                [] -> run(Acc,IR,Stack,Partials,Ctx); 
                _ -> 
                    run_section(Acc,SectionIR,L,[IR|Stack],Partials,Ctx)
            end;
        F when erlang:is_function(F,2) ->
            Acc0 = run(<<>>,SectionIR,[],Partials,Ctx),
            Acc1 = F(Acc0,Ctx),
            run(<<Acc/binary,Acc1/binary>>,IR,Stack,Partials,Ctx);
        %% 此处是扩展，当一个函数返回true的时候，里面的section会执行
        %% 实现简单if true 操作
        F when erlang:is_function(F,1) ->
            Run = F(Ctx),
            if
                Run == true -> run(Acc,SectionIR,[IR|Stack],Partials,Ctx);
                true ->  run(Acc,IR,Stack,Partials,Ctx)
            end;
        _->
            run(Acc,IR,Stack,Partials,Ctx)
    end.

run_section(Acc,_SectionIR,[],[IR|Stack],Partials,Ctx)->
     run(Acc,IR,Stack,Partials,Ctx);
run_section(Acc,SectionIR,[H|T],Stack,Partials,Ctx)->
     Acc0 = run(Acc,SectionIR,[],Partials,maps:merge(Ctx,H)),
     run_section(Acc0,SectionIR,T,Stack,Partials,Ctx).
