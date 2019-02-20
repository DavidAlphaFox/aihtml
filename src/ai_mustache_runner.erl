-module(ai_mustache_runner).
-export([render/2,render/3]).

-spec render({[term()],map()},map())-> binary().
render({IR,Partials},Ctx)->run(<<>>,IR,[],Partials,Ctx).

-spec render([term()],map(),map())-> binary().
render(IR,Partials,Ctx)-> run(<<>>,IR,[],Partials,Ctx).

run(Acc,[],[],_Partials,_Ctx)-> Acc;
run(Acc,[],[{H,OldCtx}|Stack],Partials,_Ctx)->
    run(Acc,H,Stack,Partials,OldCtx);
run(Acc,[{binary,Value}|IR],Stack,Partials,Ctx)->
    run(<<Acc/binary,Value/binary>>,IR,Stack,Partials,Ctx);
run(Acc,[{tag,none,Name}|IR],Stack,Partials,Ctx)->
    Value = ai_string:html_escape(ai_maps:get(Name,Ctx,<<"">>)),
    run(<<Acc/binary,Value/binary>>,IR,Stack,Partials,Ctx);
run(Acc,[{tag,raw,Name}|IR],Stack,Partials,Ctx)->
    Value = ai_string:to_string(ai_maps:get(Name,Ctx,<<"">>)),
    run(<<Acc/binary,Value/binary>>,IR,Stack,Partials,Ctx);
run(Acc,[{tag,partial,Name }|IR],Stack,Partials,Ctx)->
    Paths = binary:split(Name,<<"/">>,[global]),
    [H|T] = Paths,
    Key = [<<".",H/binary>>|T],
    case ai_maps:get(Paths,Partials,undefined) of 
        undefined -> run(Acc,IR,Stack,Partials,Ctx);
        NewIR ->
            case ai_maps:get(Key,Ctx,undefined) of 
                undefined -> 
										run(Acc,NewIR,[{IR,Ctx}|Stack],Partials,Ctx);
                NewCtx -> 
                    NewCtx0 = maps:merge(Ctx,NewCtx),
                    run(Acc,NewIR,[{IR,Ctx}|Stack],Partials,NewCtx0)
            end
    end;
run(Acc,[{section,Name,SectionIR,false}|IR],Stack,Partials,Ctx)->
    case ai_maps:get(Name,Ctx,undefined) of
        undefined ->  run(Acc,SectionIR,[{IR,Ctx}|Stack],Partials,Ctx);
        false -> run(Acc,SectionIR,[{IR,Ctx}|Stack],Partials,Ctx);
        true -> run(Acc,IR,Stack,Partials,Ctx);
        L when erlang:is_list(L)->
            case L of 
                [] -> run(Acc,SectionIR,[{IR,Ctx}|Stack],Partials,Ctx);
                _ -> run(Acc,IR,Stack,Partials,Ctx)
            end;
        _ ->
            run(Acc,IR,Stack,Partials,Ctx)
    end;
run(Acc,[{section,Name,SectionIR,true}|IR],Stack,Partials,Ctx)->
    case ai_maps:get(Name,Ctx,undefined) of 
        undefined ->  run(Acc,IR,Stack,Partials,Ctx);
        false -> run(Acc,IR,Stack,Partials,Ctx);
        true ->  run(Acc,SectionIR,[{IR,Ctx}|Stack],Partials,Ctx);
        M when erlang:is_map(M)-> run(Acc,SectionIR,[{IR,Ctx}|Stack],Partials,Ctx);
        L when erlang:is_list(L)->
            case L of 
                [] -> run(Acc,IR,Stack,Partials,Ctx); 
                _ -> 
                    run(Acc,SectionIR,L,[{IR,Ctx}|Stack],Partials,Ctx)
            end;
        F when erlang:is_function(F,2) ->
            Acc0 = run(<<>>,SectionIR,[],Partials,Ctx),
            Acc1 = F(Acc0,Ctx),
            run(<<Acc/binary,Acc1/binary>>,IR,Stack,Partials,Ctx);
        _->
            run(Acc,IR,Stack,Partials,Ctx)
    end.
run(Acc,_SectionIR,[],[{IR,OldCtx}|Stack],Partials,_Ctx)->
     run(Acc,IR,Stack,Partials,OldCtx);
run(Acc,SectionIR,[H|T],Stack,Partials,Ctx)->
     Acc0 = run(Acc,SectionIR,[],Partials,maps:merge(Ctx,H)),
     run(Acc0,SectionIR,T,Stack,Partials,Ctx).


