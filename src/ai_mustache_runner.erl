-module(ai_mustache_runner).
-export([render/3]).

-spec render([term()],map(),map())-> binary().
render(IR,Partials,Ctx)->
    run(IR,<<>>,[],Partials,Ctx).

run([],Acc,[],_Partials,_Ctx)-> Acc;
run([],Acc,[{H,OldCtx,OldPartials}|Stack],Partials,_Ctx)->
    run(H,Acc,Stack,Partials,OldCtx);
run([{binary,Value}|IR],Acc,Stack,Partials,Ctx)->
    run(IR,<<Acc/binary,Value/binary>>,Stack,Partials,Ctx);
run([{tag,none,Name}|IR],Acc,Stack,Partials,Ctx)->
    Value = ai_string:html_escape(maps:get(Name,Ctx,<<"">>)),
    run(IR,<<Acc/binary,Value/binary>>,Stack,Partials,Ctx);
run([{tag,raw,Name}|IR],Acc,Stack,Partials,Ctx)->
    Value = ai_string:to_string(maps:get(Name,Ctx,<<"">>)),
    run(IR,<<Acc/binary,Value/binary>>,Stack,Partials,Ctx);
run([{tag,partial,Name}|IR],Acc,Stack,Partials,Ctx)->
    case maps:get(Name,Partials,undefined) of 
        undefined ->
            run(IR,Acc,Stack,Partials,Ctx);
        {NewIR,undefined}->
            run(NewIR,Acc,[{IR,Ctx}|Stack],Partials,Ctx);
        {NewIR,NewCtx}->
            run(NewIR,Acc,[{IR,Ctx}|Stack],Partials,
                maps:merge(Ctx,NewCtx))
    end;
run([{section,Name,SectionIR,false}|IR],Acc,Stack,Partials,Ctx)->
    case maps:get(Name,Ctx,undefined) of 
        undefined ->  run(SectionIR,Acc,[{IR,Ctx}|Stack],Partials,Ctx);
        false -> run(SectionIR,Acc,[{IR,Ctx}|Stack],Partials,Ctx);
        true -> run(IR,Acc,Stack,Partials,Ctx);
        L when erlang:is_list(L)->
            case L of 
                [] -> run(SectionIR,Acc,[{IR,Ctx}|Stack],Partials,Ctx);
                _ -> run(IR,Acc,Stack,Partials,Ctx)
            end;
        _ ->
            run(IR,Acc,Stack,Partials,Ctx)
    end;
run([{section,Name,SectionIR,true}|IR],Acc,Stack,Partials,Ctx)->
    case maps:get(Name,Ctx,undefined) of 
        undefined ->  run(IR,Acc,Stack,Partials,Ctx);
        false -> run(IR,Acc,Stack,Partials,Ctx);
        true ->  run(SectionIR,Acc,[{IR,Ctx}|Stack],Partials,Ctx);
        L when erlang:is_list(L)->
            case L of 
                [] -> run(IR,Acc,Stack,Partials,Ctx); 
                _ -> 
                    run(SectionIR,Acc,L,[{IR,Ctx}|Stack],Partials,Ctx)
            end;
        F when erlang:is_function(F,2) ->
            Acc0 = run(SectionIR,<<>>,[],Partials,Ctx),
            Acc1 = F(Acc0,Ctx),
            run(IR,<<Acc/binary,Acc1/binary>>,Stack,Partials,Ctx);
        _->
            run(IR,Acc,Stack,Partials,Ctx)
    end.
run(_SectionIR,Acc,[],[{IR,OldCtx}|Stack],Partials,_Ctx)->
     run(IR,Acc,Stack,Partials,OldCtx);
run(SectionIR,Acc,[H|T],Stack,Partials,Ctx)->
     Acc0 = run(SectionIR,Acc,[],Partials,maps:merge(Ctx,H)),
     run(SectionIR,Acc0,T,Stack,Partials,Ctx).