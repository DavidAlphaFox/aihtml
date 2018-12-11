-module(ai_mustache_context).

-export([new/0,new/1,to_list/1]).
-export([merge/2 ]).
-export([get_value/2,set_value/3]).
-export([partial/3,module/2]).

-define(MODULE_KEY, '__mod__').
-define(NEW_EXIT(Data), exit({improper_ctx, Data})).

new() -> new([]).

new(List) when is_list(List) ->
    try 
        maps:from_list(List)
    catch
        _:_ -> ?NEW_EXIT(List)
    end;
new(Data) when erlang:is_map(Data) ->Data;
new(Data) -> ?NEW_EXIT(Data).

to_list(Ctx) ->
    Ctx0  = maps:remove(?MODULE_KEY,Ctx),
    maps:to_list(Ctx0).


%% ===================================================================
%% Merge
%% ===================================================================

merge(Ctx1, Ctx2) -> maps:merge(Ctx1,Ctx2).

module(Module, Ctx) -> maps:put(?MODULE_KEY,Module,Ctx).
%% ===================================================================
%% Dynamic data module
%% ===================================================================

module(Ctx) ->
    case maps:get(?MODULE_KEY, Ctx,undefined) of 
        undefined -> {error, module_not_set};
        Value-> Value
    end.

%% ===================================================================
%% Module
%% ===================================================================
partial(Key,Fun,Ctx)-> maps:put(Key,Fun,Ctx).
set_value(Key,Value,Ctx)-> maps:put(Key,Value,Ctx).
get_value(Key, Ctx) ->
    case maps:get(Key,Ctx,undefined) of
        undefined -> 
            case get_from_module(Key, Ctx) of 
                {error,_} -> undefined;
                Value -> Value
            end;
        Value -> Value
    end.

get_from_module(Key, Ctx) ->
    case module(Ctx) of
        {error, _} -> undefined;
        Module -> 
            try
                Module:get_value(Key,Ctx) 
            catch
                _:_ -> 
                    {error, not_found}
            end
    end.