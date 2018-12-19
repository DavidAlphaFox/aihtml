-module(ai_mustache).
-export([render/2,render/3]).

render(Body,Ctx)->
    {IR,_Partials} = ai_mustache_parser:parse(Body),
    ai_mustache_runner:render(IR,#{},Ctx).
render(Body,Partils,Ctx)->
    {IR,_Partials} = ai_mustache_parser:parse(Body),
    ai_mustache_runner:render(IR,Partils,Ctx).