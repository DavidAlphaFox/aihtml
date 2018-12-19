-module(ai_mustache).
-export([render/2,render/3]).

%% 定义如下规则


%% Partils中包含的是子模板的代码
%% 例如模板A中使用了同路径下的模板B和非同路径下的模板C,C又使用了自己同路径下的B模板
%% Partails中的结构如下
%% #{B => IRB,"Path" => #{B => IRB,C => IRC} }

%% Ctx 中对于子模板，会自动获取名字空间并进行合并
%% 例如模板A中使用了同路径下的模板B和非同路径下的模板C,C又使用了自己同路径下的B模板
%% Ctx中的结构是
%% #{.B => #{<<"items">> => 1},.Patch => #{.C => ...., .B => #{<<"items">> => 2}},Other => {...} }
%% 在渲染A路径下的B模板的时候，Ctx会变成如下情况
%% #{.B => #{<<"items">> => 1},.Patch => #{.C => ...., .B => #{<<"items">> => 2}},Other => {...},<<"items">> => 1 }
%% 在渲染C模板的时候，Ctx会变成如下情况
%% #{.B => #{<<"items">> => 2},.Patch => #{.C => ...., .B => #{<<"items">> => 2}},Other => {...},.C => .... }
%% 在渲染C模板下的B模板时候，Ctx会变成如下情况
%% #{.B => #{<<"items">> => 2},.Patch => #{.C => ...., .B => #{<<"items">> => 2}},Other => {...} ,<<"items">> => 2,.C => .... }


render(Body,Ctx)->
    {IR,_Partials} = ai_mustache_parser:parse(Body),
    ai_mustache_runner:render(IR,#{},Ctx).
render(Body,Partils,Ctx)->
    {IR,_Partials} = ai_mustache_parser:parse(Body),
    ai_mustache_runner:render(IR,Partils,Ctx).