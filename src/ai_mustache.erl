-module(ai_mustache).
-export([prepare/0,prepare/1,prepare/2]).
-export([render/2]).

%% 定义如下规则


%% Partils中包含的是子模板的代码
%% 例如模板A中使用了同路径下的模板B和非同路径下的模板C,C又使用了自己同路径下的B模板
%% Partails中的结构如下
%% #{B => IRB,"Path" => #{B => IRB,C => IRC} }

%% Ctx 中对于子模板，会自动获取名字空间并进行合并
%% 例如模板A中使用了同路径下的模板B和非同路径下的模板C,C又使用了自己同路径下的B模板
%% Ctx中的结构是
%% #{<<".">> => #{B => #{<<"items">> => 1},Path => #{C => ...., B => #{<<"items">> => 2}}},Other => {...} }
%% 在渲染A路径下的B模板的时候，Ctx会变成如下情况
%% #{<<".">> => #{B => #{<<"items">> => 1},Path => #{C => ...., B => #{<<"items">> => 2}}},Other => {...},<<"items">> => 1 }
%% 在渲染C模板的时候，Ctx会变成如下情况
%% #{<<".">> => #{B => #{<<"items">> => 1},Path => #{C => ...., B => #{<<"items">> => 2}}},Other => {...},CtxC }
%% 在渲染C模板下的B模板时候，Ctx会变成如下情况
%% #{<<".">> => #{B => #{<<"items">> => 1},Path => #{C => ...., B => #{<<"items">> => 2}}},Other => {...}<<"items">> => 2}


render(Template,Ctx)->
    Run =
        case erlang:get(Template) of 
            undefined ->
                Code = ai_mustache_loader:template(Template),
                erlang:put(Template,Code),
                Code;
            Cache ->
                Cache
        end,
    ai_mustache_runner:render(Run,Ctx).

prepare()-> ai_mustache_loader:prepare().
prepare(ViewPath)-> ai_mustache_loader:prepare(ViewPath).
prepare(ViewPath,Suffix)-> ai_mustache_loader:prepare(ViewPath,Suffix).

