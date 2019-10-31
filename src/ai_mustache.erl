-module(ai_mustache).
-export([prepare/0,prepare/1,prepare/2]).
-export([render/2]).

%% 定义如下规则


%% Partils中包含的是子模板的代码
%% Partils是一个map
%% 结构为 #{path => IRCode}

%% Ctx 全局共享
%% 对于mustach中 {{ shared.name }} 的变量会自动去寻找
%% #{ "shared" => #{"name" => name} }

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

