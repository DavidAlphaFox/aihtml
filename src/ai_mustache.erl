-module(ai_mustache).
-export([bootstrap/0,bootstrap/1,bootstrap/2]).
-export([reload/0]).
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

bootstrap()-> ai_mustache_loader:bootstrap().
bootstrap({suffix,Suffix}) -> ai_mustache_loader:bootstrap(undefined,Suffix);
bootstrap(ViewPath)-> ai_mustache_loader:bootstrap(ViewPath,undefined).
bootstrap(ViewPath,Suffix)->ai_mustache_loader:bootstrap(ViewPath,Suffix).
reload()-> ai_mustache_loader:reload().
