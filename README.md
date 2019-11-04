# aihtml

## Erlang Mustache Template Compiler 

Mustache is a framework-agnostic templating system that enforces separation of view logic from the template
file. Indeed, it is not even possible to embed logic in the template. This
allows templates to be reused across language boundaries and for other
language independent uses.

Working with Mustache means dealing with templates, views, and contexts.
Templates contain HTML (or some other format) and Mustache tags that specify
what data to pull in. A template can be either a string or a file (usually
ending in .mustache). Views are Erlang modules that can define functions that
are called and provide the data for the template tags. A context is an Erlang
dict that contains the current context from which tags can pull data. A few
examples will clarify how these items interact.

NOTE: This is alpha software. Do not use it in production without extensive
testing. The API may change at any time. It still lacks some of the features
of Mustache for Ruby and the performance (even with compiled templates) is not
yet where I'd like it to be.


### Installation


To compile the code, navigate to the Mustache.erl project root and issue:

    make

This will produce a `mustache.beam` file in the `ebin` directory that must be
included in the code path of projects that need it.


### The Simplest Example


The simplest example involves using a string template and a context from the REPL.

    Ctx = maps:from_list([{<<"planet">>, <<"World!">>}]).
    {IR,Partials} = ai_mustache_parser:parse(<<"Hello {{planet}}">>).
    ai_mustache_runner:render({IR,Partials},Ctx).
    
  

In line 1 we created a context that contains a value bound to the `planet`
tag. In line 2 we generate some IR code of the template.
In line 3 we render then template by passing in the context.


### Using as View Engine

please refer [examples/complex.erl](https://github.com/DavidAlphaFox/aihtml/blob/master/examples/complex.erl)

The engine will start a process named `ai_mustache_loader` to manage the templates under given directory.

If a template is a parials, the filename must be prefixed with underscore.

The engine can load the templates without `ai_mustche:prepare`, but I recommand that call this fucation once after the tmeplate engine is started. Because the engine use only one `gen_server` to load template which may be very slow when the request is arrived.



## Erlang版Mustache编译器

Mustache是一款和框架无关的模板引擎系统，着重于将逻辑和视图分开。因此Mustache模板甚至没有任何内嵌的逻辑，因此可以跨多个语言使用。

当前该版本是一个比较早期的版本，在内部虽然得到部分使用，但依然存在一定量的Bug，请谨慎使用


### 最简单的例子

这个例子只是在Erlang的REPL中使用一个字符串和一个简单的context。

    Ctx = maps:from_list([{<<"planet">>, <<"World!">>}]).
    {IR,Partials} = ai_mustache_parser:parse(<<"Hello {{planet}}">>).
    ai_mustache_runner:render({IR,Partials},Ctx).
    
  
第一行创建了一个非常简单的context，其中包行了 `planet` 标签。
第二行生成了模板的IR代码。而在第三行则使用IR代码和前面的context将木板进行了渲染。

### 做为视图引擎
请参考[examples/complex.erl](https://github.com/DavidAlphaFox/aihtml/blob/master/examples/complex.erl)

视图引擎将启动一个叫`ai_mustache_loader`的进行对指定目录下的所有模板进行管理

如果文件前面有一个`_`下划线，引擎则会认为这个文件为子模板。虽然引擎可以不使用`ai_mustche:prepare`进行预热，也可以按照需要逐个编译模板。
因为该引擎只使用了一个`gen_server`和两个ets表来管理所有的模板，所以当请求到达的时候才去遍历模板，并加载会比较缓慢，因此建议在引擎启动后立刻执行
`ai_mustche:prepare`进行预热，同时也可发现不正确的模板或引擎问题。
