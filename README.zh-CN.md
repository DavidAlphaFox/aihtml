# aihtml

[English](README.md) · [中文](README.zh-CN.md)

把模板编译成 Erlang 模块的模板引擎。Mustache 与 **Jinja2**，并列提供。

模板在构建期由 rebar3 plugin 编译成 `.erl` 文件，因此运行期的渲染就是一次普通函数调用：没有进程，没有 ETS 表，不查任何表。

- 通过 [mustache spec](https://github.com/mustache/spec) 六个必选模块的全部 136 个用例
- 零依赖：库和测试套件都只依赖 OTP
- 静态模板文本进入模块的 literal pool，跨进程按引用共享

## 两套引擎

| | mustache | jinja |
|---|---|---|
| 后缀 / 前缀 | `.mustache` / `view_` | `.j2` / `j2_` |
| provider | `rebar3 mustache` | `rebar3 jinja` |
| 配置键 | `mustache_opts` | `jinja_opts` |
| 一致性基线 | 官方 spec 的 136 个用例 | 由 CPython jinja2 3.1 生成的 520 条 fixture |
| 文档 | 本 README | [docs/jinja.zh-CN.md](docs/jinja.zh-CN.md) |

二者是并列关系，不是二选一。同一个项目可以两个都用，共用一个 `views` 目录和一个 `out_dir`：
每个生成文件都写明自己出自哪个引擎，每个 provider 也只回收自己的孤儿文件。

**这两门语言是不一样的，差在哪里值得先知道：**

| | mustache | jinja |
|---|---|---|
| 变量查找 | 动态回溯 context stack | 词法作用域 |
| `0` 与 `#{}` | 真 | **假** |
| 关掉转义 | `{{{x}}}` | `\|safe` |
| 复用 | `{{> p}}` | include / extends / macro / import |
| `true` 输出为 | `true` | `True` |
| 列表输出为 | 它的字符 | `[1, 2]` |

```erlang
{provider_hooks, [{pre, [{compile, mustache}, {compile, jinja}]}]}.
{mustache_opts, [{views, "views"}, {prefix, "view_"}]}.
{jinja_opts,    [{views, "views"}, {suffix, ".j2"}, {prefix, "j2_"}]}.
```

---

## 0.5.0 新增

Jinja2 引擎、`rebar3 jinja`、`ai_jinja_transform`，以及两个引擎共用的底层。
mustache 的行为没有变化。完整说明见 [CHANGELOG.md](CHANGELOG.md)，
引擎本身的文档见 [docs/jinja.zh-CN.md](docs/jinja.zh-CN.md)。

---

## 0.4.0 的不兼容变更

0.4.0 是一次彻底重写。模板和调用方代码都需要改。

### 标准 context stack 语义

此前用的是扁平 context 加全路径查找。现在改为标准 Mustache 的 context stack —— 其它语言的 Mustache 实现都是这么做的。

```diff
- {{#user}}{{user.name}}{{/user}}
+ {{#user}}{{name}}{{/user}}

- {{#items}}{{+ items.current}}<li>{{items.name}}</li>{{/ items.current}}{{/items}}
+ {{#items}}{{+ current}}<li>{{name}}</li>{{/ current}}{{/items}}
```

规则：在 `{{#X}}` 内部，引用 `X` 自己的字段时去掉 `X.` 前缀。`rebar3 mustache migrate` 会做机械改写，无法确定的部分原样保留并列进报告。

**没有兼容开关，这是刻意的。** 同时支持两套语义会让编译器的作用域解析分叉，长期维护成本远高于一次性迁移。

### API

| 移除 | 替代 |
|---|---|
| `ai_mustache:bootstrap/0,1` | 无 —— 模板由构建期编译 |
| `ai_mustache:reload/0` | `ai_mustache_dev:reload/1`（仅开发期） |
| `ai_mustache:render(Name, Ctx)`，`Name` 是字符串 | `view_name:render(Ctx)`，或 `ai_mustache:render(view_name, Ctx)` |
| `application:start(aihtml)` | 无 —— aihtml 是 library application |
| `ai_dom_node`、`ai_dom_render` | 删除；与 Mustache 无关 |

### HTML 转义从 8 个字符收窄到 5 个

转义 `&` `<` `>` `"` `'`，**不再**转义 `/`、`=`、反引号。

旧的字符集会破坏 URL：`href="/a/b"` 会变成 `href="&#x2F;a&#x2F;b"`。旧实现的 `&` 替换还漏了分号，产出 `&amp` 而非 `&amp;`，这个也修了。

### partial 缩进作用于每一行

`{{> partial}}` 独占一行且有缩进时，该缩进会应用到 partial 输出的每一行，符合 spec 要求。旧实现只缩进第一行。

### 不再依赖 ailib

用到的部分全部在 `ai_mustache_rt` 中重新实现，只依赖 OTP。

### 其它修复

- `{{.}}` 隐式迭代器可用了，此前永远取不到值。
- `{{> a.b}}` 这类带点的 partial 路径不再让 parser 崩溃。
- `{{#a.b}}` 迭代时不再把 `a` 下的其它键丢掉。

---

## 工作原理

```
views/index.mustache
   |
   +- ai_mustache_scanner     词法：文本与 tag token、standalone 行、分隔符
   +- ai_mustache_parser      递归下降 -> AST
   +- ai_mustache_ast         合并文本、消除空 body、解析 partial
   |
   +- ai_mustache_compiler    AST -> Erlang abstract forms      （唯一的实现）
         |
         +- rebar3_aihtml     erl_prettypr -> _gen/view_index.erl -> .beam
         +- parse_transform   forms 注入调用方模块
```

`views/index.mustache` 大致会变成：

```erlang
-module(view_index).

-mustache_source(#{path => <<"views/index.mustache">>, stamp => <<...>>,
                   mtime => 1757203845, vsn => 1, opts => #{...}}).

-export([render/1, render_iolist/1, render_stack/1, render_stack/2, partials/0]).

render(Ctx) -> erlang:iolist_to_binary(render_stack([Ctx], <<>>)).
partials()  -> [view_shared_item].

render_stack(S, I) ->
    [I, <<"<h1>">>,
     ai_mustache_rt:escape(ai_mustache_rt:lookup([header], S)),
     <<"</h1>\n">>,
     sec_1(S, I)].

sec_1(S, I) ->
    case ai_mustache_rt:lookup([items], S) of
        []                       -> [];
        L when is_list(L)        -> [sec_1_body([E | S], I) || E <- L];
        M when is_map(M)         -> sec_1_body([M | S], I);
        true                     -> sec_1_body(S, I);
        F when is_function(F, 2) -> F(erlang:iolist_to_binary(sec_1_body(S, I)), hd(S));
        F when is_function(F, 1) -> ai_mustache_rt:section(F(hd(S)), fun sec_1_body/2, S, I);
        V                        -> ai_mustache_rt:section(V, fun sec_1_body/2, S, I)
    end.

sec_1_body(S, I) -> [<<"  ">>, view_shared_item:render_stack(S, I)].
```

三个值得点明的后果：

**静态文本是字面量。** 它进入模块的 literal pool，跨进程按引用共享。旧设计把解析后的模板放在 ETS 里，每次 `ets:lookup/2` 都要把整棵树深拷贝进调用进程。

**partial 是跨模块调用。** 因此改 partial 不需要重编引用它的模板，两个模板还可以互相引用 —— 旧的树遍历解释器会无限递归。

**section 迭代是往列表上压栈。** `[Item | Stack]` 只分配一个 cons cell。旧 runner 每次迭代都用 `maps:merge/2` 重建 context，那是整个 map 的拷贝；section 名带点时还会悄悄丢掉兄弟键。

---

## 安装

需要**两个**条目：plugin 在构建期编译模板，`aihtml` 依赖提供生成模块运行期调用的 `ai_mustache_rt`。

```erlang
%% rebar.config —— 单 app 项目的全部内容
{erl_opts, [debug_info, {src_dirs, ["src", "_gen"]}]}.

{deps, [{aihtml, {git, "https://github.com/DavidAlphaFox/aihtml.git",
                  {tag, "v0.4.0"}}}]}.

{plugins, [{rebar3_aihtml, {git_subdir, "https://github.com/DavidAlphaFox/aihtml.git",
                            {tag, "v0.4.0"}, "rebar3_aihtml"}}]}.

{provider_hooks, [{pre, [{compile, mustache}]}]}.
```

真的就这些 —— 上面没有 `mustache_opts`，因为每个选项都有可用的默认值。plugin 在 aihtml 仓库的子目录里，所以是 `git_subdir` 而不是 `git`。

把 `_gen/` 加进 `.gitignore`，那是构建产物。

### 项目结构

```
myapp/
  rebar.config
  src/
    myapp.app.src
    myapp.erl              %% 调用 view_index:render/1
  views/                   %% <- {views, "views"}，默认值
    index.mustache         %% -> view_index
    layout/
      head.mustache        %% -> view_layout_head
    shared/
      post.mustache        %% -> view_shared_post
  _gen/                    %% 生成物，已 gitignore
    view_index.erl
    view_layout_head.erl
    view_shared_post.erl
```

`{src_dirs, ["src", "_gen"]}` 是最容易忘的一行：漏了它 rebar3 就不会编译生成的模块，`view_index` 会 undefined。

partial 按它在 `views/` 下的路径命名，不带后缀：

```mustache
{{> layout/head}}
{{> shared/post}}
```

### 覆盖默认值

```erlang
{mustache_opts, [
    {views,    "views"},        % 模板根目录，相对于 app
    {suffix,   ".mustache"},
    {out_dir,  "_gen"},         % 生成的 .erl 放哪
    {prefix,   "view_"},        % 模块名前缀
    {line_map, true},           % 生成代码的行号映射回模板
    {extensions, []},           % 自定义 tag 模块，见下方 parse_transform
    {ext_opts, #{}},            % 传给这些模块的配置
    {warnings_as_errors, false}
]}.
```

无法识别的键会被报出来而不是忽略，所以打错字不会悄无声息地不生效。

### 为什么生成 `.erl` 而不是直接出 `.beam`

生成的源码可读、可 grep、dialyzer 看得见，模板出错时堆栈里的行号是真实的。

### 模块命名

```
views/index.mustache          ->  view_index
views/shared/item.mustache    ->  view_shared_item
views/layout/default.mustache ->  view_layout_default
```

`/`、`-`、`.` 都归一为 `_`，所以 `shared/item.mustache` 和 `shared_item.mustache` 会撞名。plugin 会检测到并报错，而不是静默覆盖其中一个。

### 迁移 0.3.x 的模板

```sh
rebar3 mustache migrate           # 只打印 diff
rebar3 mustache migrate --write   # 落盘
```

原地改写，保留注释、空白和自定义分隔符。无法确定的部分 —— 比如引用了兄弟 section 的变量 —— 原样保留并列进报告，交由人工处理。

### Umbrella 项目

plugin、依赖和 hook 放在项目根，每个 app 保有自己的 `views/` 并生成到自己的 `_gen/`。

```
myproject/
  rebar.config             %% plugins、deps、provider_hooks、共享的 mustache_opts
  apps/
    web/
      rebar.config         %% {erl_opts, [{src_dirs, ["src", "_gen"]}]} 及各自的覆盖
      views/ ...
      _gen/ ...
    admin/
      rebar.config
      views/ ...
      _gen/ ...
```

两点要注意：

- **`{src_dirs, ["src", "_gen"]}` 必须写在每个 app 自己的 `rebar.config` 里**，只放根目录不够。
- **同路径模板的 app 要各给一个 `prefix`。** `web/views/index.mustache` 和 `admin/views/index.mustache` 都映射到 `view_index`，一个模块名对应两个模块。plugin 会跨 app 检测到冲突并告诉你涉及哪些 prefix；在其中一个里写 `{prefix, "admin_"}` 即可解决，旧名字下生成的模块会在下次构建时被当作孤儿清理掉。

### erlang.mk

```makefile
BUILD_DEPS = rebar3_aihtml
DEP_PLUGINS = rebar3_aihtml
dep_rebar3_aihtml = git https://github.com/DavidAlphaFox/aihtml.git v0.4.0
```

支持，但不是主路径；CI 以 rebar3 构建。

---

## 渲染

```erlang
view_index:render(Ctx)         -> binary().
view_index:render_iolist(Ctx)  -> iolist().

%% 模板名运行期才确定时 —— 比如按路由挑布局：
ai_mustache:render(view_index, Ctx)         -> binary().
ai_mustache:render_iolist(view_index, Ctx)  -> iolist().

%% 内联模板（见下方 parse_transform）：
ai_mustache:inline(~"Hello {{name}}!", #{name => Name}) -> binary().
```

`render_iolist/1` 可以直接作为 cowboy 的响应 body，省掉构造扁平 binary 的那一步。

context 可以是任意 term，不限于 map：spec 里就有一个用例的全部数据是整数 `85`，通过 `{{.}}` 取到。

### 开发期热重载

```erlang
ai_mustache_dev:check()          % 一次性环境自检
ai_mustache_dev:stale(view_index)
ai_mustache_dev:reload(view_index)
ai_mustache_dev:reload(all)
```

**`check/0` 不是开关。** 这个模块不持有任何状态 —— 没有 ETS、没有 persistent_term、没有进程、没有缓存文件 —— 所以"是否启用"这个标志无处安放。它不监视你的文件。触发由你自己发起：开发期的 middleware、编辑器保存钩子，或请求处理函数的开头调一次 `reload/1`。

是否过期由文件内容的 hash 决定，绝不看 mtime：POSIX mtime 只有秒级精度，而"改完立刻刷新"这个循环完全发生在一秒之内。

---

## 模板语义

context 和模板里的 key 都是 **atom**：

```erlang
#{user => #{name => <<"David Gao">>, level => 1}, stars => 10}
```

模板正文与路径是 **UTF-8 binary**。入口会接受任意 `unicode:chardata()` 并归一化一次，但不是合法 UTF-8 的模板会以 `{invalid_utf8, ByteOffset}` 被拒绝，而不是按字节透传 —— 在门口失败比后面从生成模块里吐出乱码好查得多。路径只归一化不拒绝，因为非 UTF-8 文件名的系统上，那样的路径仍然指向真实文件。

### 在你自己的 Erlang 代码里写非 ASCII

模板文件按 UTF-8 读取，不需要任何处理。但你自己源码里的字面量需要：

```erlang
%% 错：每个码点被截成 8 位，文字被毁掉
#{title => <<"我的博客">>}

%% 对
#{title => <<"我的博客"/utf8>>}
```

这是 Erlang 本身的行为，不是 aihtml 引入的，但它是"模板明明没问题、页面却是乱码"最常见的成因。

### 名字解析

`{{name}}` 从栈顶向外逐层查找，取第一个含该 key 的 frame 中的值。

`{{a.b}}` 先在栈上解析 `a`，然后**严格在 `a` 内部**取 `b`。如果 `a` 里没有 `b`，结果为空，不会继续向外找。

`{{.}}` 是隐式迭代器：栈顶的值。

### falsy 的定义

恰好五个：`undefined`、`false`、`[]`、`<<>>`、`null`。

### 0 是 truthy，`#{}` 也是

这一点很容易踩，所以专门说明：

```erlang
%% {{#count}}You have {{.}} messages{{/count}}
#{count => 0}   %% section 会执行，渲染出 "You have 0 messages"
#{count => []}  %% section 被跳过
```

如果你要的是"零就隐藏"，在代码里判断后传一个布尔值。

### section 分派

`{{#x}}` 的行为取决于 `x` 的运行期类型：

| `x` | 行为 | 是否压栈 |
|---|---|---|
| `[]` | 跳过 | -- |
| 非空 list | 每个元素执行一次 body | 是，逐元素 |
| map | 执行一次 body | 是 |
| `true` | 执行一次 body | 否 |
| `fun/2` | 调用 `F(渲染后的Body, 当前Frame)` | 否 |
| `fun/1` | 调用 `F(当前Frame)`，返回值再次分派 | 取决于返回值 |
| falsy | 跳过 | -- |
| 其它 | 执行一次 body | 是，因此 `{{.}}` 可用 |

`{{^x}}` 在 `x` 为 falsy 时执行 body，且永不压栈。

---

## `{{#}}` 与 `{{+}}` 的区别

两者看起来都是条件判断，差别在于作用域。

```mustache
{{#user}}{{name}}{{/user}}        渲染用户名 —— {{#}} 把 user 压栈
{{+user}}{{user.name}}{{/user}}   渲染同样的东西 —— {{+}} 不压栈，所以要写全路径
```

`{{#}}` 的语义是 *with* / *for each*：进入作用域并迭代 list。
`{{+}}` 的语义是 *if*：判断真假，在**外层作用域**里执行一次 body。`{{-}}` 是 `{{+}}` 取反。

当你只想要一个条件、不想改变里面名字的含义时，用 `{{+}}`：

```mustache
{{#items}}
  {{+ current}}<li class="on">{{name}}</li>{{/ current}}
  {{- current}}<li>{{name}}</li>{{/ current}}
{{/items}}
```

两个分支里的 `{{name}}` 指的都是当前 item。换成 `{{#current}}` 就会指向 `current` 内部的东西了。

两者都接受 `fun/1`，参数是当前 frame：

```erlang
#{has_friends => fun(Frame) -> maps:get(friends, Frame, []) =/= [] end}
```

---

## 条件渲染 partial

`{{> x}}` 本身是无条件的，用 section 包起来：

```mustache
{{#user}}
  {{> shared/card}}
{{/user}}

{{+is_admin}}
  {{> shared/panel}}
{{/is_admin}}

{{^items}}
  {{> shared/empty}}
{{/items}}
```

选哪种 section 决定了 partial 内部的名字怎么解析 —— `{{#}}` 压栈，`{{+}}` 不压栈。

**让 partial 独占一行。** 挤在一行里它就不是 standalone，会丢掉缩进：

```mustache
{{#show}}
  {{> row}}          正确：row 的每一行都获得 2 空格缩进
{{/show}}

{{#show}}{{> row}}{{/show}}   缩进丢失，且末尾多一个空行
```

运行期决定**用哪个** partial（spec 的 Dynamic Names 未实现）：互斥 section，或者用 lambda 直接调模块 —— partial 编译成真模块，可以从 Erlang 里调用：

```erlang
Pick = fun(Frame) ->
    Mod = case maps:get(kind, Frame) of
              text  -> view_shared_text;
              image -> view_shared_image
          end,
    Mod:render_iolist(Frame)     %% 返回 iolist，不会被转义
end.
```

模板里写 `{{*body}}`。

---

## Lambda

`{{*name}}` 是 aihtml 的扩展。它的输出**不转义** —— 产出标记正是它的用途。

```erlang
%% fun/1：收到当前 frame
#{yield => fun(Frame) -> render_something(Frame) end}

%% fun/2 加一个值：调用 Fun(Value, Frame)
#{yield => [fun render_layout/2, <<"index">>]}
```

0.4.0 起这个 fun 收到的是**栈顶**，不再是扁平的全局 context。写在模板顶层的 lambda 仍然看得到根 context，但挪进 section 就看不到了。需要什么就通过 `fun/2` 显式传进去，不要依赖 tag 所在的位置。

---

## Partial

```mustache
{{> shared/user}}
```

解析为 `view_shared_user`，编译成一次直接调用。partial 用**当前栈**渲染，所以放在 `{{#items}}` 里的 partial 看得到当前 item：

```mustache
{{! views/index.mustache }}
<ul>{{#items}}{{> shared/row}}{{/items}}</ul>

{{! views/shared/row.mustache }}
<li>{{name}}</li>
```

独占一行且有缩进的 partial，该缩进会应用到它输出的每一行。插值出来的内容不会被再次缩进，所以含换行的值保持自己的形状。

---

## parse_transform

完整指南：**[docs/parse-transform.zh-CN.md](docs/parse-transform.zh-CN.md)**。

```erlang
-module(my_views).
-compile({parse_transform, ai_mustache_transform}).

%% (a) 声明自定义 tag
-mustache_tag(my_i18n).

%% (b) 内联模板，编译期展开
greet(Name) -> ai_mustache:inline(~"Hello {{name}}!", #{name => Name}).

%% (c) 把模板文件编译成 index/1 与 index_iolist/1
-mustache_template({index, "views/index.mustache"}).
```

**没有 `~mustache` 这种 sigil。** Erlang 的 sigil 是封闭集合，自定义的过不了词法。所以内联模板靠"识别第一个参数是 binary 字面量的 `ai_mustache:inline/2` 调用"实现。`~"..."` 是 OTP 27 的标准字符串 sigil，产出的正是这样的字面量。

不加 transform，或第一个参数不是字面量时，调用照常工作 —— 降级为运行期编译，输出相同，只是更慢。唯一的例外是自定义 tag：它由编译期回调展开，降级路径上无法执行。

---

## 与 spec 的差异

必选模块 —— Comments、Delimiters、Interpolation、Inverted、Partials、Sections —— 全部通过。以下是刻意的偏离：

| | |
|---|---|
| `{{+x}}` / `{{-x}}` | aihtml 扩展；spec 没有这两个 tag |
| `{{*x}}` | aihtml 扩展 |
| key 类型 | atom，spec 用字符串 |
| Lambda | 返回值不会被再次当作模板解析 |
| Dynamic Names、Blocks | 未实现（spec 的可选模块） |
| `'` 的转义 | 转成 `&#39;`，spec 并未要求 |

---

## 性能

`bench/run.sh` 会和 v0.3.7 在一个两边渲染出**逐字节相同**页面的场景下对比 —— 一旦输出不一致，脚本会拒绝给出计时结果。OTP 28 / Linux x86-64，7 次运行取中位数：

| 页面 | v0.3.7 | 0.4.0 `render/1` | 0.4.0 `render_iolist/1` |
|---|---|---|---|
| 20 项，1843 字节 | 632.5 us | 11.8 us（**53 倍**） | 10.0 us |
| 100 项，8885 字节 | 3235.0 us | 51.9 us（**62 倍**） | 46.0 us |

差距随 section 迭代次数增大：旧 runner 每个元素都用 `maps:merge/2` 重建 context，拷贝的是一整个不会变小的 map；压栈只是一个 cons cell。

方法论和差距的其余来源见 [bench/README.md](bench/README.md)。

---

## 一个完整的例子

`examples/` 是一个完整项目：模板在 `examples/views/`，驱动代码在 `examples/src/complex.erl`，plugin 在 `examples/rebar.config` 里接好。

```sh
sh examples/run.sh
```

它在临时副本里构建，因为 aihtml 和 rebar3_aihtml 必须通过 `_checkouts` 接进来（rebar3 没有 `path` resource），而把仓库根链进仓库内的目录会形成循环。脚本会打印生成的模块列表和渲染出的页面。

这个例子刻意做得很密集：涵盖 partial 缩进、`{{+}}` 与 `{{-}}`、不压栈的 `true` section、inverted section、truthy 的 `0`、`{{.}}`，以及 `fun/2` lambda。`test/examples_tests.erl` 对它的输出做了逐字节固定。

---

## 使用 aihtml 的项目

- [aiwiki](https://github.com/DavidAlphaFox/aiwiki) —— 一个非常简单的博客。它的模板早于 0.4.0，需要迁移。

---

## 文档

- [使用 parse_transform](docs/parse-transform.zh-CN.md) —— 扩展 tag、内联模板、文件模板
- [基准测试](bench/README.md) —— 方法论与结果
- [设计文档](designs/README.md) —— 引擎为什么这样设计

## 致谢

scanner 切分 tag 的逻辑源自 Hinagiku Soranoba 的 [bbmustache](https://github.com/soranoba/bbmustache)，遵循 MIT 许可使用。

## 许可

MIT，见 [LICENSE](LICENSE)。
