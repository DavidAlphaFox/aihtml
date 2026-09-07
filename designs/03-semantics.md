# 03 · 模板语义定稿

> **决策 D2：改为标准 mustache 的 context stack 语义。** 这是本次重构破坏性最大的一项，现有模板需要迁移。

## 1. 为什么改

当前是**扁平 context + 全路径寻址**：section 内部要写 `{{ items.name }}` 而非 `{{ name }}`。

改为标准语义的四点理由：

1. **可移植** —— 模板能在 mustache 的其它语言实现间复用，这本就是 mustache 的设计目标。
2. **能接官方 spec 测试集** —— 现有语义无法通过 [mustache/spec](https://github.com/mustache/spec)，改标准后可以拿它当回归基线。
3. **编译期能生成更好的代码** —— 压栈迭代不需要构造新 context map，直接传 `[Item | Stack]`。
4. **顺手修掉 B1** —— `maps:merge` 丢失兄弟键的 bug 在压栈方案下不存在。

## 2. 语义规则表

| 写法 | 语义 |
|---|---|
| `{{name}}` | 从栈顶向外逐层查找，返回第一个含 `name` 的 frame 中的值；HTML 转义后输出 |
| `{{{name}}}` / `{{&name}}` | 同上，但不转义 |
| `{{a.b}}` | 在栈上解析 `a`（可回溯），然后**只**在 `a` 内部取 `b`（不回溯） |
| `{{.}}` | 栈顶本身（隐式迭代器）。修掉 [B5](01-current-state.md#b5-·--隐式迭代取不到值) |
| `{{#x}}...{{/x}}` | section，见 §3 |
| `{{^x}}...{{/x}}` | inverted section，falsy 时执行，**不压栈** |
| `{{+x}}...{{/x}}` | has，纯布尔判定，**不压栈不迭代**（aihtml 扩展） |
| `{{-x}}...{{/x}}` | inverted has，同上取反（aihtml 扩展） |
| `{{*f}}` | lambda（aihtml 扩展），见 §5 |
| `{{> path}}` | partial，以**当前栈**渲染；保持 standalone 缩进 |
| `{{! ... }}` | 注释 |
| `{{=<% %>=}}` | 自定义分隔符 |

### 2.1 falsy 的定义

统一为：`undefined` · `false` · `[]` · `<<>>` · `null`。

其余一切（包括 `0` 与 `#{}`）为 truthy。这与标准 mustache 一致，`0` 为真是常见踩坑点，需在 README 显式说明。

## 3. Section 的分派规则

`{{#x}}` 按 `x` 的运行期类型分派：

| `x` 的值 | 行为 |
|---|---|
| `[]` | 不执行 |
| 非空 list | 每个元素**压栈**迭代执行 body |
| map | **压栈**执行一次 body |
| `true` | 执行一次 body，**不压栈** |
| `fun/2` | 先渲染 body 得 binary，调 `F(RenderedBody, CurrentFrame)`，用返回值 |
| `fun/1` | 调 `F(CurrentFrame)`，返回值按上表递归分派（支持惰性求值） |
| falsy | 不执行 |
| 其它（binary / number / atom） | 执行一次 body，**压栈**（使 `{{.}}` 可用） |

`{{^x}}` 只在 `x` falsy 时执行 body，永不压栈。

## 4. `{{+}}` / `{{-}}` 的存在意义

改成标准语义后，`{{#user}}` 与 `{{+user}}`（`user` 是 map 时）看起来功能重叠。**它们的差别是要不要压栈**：

```mustache
{{#user}}{{name}}{{/user}}        →  压栈，name 从 user 里取
{{+user}}{{user.name}}{{/user}}   →  不压栈，作用域不变，纯做「存在性判断」
```

`{{+}}` 的定位是**条件渲染**（if），`{{#}}` 的定位是**作用域进入 + 迭代**（with / for）。保留二者是有意义的，在 README 中要讲清楚这个区分。

`{{+x}}` 支持 `fun/1`：调 `F(CurrentFrame)`，返回 `true` 则执行 body。`{{-x}}` 取反。

## 5. Lambda `{{*f}}`

aihtml 扩展，保留现有两种形态：

```erlang
%% fun/1：收当前 frame
#{ yield => fun(Frame) -> ... end }

%% fun/2 + 值：收 [Fun, Value]，调 Fun(Value, Frame)
#{ yield => [fun render_layout/2, <<"index">>] }
```

返回值必须是 `binary()` 或 `iolist()`，**不做转义**（lambda 的用途就是产出 HTML）。

> **注意语义变更**：现在这些 fun 收到的是**栈顶 frame**（`hd(Stack)`）而非旧的扁平全局 Ctx。若 lambda 需要访问根 context，应改为使用 `fun/2` 显式传值，或在 §7 的扩展点里注册一个能拿到完整栈的自定义 tag。

## 6. Partial 的 standalone 缩进

标准 mustache 要求：当 `{{> x}}` 独占一行时，该行的缩进要**应用到 partial 输出的每一行**。

现有实现（`ai_mustache_parser.erl:180`）只是把 Indent 作为普通文本插在 partial 前面，即只缩进第一行。这不符合 spec，需要在编译期把 Indent 作为参数传给 partial 模块：

```erlang
view_shared_item:render_stack(S, <<"    ">>)
```

## 7. 迁移方案

### 7.1 需要改的写法

```diff
- {{#user}}{{user.name}}{{/user}}
+ {{#user}}{{name}}{{/user}}

- {{# items}}{{+ items.current}}<li>{{items.name}}</li>{{/ items.current}}{{/ items}}
+ {{# items}}{{+ current}}<li>{{name}}</li>{{/ current}}{{/ items}}
```

规则：**在 section `{{#X}}` 的 body 内部，把以 `X.` 开头的引用剥掉这层前缀。**

### 7.2 `rebar3 mustache migrate`

提供机械改写子命令：

- 解析模板得到 AST，对每个 section 记录其 key 路径
- 在 body 内遍历所有 `{{tag}}` / `{{#}}` / `{{+}}` 的 keys，若前缀匹配所在 section 的 key，剥掉前缀
- 输出 diff，`--write` 才真正落盘
- **不做的事**：无法确定的（比如引用了兄弟 section 的变量、跨 partial 的隐式依赖）原样保留并在报告中列出，交人工处理

### 7.3 影响范围

- 本仓 `examples/shared/{item,user,level}.mustache` + `examples/complex.mustache`
- 下游：[aiwiki](https://github.com/DavidAlphaFox/aiwiki) 及其它使用方

### 7.4 版本与公告

- 版本号跳到 **0.4.0**，README 顶部加 **Incompatible Changes** 段落（沿用现有 v0.3.5 的公告体例）
- 明确写出：v0.3.x 的扁平全路径语义已移除，无兼容开关

> **设计决定：不提供 `{compat, flat}` 兼容开关。** 两套语义并存会让 compiler 的作用域解析分叉，长期维护成本远高于一次性迁移的收益。

## 8. 与官方 spec 的差异清单

接入 mustache 官方 spec 测试集后，以下为**有意保留的差异**，需在 README 与测试中显式标注为 known deviation：

| 项 | aihtml 行为 | 原因 |
|---|---|---|
| `{{+x}}` / `{{-x}}` | 非标准的 has / inverted-has | aihtml 扩展，spec 无此语法 |
| `{{*f}}` | 非标准的 lambda | aihtml 扩展 |
| key 类型 | **atom**（spec 用 string） | 沿袭 v0.3.5 的决定，Erlang 侧 map 用 atom key 更自然 |
| lambda 返回值 | 不做二次模板解析 | spec 的 Lambdas 可选模块要求返回值再解析一遍；aihtml 编译期方案下代价过高 |
| Dynamic Names / Blocks | 不支持 | spec 可选模块，暂不实现 |

官方 spec 的 **必选模块**（Comments / Delimiters / Interpolation / Inverted / Partials / Sections）应全部通过。
