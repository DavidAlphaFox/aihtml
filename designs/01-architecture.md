# 01 架构

## 目标

用 Erlang 函数调用直接写 HTML 页面。每个预制件是一个 Erlang 函数，它同时决定三件事：

- 输出的 HTML 结构
- 对应的 CSS 语义 class
- 对应的 jQuery 行为

## 分层

```
aihtml            门面：标签函数、预制件、render、page、fetch
aihtml_prefab     预制件实现
aihtml_catalog    预制件元数据：修饰符组、标志、选项、行为、事件
aihtml_theme      四轴定义与校验
aihtml_page       完整文档
aihtml_action     action：令牌签名与校验、执行、AG-UI 事件流、DOM 操作
aihtml_push       推送：主题令牌、pg 分发
aihtml_html       元素树、class 与属性归一化、渲染
beamai_render     beamai_html_escape：转义与值格式化
```

`aihtml_html` 在构建元素时就完成归一化与校验，错误出现在写错的调用点。渲染阶段只做拼接。

## 元素树

元素是不透明记录 `#el{tag, attrs, children}`，属性已归一化为 `[{binary(), binary() | true}]`。选择元素树而不是直接输出 iodata，有两个原因：

- 预制件可以合并使用者传入的属性，并按规则覆盖。
- 测试可以断言结构，预制件内部也可以安全地嵌套。

`{safe, iodata()}` 与 beamai_jinja 的安全标记同形，渲染结果可以直接放进 Jinja 模板。

## Css 参数的两层

sigil 的原则是标记里只写语义 class，主题切换时不改 HTML。本项目保留这条原则，同时允许用 Tailwind 写布局：

- **原子**：预制件的语义修饰符，经 `aihtml_catalog` 校验后写成 `<root>-<modifier>`。
- **binary**：字面 class，排在语义 class 之后。

预制件样式位于 `@layer components`，Tailwind 工具类位于更靠后的 utilities 层，因此工具类总能覆盖预制件样式。变体只在 `:where()` 中设置组件令牌，不增加选择器权重。

## 四轴

与 sigil 相同，四个轴互相独立：

| 轴 | 属性 | 可写的令牌 |
|---|---|---|
| appearance | data-theme | 中性色与状态色 |
| palette | data-palette | `--ah-color-primary*` |
| typography | data-typography | `--ah-font-*` |
| skin | data-skin | 圆角、边框宽度、阴影；颜色只能通过 `var()` 读取 |

派生令牌，例如 `--ah-color-primary-soft`，定义在 `:root` 上，因此会随所有轴一起重新计算。`aihtml_theme:axes/0` 是取值的唯一来源，测试会检查每个取值在 CSS 中都有对应规则。

## 交互模型

两种模式都是无状态的，可以在同一页面混用：

- **action 模式**：参照 AG-UI，每个事件一次 POST，响应为事件流，详见 [02-actions.md](02-actions.md)。
- **服务端推送**：页面订阅签名主题，服务端通过 SSE 推送，详见 [03-push.md](03-push.md)。
- **fetch 模式**：请求开发者自己路由的 URL，通过 `data-ah-fetch`、`data-ah-url`、`data-ah-target`、`data-ah-swap`、`data-ah-trigger`、`data-ah-confirm` 驱动。

预制件的客户端行为通过 `data-ah="<name>"` 和 `AH.define` 挂载，事件都在 `.ah` 命名空间下。

## 构建

- beamai_render 目前没有 git tag，依赖按提交哈希固定。
- Tailwind CLI v4 负责构建。`priv/static/aihtml.css` 是类库的预构建产物，示例有自己的 `example.css`。
- jQuery 由 `scripts/vendor.mjs` 从 node_modules 复制到 `priv/static/vendor`，纯 Erlang 使用方无需运行 npm。
