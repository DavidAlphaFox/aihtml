# 01 架构

## 目标

用 Erlang 函数调用直接写 HTML 页面。每个预制件是一个 Erlang 函数，它同时决定三件事：

- 输出的 HTML 结构
- 对应的 CSS 语义 class
- 对应的浏览器端行为（Stimulus 控制器）

## 分层

```
aihtml            门面：标签函数、预制件、render、page、fetch
aihtml_prefab     预制件实现
aihtml_catalog    预制件元数据：修饰符组、标志、选项、行为、事件
aihtml_theme      四轴定义与校验
aihtml_page       完整文档
aihtml_action     action：令牌签名与校验、执行、DOM 操作（响应格式由传输层决定）
aihtml_push       推送：主题令牌、pg 分发
aihtml_html       元素树、class 与属性归一化、渲染
aihtml_element    元素 record：行为、构建、渲染分发
beamai_render     beamai_html_escape：转义与值格式化
```

构建函数只做便宜的检查（标签名是否合法、空元素有没有子节点），错误出现在写错的调用点；class 与属性的归一化和校验在渲染时进行，所以元素在渲染前一直是普通数据。

## 元素树

所有元素都是 record，都以同一组公共字段（module、id、css、attrs、postback、delegate）开头，详见 [05-records.md](05-records.md)：

- **普通标签**生成公共 record `#ah_el{tag, body}`，由 `aihtml_html` 渲染；`body` 为 `void` 表示空元素。
- **组件**生成带 `ah_` 前缀的 record（如 `#ah_button{}`），渲染时交给组件模块的 `render/1`。

选择元素树而不是直接输出 iodata，有三个原因：

- 预制件可以合并使用者传入的属性，并按规则覆盖。
- 测试可以断言结构，预制件内部也可以安全地嵌套。
- 元素在渲染前可以模式匹配、修改字段、组合。

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

- **action 模式**：每个事件一次 POST，响应是要应用的 DOM 操作（JSON；有渐进更新时是 NDJSON），详见 [02-actions.md](02-actions.md)。
- **服务端推送**：页面订阅签名主题，服务端通过 SSE 推送，详见 [03-push.md](03-push.md)。
- **fetch 模式**：请求开发者自己路由的 URL，通过 `data-ah-fetch`、`data-ah-url`、`data-ah-target`、`data-ah-swap`、`data-ah-trigger`、`data-ah-confirm` 驱动。

预制件的客户端行为是 Stimulus 控制器，根元素写 `data-ah="<name>"`，按需加载，见 [06-bundling.md](06-bundling.md)。

## 构建

- beamai_render 目前没有 git tag，依赖按提交哈希固定。
- Tailwind CLI v4 负责构建。`priv/static/aihtml.css` 是类库的预构建产物，示例有自己的 `example.css`。
- 浏览器端代码由 Vite 打包到 `priv/static/js`（入口 + 每个组件一个代码块 + manifest），第三方库 echarts、xlsx、jspdf 是其中按需 `import()` 的代码块，ProseMirror 由 markdown_editor 直接 import、在它的代码块里；`priv/static/vendor` 只放给页面脚本用的 jQuery。产物提交进仓库，纯 Erlang 使用方无需运行 npm。详见 [06-bundling.md](06-bundling.md)。
