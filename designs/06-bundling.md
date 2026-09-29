# 06 打包与 Stimulus

浏览器端代码改为 **Stimulus + ES 模块，用 Vite 打包**，只派发打包产物；jQuery 在迁移期保留，逐个组件去掉，最后从库里移除（方案 B）。本文定下目标结构、兼容约束和迁移步骤。

## 目标

- **静态 HTML 为本**：服务端渲染全部内容，JS 只做增强。这一点不变，也是 SEO 的基础。
- **只派发打包产物**：源码在 `apps/aihtml/assets/js/`，打包结果在 `apps/aihtml/priv/static/js/`（压缩、带内容哈希、附 `manifest.json`）。release 只带 `priv`，所以派发的就是打包后的代码；产物提交进仓库，纯 Erlang 的使用方不需要 npm。
- **按需加载**：每个页面只加载入口（运行时核心 + Stimulus），组件代码在页面上出现对应组件时才加载。
- **最终去掉 jQuery**：控制器用原生 DOM API。使用方自己的页面脚本需要 jQuery 时，仍可由 `aihtml_page` 的 `jquery` 选项单独引入。

## 结构

```
apps/aihtml/assets/js/
  main.js                 入口：核心、模板、组件注册表，启动 Stimulus
  core.js                 运行时：action、推送、替换与形变、浮层定位、主题、适配层
  components/<name>.js    一个组件的行为（ES 模块），共用代码 import ./_lib_<topic>.js
vite.config.mjs           构建配置，含两个插件：
                          - virtual:ah-templates  把 templates/*.mustache 编译成模块
                          - virtual:ah-registry   行为名、页面函数、触发选择器 → 代码块
apps/aihtml/priv/static/js/
  main-<hash>.js, <chunk>-<hash>.js, .vite/manifest.json
```

`aihtml_page` 读取 manifest，写出 `<script type="module" src=".../main-<hash>.js">` 和入口依赖的 `<link rel="modulepreload">`。代码块之间用相对路径引用，静态资源挂在 `/aihtml/` 或别的路径下都能工作。

## Stimulus

- **属性**：Stimulus 的控制器属性配置成 `data-ah`，所以服务端输出的 HTML 不变：`data-ah="datagrid"` 就是 datagrid 控制器。
- **生命周期**：Stimulus 用 MutationObserver 在元素进入页面时连接控制器、离开时断开，替换内容后不再需要手动挂载。
- **元素移动不重置**：元素被移动（形变替换、`preserve()`）时，Stimulus 会先断开再连接。控制器的销毁推迟到微任务里执行，元素仍在页面上就不销毁，所以状态得以保留。
- **同步可用**：服务端可能在同一个响应里先插入组件、再调用它的方法（`call` 操作），测试也要在挂载后立即断言。所以 `AH.mount(root)` 会立即初始化其中已加载的组件，Stimulus 稍后连接时发现已经初始化就跳过；对还没加载的组件调用方法，会排队到加载并连接之后再执行。
- **组件之间的事件**：统一用原生 `dispatchEvent`。jQuery 的 `.trigger` 不产生原生事件，Stimulus 的 `data-action` 收不到；去掉 jQuery 的过程中逐个组件改掉。

## 按需加载

构建时扫描 `components/*.js`，生成对照表：

| 触发条件 | 例子 | 来源 |
|---|---|---|
| 页面上出现 `data-ah="<name>"` | `data-ah="datagrid"` | 文件里的 `AH.define("<name>", ...)` |
| 服务端调用页面函数 | `call(Ctx, global, toast, ...)` | 文件里的 `AH.fn("<name>", ...)` |
| 页面上出现某个属性 | `[data-ah-tooltip]`、`[data-ah-open]`、`[data-ah-validate]` | 文件头的注释 `// ah-load: <选择器>` |

运行时在启动时、以及每次 DOM 变化后检查这些条件，加载缺的代码块，再注册控制器。共用代码 `_lib_*.js` 由组件 `import`，打包工具会自动拆出共享代码块。第三方库（echarts、xlsx、jspdf、ProseMirror）先保留 `AH.vendor`，去 jQuery 的阶段再改成组件内部的动态 `import()`。

## 迁移步骤

1. **构建与适配（第一阶段，已完成）**：
   - 引入 Vite 和 Stimulus，搭好两个插件、manifest，以及 `aihtml_page` 对接。
   - 组件文件机械地改成 ES 模块：`import $ from "jquery"`、`import AH from "../core.js"`，共用代码用 import 引入。
   - `AH.define` 变成适配层，把旧的 `{init, destroy, methods}` 包成 Stimulus 控制器。
   - 浏览器测试和预览改为通过本地 HTTP 服务加载打包产物（ES 模块不能从 `file://` 加载）。
   - **验收**：服务端 HTML 不变（455 个演示比对），216 个浏览器测试照常通过。
   - **结果**：入口 155 KB（gzip 后 49 KB，其中 jQuery 约一半），原来是每页 1.4 MB（gzip 后 268 KB）；每个组件一个代码块，共享代码和模板随用到它的代码块加载。
   - **实现要点**：
     - Stimulus 的动作属性配置成 `data-ah-do`，因为 `data-action` 已经被组件内部使用。
     - 通过辅助函数注册、名字是算出来的行为，用 `// ah-define: <name>` 声明，`aihtml_tests` 会检查每个行为都能找到。
     - 共享模板每个一个虚拟模块 `virtual:ah-tpl/<name>`，由组件自己 import。
2. **去 jQuery（第二阶段，按组件并行）**：
   - 每个组件改写成原生 Stimulus 控制器：`connect`/`disconnect`、原生 DOM、原生事件、`AbortController` 统一解绑；共用代码同样改写。
   - 对应的浏览器测试改用原生事件。
   - 第三方库改为动态 `import()`。
3. **移除 jQuery**：最后一处用完后，从入口里去掉 jQuery 和 `window.jQuery`，`aihtml_page` 默认不再引入。
4. **SEO 补强**：服务端渲染 datagrid 远程模式的首页、图表的可读数据、导航类交互的真实链接、页面元信息选项。
