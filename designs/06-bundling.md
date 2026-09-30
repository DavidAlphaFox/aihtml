# 06 打包与 Stimulus

浏览器端代码是 **TypeScript（面向对象、strict）+ Stimulus，用 Vite 打包**，只派发打包产物；jQuery 在迁移期保留、逐个组件去掉，最后从库里移除（方案 B，已完成）。本文定下目标结构、兼容约束和迁移步骤。

## 目标

- **静态 HTML 为本**：服务端渲染全部内容，JS 只做增强。这一点不变，也是 SEO 的基础。
- **只派发打包产物**：源码在 `apps/aihtml/assets/js/`，打包结果在 `apps/aihtml/priv/static/js/`（压缩、带内容哈希、附 `manifest.json`）。release 只带 `priv`，所以派发的就是打包后的代码；产物提交进仓库，纯 Erlang 的使用方不需要 npm。
- **按需加载**：每个页面只加载入口（运行时核心 + Stimulus），组件代码在页面上出现对应组件时才加载。
- **最终去掉 jQuery**：控制器用原生 DOM API，库里不再附带 jQuery。使用方自己的页面脚本需要 jQuery 时，自行提供文件，用 `aihtml_page` 的 `js` 选项引入。

## 结构

```
apps/aihtml/assets/js/
  main.ts                 入口：设置 window.AH，交给运行时组件注册表，启动 Stimulus
  core.ts                 运行时对外的 AH 对象（AHApi 接口），把 runtime/ 的各部分接起来
  runtime/controller.ts   Controller 基类（setup/teardown、listen、delegate、fire、signal）
  runtime/behaviours.ts   Behaviours：注册、调用、就绪、按需加载
  runtime/swap.ts         Swapper：替换、preserve、settle、焦点、形变
  runtime/actions.ts      Actions：data-ah-on、请求协调、服务端操作（类型化的 Op 联合）
  runtime/push.ts         PushStream：订阅与重连刷新
  runtime/fetch.ts        Fetcher：data-ah-fetch 往返
  runtime/float.ts        FloatingPopup：浮层定位
  runtime/theme.ts        Theme：四轴
  runtime/forms.ts、requests.ts、dom.ts、vendor.ts   表单取值、请求状态、元素工具、第三方库
  components/<name>.ts    一个组件的行为：控制器类，共用代码 import ./_lib_<topic>.ts
  types/catalog.d.ts      生成：每个行为服务端可调用的方法（scripts/gen-ts-catalog.escript）
  types/env.d.ts、virtual.d.ts   window.AH，虚拟模块
tsconfig.json             类型检查（strict）；npm run typecheck
vite.config.mjs           构建配置：
                          - 组件里的 import AH from "../core.ts" 改写成 window.AH（见下）
                          - virtual:ah-tpl/<name>  把 templates/<name>.mustache 编译成模块
                          - virtual:ah-registry    行为名、页面函数、触发选择器 → 代码块
                          - 第三方库代码块命名 vendor-<库>，生成 THIRD-PARTY-LICENSES.txt
apps/aihtml/priv/static/js/
  main-<hash>.js, <chunk>-<hash>.js, vendor-<库>-<hash>.js, .vite/manifest.json,
  preload-<hash>.js, tpl_runtime-<hash>.js, THIRD-PARTY-LICENSES.txt
apps/aihtml/priv/static/css/
  aihtml-<hash>.css, manifest.json       Tailwind 构建后由 scripts/hash-css.mjs 命名
```

**文件名稳定**：代码块之间按带哈希的文件名互相引用，被引用的块一改名，引用它的块也跟着改名；入口又列出了全部代码块的文件名（注册表）。所以组件不直接 import 入口：源码写 `import AH from "../core.ts"`（类型照常检查），构建时改写成 `const AH = window.AH`（`main.ts` 在加载任何组件之前设置它，写法不对时构建报错；`import type` 不受限制）。Vite 预加载动态 import 依赖的辅助函数、模板运行时也各放一个小代码块，不留在入口里。结果：改一个组件只改名它自己和 `main`，改运行时只改名 `main`（原来两种情况都会让 110 多个文件全部改名）。

`aihtml_page` 读取 manifest，写出 `<script type="module" src=".../main-<hash>.js">` 和入口依赖的 `<link rel="modulepreload">`；样式同样由 `aihtml_assets:css/0` 从 `css/manifest.json` 读出带哈希的文件名。代码块之间用相对路径引用，静态资源挂在 `/aihtml/` 或别的路径下都能工作（`assets` 选项，脚本和样式一起跟着改）。

**缓存由使用方决定**：带哈希的文件内容永不变化，适合长期缓存，但 aihtml 自己不设置 `Cache-Control`，`aihtml_cowboy` 的静态路由是普通的 `cowboy_static`。要不要缓存、缓存多久，由使用方在自己的服务器或 CDN 上配置。

## Stimulus

- **属性**：Stimulus 的控制器属性配置成 `data-ah`，所以服务端输出的 HTML 不变：`data-ah="datagrid"` 就是 datagrid 控制器。
- **生命周期**：Stimulus 用 MutationObserver 在元素进入页面时连接控制器、离开时断开，替换内容后不再需要手动挂载。
- **元素移动不重置**：元素被移动（形变替换、`preserve()`）时，Stimulus 会先断开再连接。控制器的销毁推迟到微任务里执行，元素仍在页面上就不销毁，所以状态得以保留。
- **异步连接**：控制器由 Stimulus 异步连接。服务端可能在同一个响应里先插入组件、再调用它的方法（`call` 操作）：`AH.invoke` 对还没加载或还没连接的组件排队，连接之后再执行。页面脚本和测试插入 HTML 后 `await AH.ready(root)`。`AH.destroy(el)` 立即执行 `teardown`，之后的 `AH.mount(el)` 重新执行 `setup`（形变替换对内容变了的组件就是这样重新初始化的）。
- **组件之间的事件**：统一用原生 `CustomEvent`（控制器的 `this.fire`），数据在 `e.detail`。运行时事件（`ah:theme`、`ah:error`、`ah:before-fetch` 等）、服务端的 `trigger` 操作和 `[data-ah-on]` 的委托监听都是原生的。

## 按需加载

构建时扫描 `components/*.ts`，生成对照表：

| 触发条件 | 例子 | 来源 |
|---|---|---|
| 页面上出现 `data-ah="<name>"` | `data-ah="datagrid"` | 文件里的 `AH.register("<name>", ...)` |
| 服务端调用页面函数 | `call(Ctx, global, toast, ...)` | 文件里的 `AH.fn("<name>", ...)` |
| 页面上出现某个属性 | `[data-ah-tooltip]`、`[data-ah-open]`、`[data-ah-validate]` | 文件头的注释 `// ah-load: <选择器>` |

运行时在启动时、以及每次 DOM 变化后检查这些条件，加载缺的代码块，再注册控制器。共用代码 `_lib_*.ts` 由组件 `import`，打包工具会自动拆出共享代码块。第三方库由组件直接 import：echarts、xlsx、jspdf、jspdf-autotable 用动态 `import()`，各成一个代码块 `vendor-<库>-<hash>.js`，用到时才下载，`AH.vendor(name)` 给页面脚本取得同一份库；ProseMirror 和 markdown-it 只有 markdown_editor 用、挂载就要用，所以静态 import，打进 markdown_editor 的代码块（少一次请求，也不需要单独的入口文件和全局变量）。

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
2. **去 jQuery（第二阶段，已完成）**：
   - 每个组件改写成原生 Stimulus 控制器：`connect`/`disconnect`、原生 DOM、原生事件、`AbortController` 统一解绑；共用代码同样改写。
   - 对应的浏览器测试改用原生事件。
   - **结果**：110 个组件文件、18 个共享文件和运行时核心全部改成原生写法；浏览器测试从 216 个增加到 429 个，每个有行为的组件都有测试；服务端 HTML 不变（455 个演示比对）。
3. **移除 jQuery（已完成）**：入口里不再有 jQuery 和 `window.jQuery`，旧写法的适配层 `AH.define` 已删除，action 事件监听改为原生。入口 82 KB（gzip 后 23 KB）。`aihtml_page` 默认不引入 jQuery，页面脚本需要时用 `jquery` 选项。之后（2026-09-30）连同 npm 依赖、`scripts/vendor.mjs`、`priv/static/vendor/` 和 `jquery` 选项一起删除，页面脚本需要 jQuery 时用 `js` 选项引入自己的文件。
4. **SEO 补强与第三方库（已完成）**：
   - 页面元信息：`aihtml_page` 的 `description`、`robots`、`canonical`、`alternates`、`og`、`meta`、`json_ld` 选项。
   - markdown_view：服务端渲染 Markdown（`aihtml_lib_markdown` 移植 markdown-it，输出与浏览器端逐字节一致，由固定用例检查）。
   - 图表：画布旁输出视觉隐藏的数据表，容器角色 `figure`。
   - 导航的真实链接：pagination、datagrid、datatable、calendar、scheduler 的 `href` 选项（URL 模板）；配合 `on(change, ...)` 时页面内处理并推入 URL。
   - datagrid 远程模式首页总在服务端渲染，挂载时不再请求。
   - 第三方库从 `priv/static/vendor` 的预构建文件改为 Vite 代码块（动态 `import()`），构建时生成许可证清单。之后 ProseMirror 去掉了单独的入口 `prosemirror.entry.js`、`AH.vendor("prosemirror")` 和全局变量 `window.AHProseMirror`，改由 markdown_editor 按需静态 import。
   - **验收**：460 个演示比对，变化只在图表（数据表、角色）和带 `href` 的导航演示；1423 个 EUnit、445 个浏览器测试通过；演示站 112 个组件页无脚本错误。
5. **TypeScript（已完成）**：
   - 运行时拆成 `runtime/` 下的类（Behaviours、Controller、Swapper、Actions、PushStream、Fetcher、FloatingPopup、Theme），`core.ts` 把它们接成 `AH` 对象（`AHApi` 接口）；服务端的 DOM 操作是类型化的联合类型 `Op`。
   - 110 个组件文件改成 `components/<name>.ts`：每个行为一个具名控制器类，状态是私有字段，内部逻辑是私有方法，事件 detail 是导出的接口；几个组件共用的有状态部分成了 lib 里的类（`ButtonMenu`、`OverlayLayer`、`Drag`、`TableFrame`、`PickerPopup`……），纯计算保留为带类型的导出函数。`AH.lib` 只留下测试和页面脚本要用的 `values`、`chart`。
   - `tsconfig.json` 开 `strict`，不用 `any` 和 `@ts-ignore`；`npm run typecheck`，`npm test` 先跑它。`types/catalog.d.ts` 由 `aihtml_catalog` 生成，`AH.register` 按它检查每个控制器有没有服务端会调用的方法。组件给 `AHApi` 加的东西（`AH.notify`、`AH.toast`）用 `declare module "../core.ts"` 声明。
   - 构建目标改为 es2022，`#私有字段` 原样输出（es2020 要转成 WeakMap 辅助函数，更大也更慢）。
   - **验收**：服务端 HTML 不变（460 个演示比对，只有一处有意改的演示文字）；444 个浏览器测试不改动全部通过；1423 个 EUnit 通过；演示站 112 页无脚本错误。入口 gzip 后 23.7 KB。
   - 浏览器测试仍是 JS，只通过 DOM、事件和 `AH.invoke` 测行为。
