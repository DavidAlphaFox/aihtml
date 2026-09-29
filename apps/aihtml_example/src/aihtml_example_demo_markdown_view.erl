%% @doc Demos of the Markdown view (aihtml_markdown_view), shown on
%% /components/markdown_view. Each function is one example, written the
%% way an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the live preview demo.
-module(aihtml_example_demo_markdown_view).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([mv_article/0, mv_table_tasks/0, mv_live/0, mv_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => markdown_view, title => <<"MarkdownView">>,
       summary => <<"在服务端把 Markdown 渲染成 HTML：与编辑器同样的语法，读者和搜索引擎直接看到标题、列表和表格。"/utf8>>,
       demos => [{<<"渲染一篇文章"/utf8>>, mv_article},
                 {<<"表格与任务列表"/utf8>>, mv_table_tasks},
                 {<<"编辑器旁的实时预览（服务端渲染）"/utf8>>, mv_live},
                 {<<"record 写法"/utf8>>, mv_record}]}].

%%%===================================================================
%%% MarkdownView
%%%===================================================================

-spec mv_article() -> aihtml:html().
mv_article() ->
    markdown_view(<<"# 用 Erlang 渲染页面\n\n"
                    "aihtml 在**服务端**生成全部 HTML，浏览器端只做*增强*。"
                    "所以页面不运行 JavaScript 也能读，搜索引擎看到的就是这份 HTML。\n\n"
                    "## 为什么这样做\n\n"
                    "1. 首屏快：不用等脚本下载完\n"
                    "2. 好索引：标题、链接、列表都是真正的标签\n"
                    "3. 少出错：状态留在服务端\n\n"
                    "> Markdown 里的原始 HTML（例如 `<script>`）只当作文字显示。\n\n"
                    "```erlang\n"
                    "markdown_view(Markdown, [], []).\n"
                    "```\n\n"
                    "---\n\n"
                    "详见 [设计文档](/components/markdown_editor)。\n"/utf8>>,
                  [<<"max-w-2xl">>], []).

-spec mv_table_tasks() -> aihtml:html().
mv_table_tasks() ->
    markdown_view(<<"### 发布计划\n\n"
                    "| 版本 | 日期 | 状态 |\n"
                    "| :--- | :---: | ---: |\n"
                    "| 0.3 | 2026-09-29 | ~~延期~~ 已发布 |\n"
                    "| 0.4 | 2026-11-15 | 开发中 |\n\n"
                    "- [x] 服务端渲染 Markdown\n"
                    "- [x] 与编辑器的 markdown-it 输出一致\n"
                    "- [ ] 代码高亮\n"/utf8>>,
                  [], []).

%% The editor sends its Markdown on every edit (debounced); the action
%% renders the view again on the server and patches it into the page.
-spec mv_live() -> aihtml:html().
mv_live() ->
    Md = <<"## 实时预览\n\n在左边编辑，右边是**服务端**渲染的结果：\n\n"
           "- [x] 任务列表\n- [ ] `代码` 和 [链接](https://example.com)\n\n> 引用、表格、代码块也都支持。\n"/utf8>>,
    'div'([markdown_editor(Md, [], [{height, 320},
                                     on(input, {?MODULE, mv_preview,
                                                #{target => <<"mv-live-view">>}},
                                        #{debounce => 250})]),
           'div'(markdown_view(Md, [], []),
                 [<<"rounded border border-line p-4 overflow-auto">>],
                 [{id, <<"mv-live-view">>}, {style, <<"height:320px">>}])],
          [<<"grid gap-4 md:grid-cols-2">>], []).

%% The same component as a record.
-spec mv_record() -> aihtml:html().
mv_record() ->
    #ah_markdown_view{markdown = <<"**record** 写法：`#ah_markdown_view{markdown = Md}`。"/utf8>>,
                      id = <<"mv-record">>, css = [<<"text-sm">>]}.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(mv_preview, #{target := Target}, #{value := Md}, Ctx) ->
    aihtml_action:html(Ctx, {id, Target}, markdown_view(Md, [], []), morph_inner).
