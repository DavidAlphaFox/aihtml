%% @doc Demos of the Markdown editor (aihtml_markdown_editor), shown on
%% /components/markdown_editor. Each function is one example, written the
%% way an application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the postback demos.
-module(aihtml_example_demo_markdown_editor).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([md_basic/0, md_stats/0, md_readonly/0, md_form/0, md_postback/0, md_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => markdown_editor, title => <<"MarkdownEditor">>,
       summary => <<"所见即所得的 Markdown 编辑器：输入 Markdown 语法即时排版，/ 打开块菜单，拖动手柄调整顺序。"/utf8>>,
       demos => [{<<"基本用法：Markdown 快捷输入与块菜单"/utf8>>, md_basic},
                 {<<"字数统计与字数上限"/utf8>>, md_stats},
                 {<<"只读"/utf8>>, md_readonly},
                 {<<"在表单里提交 Markdown"/utf8>>, md_form},
                 {<<"失去焦点时通知服务端"/utf8>>, md_postback},
                 {<<"record 写法"/utf8>>, md_record}]}].

%%%===================================================================
%%% MarkdownEditor
%%%===================================================================

-spec md_basic() -> aihtml:html().
md_basic() ->
    markdown_editor(<<"## 周报\n\n"
                      "输入 `# ` 变成标题，`**粗体**`、`*斜体*`、`- ` 列表、`> ` 引用、"
                      "`[文字](网址)` 链接。\n\n"
                      "- [x] 完成设计评审\n- [ ] 补充测试\n\n"
                      "> 在空行输入 / 打开块菜单；鼠标移到段落左侧，拖动手柄调整顺序。\n"/utf8>>,
                    [], [{placeholder, <<"开始输入..."/utf8>>}]).

-spec md_stats() -> aihtml:html().
md_stats() ->
    markdown_editor(<<"简介不超过 80 个字符：aihtml 用 Erlang 在服务端渲染页面，"
                      "浏览器端只做增强。"/utf8>>,
                    [], [{show_stats, true}, {max_chars, 80}, {height, 260},
                         {labels, #{chars => <<"字符"/utf8>>, words => <<"词语"/utf8>>,
                                    paragraphs => <<"段落"/utf8>>, limit => <<"限制"/utf8>>,
                                    exceeded => <<"已超出限制"/utf8>>}}]).

-spec md_readonly() -> aihtml:html().
md_readonly() ->
    markdown_editor(<<"### 发布说明\n\n"
                      "| 版本 | 日期 |\n| --- | --- |\n| 0.3 | 2026-09-29 |\n\n"
                      "```erlang\nmarkdown_editor(Md, [readonly], []).\n```\n"/utf8>>,
                    [readonly], []).

-spec md_form() -> aihtml:html().
md_form() ->
    form([markdown_editor(<<"写下评论……"/utf8>>, [], [{name, comment}, {height, 220}]),
          'div'(button(<<"提交"/utf8>>, submit, [primary], [{type, submit}]),
                [<<"mt-3">>], []),
          p(<<"提交后服务端收到的 Markdown 显示在这里"/utf8>>,
            [<<"mt-2 text-sm text-muted whitespace-pre-wrap">>], [{id, <<"md-form-result">>}])],
         [], [on(submit, {?MODULE, md_submitted, #{}})]).

-spec md_postback() -> aihtml:html().
md_postback() ->
    'div'([markdown_editor(<<"修改后点击编辑器外的地方，服务端会收到新的 Markdown。"/utf8>>,
                           [], [on(change, {?MODULE, md_changed, #{target => <<"md-changed">>}})]),
           p(<<"还没有修改"/utf8>>, [<<"mt-2 text-sm text-muted whitespace-pre-wrap">>],
             [{id, <<"md-changed">>}])], [], []).

%% The same component as a record: options are checked field names, and
%% the postback runs action(md_changed, ...) below on change.
-spec md_record() -> aihtml:html().
md_record() ->
    'div'([#ah_markdown_editor{value = <<"# 草稿\n\n用 record 写的编辑器。"/utf8>>,
                               name = draft, placeholder = <<"开始输入..."/utf8>>,
                               show_stats = true, height = 240,
                               postback = {md_changed, #{target => <<"md-record-changed">>}}},
           p(<<"还没有修改"/utf8>>, [<<"mt-2 text-sm text-muted whitespace-pre-wrap">>],
             [{id, <<"md-record-changed">>}])], [], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(md_changed, #{target := Target}, #{value := Md}, Ctx) ->
    aihtml_action:html(Ctx, {id, Target}, [<<"服务端收到：\n"/utf8>>, Md]);
action(md_submitted, _Args, #{form := #{<<"comment">> := Md}}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"md-form-result">>}, [<<"服务端收到：\n"/utf8>>, Md]).
