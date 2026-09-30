%% @doc Demos of the upload component (aihtml_upload), shown on
%% /components/upload. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% Files go to "/upload" (aihtml_example_upload), which answers JSON
%% describing each file and keeps nothing. The module is also the action
%% module of the demos that react to change or call methods.
-module(aihtml_example_demo_upload).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([upload_basic/0, upload_change/0, upload_manual/0, upload_limits/0,
         upload_existing/0, upload_native/0, upload_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => upload, title => <<"Upload">>,
       summary => <<"拖拽或点击选择文件，逐个上传并显示进度，支持类型、大小和数量限制。"/utf8>>,
       demos => [{<<"拖拽或点击上传"/utf8>>, upload_basic},
                 {<<"上传完成后通知服务端"/utf8>>, upload_change},
                 {<<"手动上传与清空"/utf8>>, upload_manual},
                 {<<"类型、大小和数量限制"/utf8>>, upload_limits},
                 {<<"已上传的文件与禁用"/utf8>>, upload_existing},
                 {<<"不设 url：随表单原生提交"/utf8>>, upload_native},
                 {<<"record 写法"/utf8>>, upload_record}]}].

-spec upload_basic() -> aihtml:html().
upload_basic() ->
    ah_upload([], [<<"max-w-lg">>],
              [{url, <<"/upload">>}, {name, attachments},
               {drag_text, <<"拖拽文件到此处，或"/utf8>>}, {browse_text, <<"点击上传"/utf8>>},
               {hint, <<"任意类型，单个不超过 5 MB"/utf8>>}]).

%% Each finished file fires change; action(uploaded, ...) below lists them.
-spec upload_change() -> aihtml:html().
upload_change() ->
    ah_div([ah_upload([], [<<"max-w-lg">>],
                      [{url, <<"/upload">>}, {hint, <<"上传后服务端收到文件列表"/utf8>>},
                       on(change, {?MODULE, uploaded, <<"upload-received">>})]),
            ah_p(<<"还没有上传文件"/utf8>>, [<<"text-sm text-muted mt-2">>],
                 [{id, <<"upload-received">>}])],
           [], []).

-spec upload_manual() -> aihtml:html().
upload_manual() ->
    ah_div([ah_upload([], [<<"max-w-lg mb-3">>],
                      [{id, <<"manual-upload">>}, {url, <<"/upload">>}, {auto_upload, false},
                       {browse_text, <<"选择文件"/utf8>>},
                       {hint, <<"选好后点“上传全部”"/utf8>>}]),
            ah_div([ah_button(<<"上传全部"/utf8>>, undefined, [primary, sm],
                              [on(click, {?MODULE, call, uploadAll})]),
                    ah_button(<<"清空列表"/utf8>>, undefined, [outlined, sm],
                              [on(click, {?MODULE, call, clear})])],
                   [<<"flex gap-2">>], [])],
           [], []).

-spec upload_limits() -> aihtml:html().
upload_limits() ->
    ah_upload([], [<<"max-w-lg">>],
              [{url, <<"/upload">>}, {accept, <<"image/*">>}, {max_size, 2 * 1024 * 1024},
               {max_count, 3}, {hint, <<"只收图片，最多 3 个，单个不超过 2 MB"/utf8>>},
               {labels, #{type_mismatch => <<"不是图片"/utf8>>,
                          too_large => <<"超过 2 MB"/utf8>>,
                          too_many => <<"最多 3 个文件"/utf8>>,
                          upload_failed => <<"上传失败"/utf8>>,
                          remove => <<"删除"/utf8>>}}]).

-spec upload_existing() -> aihtml:html().
upload_existing() ->
    Files = [#{id => 11, name => <<"合同扫描件.pdf"/utf8>>, size => 482133,
               type => <<"application/pdf">>},
             #{id => 12, name => <<"logo.png">>, size => 18420, type => <<"image/png">>}],
    ah_div([ah_upload(Files, [<<"max-w-lg">>], [{url, <<"/upload">>}, {name, files}]),
            ah_upload(Files, [disabled, <<"max-w-lg">>],
                      [{url, <<"/upload">>}, {hint, <<"已禁用"/utf8>>}])],
           [<<"flex flex-col gap-6">>], []).

-spec upload_native() -> aihtml:html().
upload_native() ->
    ah_form([ah_upload([], [<<"max-w-lg mb-3">>],
                       [{name, file}, {multiple, false}, {hint, <<"提交时随表单一起发送"/utf8>>}]),
             ah_button(<<"提交表单"/utf8>>, undefined, [primary, sm], [{type, submit}])],
            [], [{action, <<"/upload">>}, {method, post},
                 {enctype, <<"multipart/form-data">>}, {target, <<"_blank">>}]).

%% The same component as a record: options are checked field names, and
%% the postback runs action(uploaded, ...) below on change.
-spec upload_record() -> aihtml:html().
upload_record() ->
    ah_div([#ah_upload{url = <<"/upload">>, accept = <<"image/*,.pdf">>, max_count = 5,
                       field_name = <<"document">>, extra_data = #{folder => <<"inbox">>},
                       hint = <<"图片或 PDF，最多 5 个"/utf8>>, css = [<<"max-w-lg">>],
                       postback = {uploaded, <<"record-received">>}},
            ah_p(<<"还没有上传文件"/utf8>>, [<<"text-sm text-muted mt-2">>],
                 [{id, <<"record-received">>}])],
           [], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(uploaded, Target, Event, Ctx) ->
    Names = [maps:get(<<"name">>, F, <<"?">>) || F <- uploaded_files(Event), is_map(F)],
    Text = case Names of
               [] -> <<"列表已清空"/utf8>>;
               _ -> [<<"服务端收到 "/utf8>>, integer_to_binary(length(Names)),
                     <<" 个文件："/utf8>>, lists:join(<<"、"/utf8>>, Names)]
           end,
    aihtml_action:html(Ctx, {id, Target}, Text);
action(call, Method, _Event, Ctx) ->
    aihtml_action:call(Ctx, {id, <<"manual-upload">>}, Method, []).
