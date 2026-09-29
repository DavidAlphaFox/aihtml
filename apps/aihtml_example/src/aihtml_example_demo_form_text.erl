%% @doc Demos of the text entry components (aihtml_form_text), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_form_text).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([input_sizes/0, input_states/0, input_addons/0, input_clearable/0,
         input_label/0, textarea_basic/0, textarea_states/0,
         password_basic/0, password_strength/0, password_states/0,
         number_basic/0, number_symbols/0, number_states/0,
         otp_digits/0, otp_separator/0, otp_alphanumeric/0,
         tags_basic/0, tags_limits/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => input, title => <<"Input">>,
       summary => <<"单行文本输入，支持尺寸、校验状态、前后缀、清除按钮和浮动标签。"/utf8>>,
       demos => [{<<"尺寸"/utf8>>, input_sizes},
                 {<<"校验状态与禁用"/utf8>>, input_states},
                 {<<"前缀与后缀"/utf8>>, input_addons},
                 {<<"可清除"/utf8>>, input_clearable},
                 {<<"浮动标签"/utf8>>, input_label}]},
     #{component => textarea, title => <<"Textarea">>,
       summary => <<"多行文本输入，样式与 Input 一致。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, textarea_basic},
                 {<<"尺寸、状态与浮动标签"/utf8>>, textarea_states}]},
     #{component => password_input, title => <<"PasswordInput">>,
       summary => <<"密码输入框，可切换明文显示，可显示强度。"/utf8>>,
       demos => [{<<"显示与隐藏"/utf8>>, password_basic},
                 {<<"强度指示"/utf8>>, password_strength},
                 {<<"尺寸与状态"/utf8>>, password_states}]},
     #{component => number_input, title => <<"NumberInput">>,
       summary => <<"数值输入框，带微调按钮，支持方向键、滚轮和范围限制。"/utf8>>,
       demos => [{<<"范围与步长"/utf8>>, number_basic},
                 {<<"前缀、后缀与无按钮"/utf8>>, number_symbols},
                 {<<"尺寸与状态"/utf8>>, number_states}]},
     #{component => input_otp, title => <<"InputOTP">>,
       summary => <<"分段验证码输入，自动跳格，粘贴整串自动填满。"/utf8>>,
       demos => [{<<"六位数字"/utf8>>, otp_digits},
                 {<<"分隔符"/utf8>>, otp_separator},
                 {<<"字母数字与禁用"/utf8>>, otp_alphanumeric}]},
     #{component => tag_input, title => <<"TagInput">>,
       summary => <<"标签输入框，回车或逗号添加，退格删除末项。"/utf8>>,
       demos => [{<<"基本用法"/utf8>>, tags_basic},
                 {<<"数量上限、颜色与禁用"/utf8>>, tags_limits}]}].

%%% Input

-spec input_sizes() -> aihtml:html().
input_sizes() ->
    row([input(undefined, [sm, <<"w-56">>], [{placeholder, <<"Small">>}]),
         input(undefined, [<<"w-56">>], [{placeholder, <<"Medium">>}, {name, q}]),
         input(undefined, [lg, <<"w-56">>], [{placeholder, <<"Large">>}])]).

-spec input_states() -> aihtml:html().
input_states() ->
    row([input(<<"not-an-email">>, [invalid, <<"w-56">>], [{name, email}]),
         input(<<"ada@example.com">>, [valid, <<"w-56">>], []),
         input(<<"Disabled">>, [disabled, <<"w-56">>], []),
         input(undefined, [no_rounded, <<"w-56">>], [{placeholder, <<"No rounding">>}])]).

-spec input_addons() -> aihtml:html().
input_addons() ->
    row([input(undefined, [<<"w-64">>], [{prefix, <<"https://">>},
                                         {placeholder, <<"example.com">>}]),
         input(<<"42">>, [<<"w-40">>], [{suffix, <<"kg">>}]),
         input(<<"19.99">>, [<<"w-48">>], [{prefix, <<"$">>}, {suffix, <<"USD">>}])]).

-spec input_clearable() -> aihtml:html().
input_clearable() ->
    Search = safe(<<"<svg width=\"14\" height=\"14\" viewBox=\"0 0 24 24\" fill=\"none\" "
                    "stroke=\"currentColor\" stroke-width=\"2\"><circle cx=\"11\" cy=\"11\" "
                    "r=\"7\"/><path d=\"m20 20-3.5-3.5\"/></svg>">>),
    row([input(undefined, [clearable, <<"w-64">>], [{prefix, Search},
                                                   {placeholder, <<"Search">>}]),
         input(<<"Clear me">>, [clearable, <<"w-56">>], [])]).

-spec input_label() -> aihtml:html().
input_label() ->
    row([input(undefined, [<<"w-56">>], [{label, <<"Full name">>}]),
         input(<<"Ada Lovelace">>, [<<"w-56">>], [{label, <<"Full name">>}])]).

%%% Textarea

-spec textarea_basic() -> aihtml:html().
textarea_basic() ->
    row([textarea(undefined, [<<"w-80">>], [{name, notes},
                                            {placeholder, <<"Write something...">>}]),
         textarea(<<"Line one\nLine two">>, [<<"w-80">>], [{rows, 4}])]).

-spec textarea_states() -> aihtml:html().
textarea_states() ->
    row([textarea(undefined, [sm, <<"w-60">>], [{label, <<"Notes">>}]),
         textarea(<<"Too short">>, [invalid, <<"w-60">>], []),
         textarea(<<"Read only">>, [disabled, <<"w-60">>], [])]).

%%% PasswordInput

-spec password_basic() -> aihtml:html().
password_basic() ->
    row([password_input(<<"secret123">>, [<<"w-60">>], [{name, password}]),
         password_input(undefined, [<<"w-60">>], [{label, <<"Password">>}]),
         password_input(undefined, [<<"w-60">>], [{toggle, false},
                                                 {placeholder, <<"No toggle">>}])]).

-spec password_strength() -> aihtml:html().
password_strength() ->
    password_input(undefined, [<<"w-72">>], [{strength, true}, {name, new_password},
                                             {placeholder, <<"New password">>}]).

-spec password_states() -> aihtml:html().
password_states() ->
    row([password_input(<<"x">>, [sm, invalid, <<"w-60">>], []),
         password_input(undefined, [lg, <<"w-60">>], [{placeholder, <<"Large">>}]),
         password_input(<<"hunter2">>, [disabled, <<"w-60">>], [])]).

%%% NumberInput

-spec number_basic() -> aihtml:html().
number_basic() ->
    row([number_input(5, [<<"w-40">>], [{min, 0}, {max, 10}, {name, qty}]),
         number_input(<<"2.5">>, [<<"w-40">>], [{step, 0.5}, {min, 0}]),
         number_input(undefined, [<<"w-40">>], [{label, <<"Amount">>}])]).

-spec number_symbols() -> aihtml:html().
number_symbols() ->
    row([number_input(19.5, [<<"w-44">>], [{step, 0.5}, {symbol, <<"$">>}]),
         number_input(75, [<<"w-40">>], [{min, 0}, {max, 100}, {symbol, <<"%">>},
                                         {symbol_position, right}]),
         number_input(undefined, [<<"w-40">>], [{spin, false},
                                                {placeholder, <<"No spin">>}])]).

-spec number_states() -> aihtml:html().
number_states() ->
    row([number_input(1, [sm, <<"w-32">>], []),
         number_input(2, [lg, <<"w-40">>], []),
         number_input(200, [invalid, <<"w-40">>], [{max, 100}]),
         number_input(3, [readonly, <<"w-32">>], []),
         number_input(4, [disabled, <<"w-32">>], [])]).

%%% InputOTP

-spec otp_digits() -> aihtml:html().
otp_digits() ->
    input_otp(6, undefined, [], [{name, code}]).

-spec otp_separator() -> aihtml:html().
otp_separator() ->
    input_otp(6, <<"123456">>, [], [{separator_at, 3}]).

-spec otp_alphanumeric() -> aihtml:html().
otp_alphanumeric() ->
    row([input_otp(4, <<"A1">>, [], [{pattern, alphanumeric}]),
         input_otp(4, <<"12">>, [disabled], [])]).

%%% TagInput

-spec tags_basic() -> aihtml:html().
tags_basic() ->
    tag_input([<<"erlang">>, <<"jquery">>, <<"tailwind">>], [<<"w-96">>], [{name, tags}]).

-spec tags_limits() -> aihtml:html().
tags_limits() ->
    'div'([tag_input([<<"red">>], [<<"w-96">>], [{max_tags, 3}, {chip_color, error},
                                                 {chip_variant, filled},
                                                 {placeholder, <<"Up to 3 tags">>}]),
           tag_input([<<"locked">>], [disabled, <<"w-96">>], [])],
          [<<"flex flex-col gap-3">>], []).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
