%% @doc Demos of the one-time code input (aihtml_input_otp), shown on
%% /components/input_otp. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_input_otp).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([otp_digits/0, otp_separator/0, otp_alphanumeric/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => input_otp, title => <<"InputOTP">>,
       summary => <<"分段验证码输入，自动跳格，粘贴整串自动填满。"/utf8>>,
       demos => [{<<"六位数字"/utf8>>, otp_digits},
                 {<<"分隔符"/utf8>>, otp_separator},
                 {<<"字母数字与禁用"/utf8>>, otp_alphanumeric}]}].

-spec otp_digits() -> aihtml:html().
otp_digits() ->
    ah_input_otp(6, undefined, [], [{name, code}]).

-spec otp_separator() -> aihtml:html().
otp_separator() ->
    ah_input_otp(6, <<"123456">>, [], [{separator_at, 3}]).

-spec otp_alphanumeric() -> aihtml:html().
otp_alphanumeric() ->
    row([ah_input_otp(4, <<"A1">>, [], [{pattern, alphanumeric}]),
         ah_input_otp(4, <<"12">>, [disabled], [])]).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-start gap-4">>], []).
