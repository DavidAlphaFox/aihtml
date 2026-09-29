%% @doc Demos of the masked input (aihtml_masked_input), shown on
%% /components/masked_input.
%% Each function is one example, written the way an application writes
%% it; the docs page prints its source under it.
%%
%% The module is also the action module of mask_change (a masked input's
%% change).
-module(aihtml_example_demo_masked_input).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([mask_phone/0, mask_formats/0, mask_label/0, mask_states/0, mask_change/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => masked_input, title => <<"MaskedInput">>,
       summary => <<"带输入掩码的文本框，电话、日期、编码只能按格式输入。"/utf8>>,
       demos => [{<<"电话与邮编"/utf8>>, mask_phone},
                 {<<"日期、时间、自定义字符类"/utf8>>, mask_formats},
                 {<<"浮动标签、占位字符、值带字面量"/utf8>>, mask_label},
                 {<<"禁用、只读、直角"/utf8>>, mask_states},
                 {<<"失焦后通知服务端"/utf8>>, mask_change}]}].

-spec mask_phone() -> aihtml:html().
mask_phone() ->
    row([masked_input(<<"5551234567">>, [<<"w-48">>],
                      [{mask, <<"(999) 999-9999">>}, {name, phone}]),
         masked_input(undefined, [<<"w-40">>], [{mask, <<"99999-9999">>}, {name, zip}])]).

-spec mask_formats() -> aihtml:html().
mask_formats() ->
    row([masked_input(<<"09/29/2026">>, [<<"w-36">>], [{mask, <<"99/99/9999">>}]),
         masked_input(undefined, [<<"w-24">>], [{mask, <<"[0-2][0-9]:[0-5][0-9]">>}]),
         masked_input(<<"ABC1234">>, [<<"w-36">>], [{mask, <<"LLL-9999">>}]),
         masked_input(undefined, [<<"w-56">>],
                      [{mask, <<"[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]:"
                                "[0-9A-F][0-9A-F]">>}])]).

-spec mask_label() -> aihtml:html().
mask_label() ->
    row([masked_input(undefined, [floating_label, <<"w-48">>],
                      [{mask, <<"(999) 999-9999">>}, {placeholder, <<"手机号"/utf8>>}]),
         masked_input(<<"4111111111111111">>, [<<"w-60">>],
                      [{mask, <<"9999 9999 9999 9999">>}, {prompt_char, <<"*">>},
                       {include_literals, true}, {name, card}])]).

-spec mask_states() -> aihtml:html().
mask_states() ->
    row([masked_input(<<"12345">>, [disabled, <<"w-32">>], []),
         masked_input(<<"12345">>, [readonly, <<"w-32">>], []),
         masked_input(undefined, [square, <<"w-32">>], [])]).

%% Leaving the field after an edit runs action(masked, ...) below.
-spec mask_change() -> aihtml:html().
mask_change() ->
    row([masked_input(undefined, [<<"w-48">>],
                      [{mask, <<"(999) 999-9999">>},
                       on(change, {?MODULE, masked, #{}})]),
         span(<<"输入后按 Tab 离开"/utf8>>, [<<"text-sm text-muted">>], [{id, <<"masked-out">>}])]).

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(masked, _Args, #{value := Value}, Ctx) ->
    aihtml_action:html(Ctx, {id, <<"masked-out">>}, [<<"服务端收到："/utf8>>, Value]).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-4">>], []).
