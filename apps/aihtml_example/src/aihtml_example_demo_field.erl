%% @doc Demos of the field (and validate/1) component (aihtml_field), shown on
%% /components/field. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_field).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([field_positions/0, field_help/0, field_validate/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => field, title => <<"Field">>,
       summary => <<"一行表单项：标签、控件，以及帮助或错误信息。"/utf8>>,
       demos => [{<<"标签位置"/utf8>>, field_positions},
                 {<<"必填、帮助与错误"/utf8>>, field_help},
                 {<<"客户端校验：气泡与标签提示"/utf8>>, field_validate}]}].

-spec field_positions() -> aihtml:html().
field_positions() ->
    ah_div([ah_field(<<"Name">>, ah_input(undefined, [], [{id, <<"fp-name">>}]),
                     [], [{for, <<"fp-name">>}, {label_width, 90}]),
            ah_field(<<"Country">>, ah_dropdownlist([{cn, <<"China">>}, {jp, <<"Japan">>}], cn, [block], []),
                     [top], []),
            ah_field(<<"Remember me">>, ah_checkbox(<<>>, yes, [], [{name, remember}, {id, <<"fp-rem">>}]),
                     [right], [{for, <<"fp-rem">>}])],
           [<<"max-w-md">>], []).

-spec field_help() -> aihtml:html().
field_help() ->
    ah_div([ah_field(<<"Name">>, ah_input(undefined, [], [{id, <<"fh-name">>}]), [],
                     [{for, <<"fh-name">>}, {required, true}, {label_width, 90},
                      {help, <<"As printed on your card.">>}]),
            ah_field(<<"Email">>, ah_input(<<"not-an-email">>, [], [{id, <<"fh-email">>}]), [],
                     [{for, <<"fh-email">>}, {label_width, 90},
                      {error, <<"Please enter a valid email address.">>}]),
            ah_field(<<"Plan">>, ah_dropdownlist([free, pro], pro, [block], []), [],
                     [{label_width, 90}, {info, <<"You can change it later.">>}])],
           [<<"max-w-md">>], []).

-spec field_validate() -> aihtml:html().
field_validate() ->
    ah_form([ah_input(undefined, [<<"w-64">>],
                      [{name, zip}, {placeholder, <<"ZIP code (tooltip)">>},
                       validate([{required, <<"Enter a ZIP code">>}, zip_code, {hint, tooltip}])]),
             ah_input(undefined, [<<"w-64">>],
                      [{name, nick}, {placeholder, <<"Nickname (label)">>},
                       validate([{min_length, 3}, {hint, label}])]),
             ah_button(<<"Check">>, undefined, [outlined], [{type, submit}])],
            [<<"flex flex-col items-start gap-3">>], [{action, <<"#checked">>}]).
