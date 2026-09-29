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
    'div'([field(<<"Name">>, input(undefined, [], [{id, <<"fp-name">>}]),
                 [], [{for, <<"fp-name">>}, {label_width, 90}]),
           field(<<"Country">>, dropdownlist([{cn, <<"China">>}, {jp, <<"Japan">>}], cn, [block], []),
                 [top], []),
           field(<<"Remember me">>, checkbox(<<>>, yes, [], [{name, remember}, {id, <<"fp-rem">>}]),
                 [right], [{for, <<"fp-rem">>}])],
          [<<"max-w-md">>], []).

-spec field_help() -> aihtml:html().
field_help() ->
    'div'([field(<<"Name">>, input(undefined, [], [{id, <<"fh-name">>}]), [],
                 [{for, <<"fh-name">>}, {required, true}, {label_width, 90},
                  {help, <<"As printed on your card.">>}]),
           field(<<"Email">>, input(<<"not-an-email">>, [], [{id, <<"fh-email">>}]), [],
                 [{for, <<"fh-email">>}, {label_width, 90},
                  {error, <<"Please enter a valid email address.">>}]),
           field(<<"Plan">>, dropdownlist([free, pro], pro, [block], []), [],
                 [{label_width, 90}, {info, <<"You can change it later.">>}])],
          [<<"max-w-md">>], []).

-spec field_validate() -> aihtml:html().
field_validate() ->
    form([input(undefined, [<<"w-64">>],
                [{name, zip}, {placeholder, <<"ZIP code (tooltip)">>},
                 validate([{required, <<"Enter a ZIP code">>}, zip_code, {hint, tooltip}])]),
          input(undefined, [<<"w-64">>],
                [{name, nick}, {placeholder, <<"Nickname (label)">>},
                 validate([{min_length, 3}, {hint, label}])]),
          button(<<"Check">>, undefined, [outlined], [{type, submit}])],
         [<<"flex flex-col items-start gap-3">>], [{action, <<"#checked">>}]).
