%% @doc Demos of the form_layout component (aihtml_form_layout), shown on
%% /components/form_layout. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_form_layout).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([form_basic/0, form_columns/0, form_validate/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => form_layout, title => <<"Form">>,
       summary => <<"声明式表单：用字段列表和取值生成整张表单。"/utf8>>,
       demos => [{<<"字段与取值"/utf8>>, form_basic},
                 {<<"多列、文字行与空行"/utf8>>, form_columns},
                 {<<"提交前校验"/utf8>>, form_validate}]}].

-spec form_basic() -> aihtml:html().
form_basic() ->
    Values = #{name => <<"Ada">>, plan => pro, budget => 400},
    ah_form_layout(
         [#{label => <<"Name">>, key => name,
            control => fun(V) -> ah_input(V, [], [{name, name}]) end},
          #{label => <<"Plan">>, key => plan,
            control => fun(V) -> ah_dropdownlist([free, pro, team], V, [block], [{name, plan}]) end},
          #{label => <<"Budget">>, key => budget,
            control => fun(V) -> ah_slider({0, 1000, 50}, V, [tooltip], [{name, budget}]) end},
          {<<>>, ah_button(<<"Save">>, undefined, [], [{type, submit}])}],
         Values, [bordered, bg, <<"max-w-lg">>], [{label_width, 90}, {action, <<"#saved">>}]).

-spec form_columns() -> aihtml:html().
form_columns() ->
    ah_form_layout(
         [{text, <<"Shipping address">>},
          {<<"Street">>, ah_input(undefined, [], [{name, street}])},
          {columns, [{<<"City">>, ah_input(undefined, [], [{name, city}])},
                     {<<"ZIP">>, ah_input(undefined, [], [{name, zip}])}]},
          blank,
          #{label => <<"Country">>, label_position => top,
            control => ah_select([{cn, <<"China">>}, {jp, <<"Japan">>}, {us, <<"USA">>}], us,
                                 [block], [{name, country}])}],
         #{}, [bordered, <<"max-w-lg">>], [{tag, 'div'}, {label_width, 70}]).

-spec form_validate() -> aihtml:html().
form_validate() ->
    ah_form_layout(
         [#{label => <<"User">>, key => user, required => true,
            control => fun(V) -> ah_input(V, [], [{name, user},
                                                  validate([required, {min_length, 3},
                                                            {starts_with_letter, <<"Must start with a letter">>}])])
                       end},
          #{label => <<"Email">>, key => email, required => true, help => <<"We never share it.">>,
            control => fun(V) -> ah_input(V, [], [{name, email}, validate([required, email])]) end},
          #{label => <<"Age">>, key => age,
            control => fun(V) -> ah_input(V, [], [{name, age}, validate([integer, {range, 18, 120}])]) end},
          #{label => <<"Plan">>, key => plan, required => true,
            control => fun(V) -> ah_dropdownlist([free, pro, team], V, [block],
                                                 [{name, plan}, validate([required])])
                       end},
          {<<>>, ah_button(<<"Sign up">>, undefined, [], [{type, submit}])}],
         #{user => <<"x">>, age => <<"12">>},
         [bordered, bg, <<"max-w-lg">>], [{label_width, 70}, {action, <<"#signed-up">>}]).
