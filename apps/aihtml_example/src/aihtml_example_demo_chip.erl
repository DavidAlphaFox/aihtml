%% @doc Demos of the chip component (aihtml_chip), shown on
%% /components/chip. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_chip).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([chip_variants/0, chip_features/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => chip, title => <<"Chip">>,
       summary => <<"可点击、可删除的小标签。"/utf8>>,
       demos => [{<<"变体与颜色"/utf8>>, chip_variants},
                 {<<"头像、删除、点击、禁用"/utf8>>, chip_features}]}].

%%% Chip

-spec chip_variants() -> aihtml:html().
chip_variants() ->
    Colors = [default, primary, success, warning, error, info],
    'div'([row([chip(atom_to_binary(C), [Variant, C], []) || C <- Colors])
           || Variant <- [filled, outlined, soft]],
          [<<"flex flex-col gap-3">>], []).

-spec chip_features() -> aihtml:html().
chip_features() ->
    row([chip(<<"Small">>, [small, primary], []),
         chip(<<"Jane Doe">>, [soft, primary], [{avatar, <<"JD">>}]),
         chip(<<"Erlang">>, [removable, outlined, info], [{value, erlang}]),
         chip(<<"Clickable">>, [clickable, soft, success], []),
         chip(<<"Disabled">>, [disabled, primary], [])]).

%% Layout helpers of the demos.
row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-4">>], []).
