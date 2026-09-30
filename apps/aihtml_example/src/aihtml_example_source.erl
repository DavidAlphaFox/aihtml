%% @doc The code shown next to each demo: the demo function's own source,
%% taken from the module's debug_info and pretty-printed, so the code on
%% the page is always the code that rendered the example. Tokens are
%% wrapped in spans for a light syntax highlight (comments are not in
%% debug_info, so demos explain themselves through their titles).
-module(aihtml_example_source).

-export([function/2, highlight/1]).

%% @doc Pretty-printed source of Mod:Fun/0.
-spec function(module(), atom()) -> binary().
function(Mod, Fun) ->
    {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} =
        beam_lib:chunks(code:which(Mod), [abstract_code]),
    [F] = [F || {function, _, N, 0, _} = F <- Forms, N =:= Fun],
    unicode:characters_to_binary(erl_pp:function(F, [{encoding, utf8}])).

%% @doc Erlang source as html: a list of text and <span class="tok-...">.
-spec highlight(binary()) -> aihtml:html().
highlight(Src) ->
    case erl_scan:string(unicode:characters_to_list(Src), {1, 1}, [text, return_comments]) of
        {ok, Tokens, _} -> spans(Tokens, {1, 1}, []);
        _ -> Src
    end.

spans([], _Pos, Acc) -> lists:reverse(Acc);
spans([T | Rest], Pos, Acc) ->
    {L, C} = erl_scan:location(T),
    Text = unicode:characters_to_binary(erl_scan:text(T)),
    Gap = gap(Pos, {L, C}),
    Out = case class(T) of
              none -> Text;
              Cls -> aihtml:ah_el(span, Text, [Cls], [])
          end,
    spans(Rest, advance({L, C}, Text), [Out, Gap | Acc]).

%% whitespace between the previous token's end and this token's start
gap({L0, _}, {L, C}) when L > L0 -> [binary:copy(<<"\n">>, L - L0), binary:copy(<<" ">>, C - 1)];
gap({_, C0}, {_, C}) -> binary:copy(<<" ">>, max(0, C - C0)).

advance({L, C}, Text) ->
    case binary:split(Text, <<"\n">>, [global]) of
        [One] -> {L, C + string:length(One)};
        Parts -> {L + length(Parts) - 1, 1 + string:length(lists:last(Parts))}
    end.

class(T) ->
    case erl_scan:category(T) of
        atom -> <<"tok-atom">>;
        var -> <<"tok-var">>;
        string -> <<"tok-str">>;
        char -> <<"tok-str">>;
        integer -> <<"tok-num">>;
        float -> <<"tok-num">>;
        comment -> <<"tok-comment">>;
        C when is_atom(C) ->
            case erl_scan:reserved_word(C) of
                true -> <<"tok-kw">>;
                false -> none
            end
    end.
