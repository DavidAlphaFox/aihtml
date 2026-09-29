%%%-------------------------------------------------------------------
%%% @doc An on/off switch ported from sigil: a real, visually hidden
%%% `<input type="checkbox" role="switch">' in a `<label>' with sigil's
%%% track and thumb. `Attrs' go to that input.
%%%
%%% switch_button/4 builds an #ah_switch_button{}
%%% (include/aihtml_switch_button.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_switch_button).
-behaviour(aihtml_element).

-include("aihtml_switch_button.hrl").

-export([switch_button/4, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_choice).

%% @doc A switch: a native checkbox with `role="switch"'. Options:
%% `on_label', `off_label' (text inside the track), `locked', and sigil's
%% `width', `height', `thumb_size' (px) for a custom size.
-spec switch_button(aihtml_html:html(), aihtml_lib_choice:value() | undefined,
                    aihtml_html:css(), aihtml_html:attrs()) -> #ah_switch_button{}.
switch_button(Content, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_switch_button{body = Content, value = Value}, Css, Attrs).

%% @doc The field names of #ah_switch_button{}.
-spec fields(atom()) -> [atom()].
fields(ah_switch_button) -> record_info(fields, ah_switch_button).

%% `id', the postback and `attrs' go on the native input.
-spec render(#ah_switch_button{}) -> aihtml_html:html().
render(#ah_switch_button{body = Content, value = Value, checked = C, disabled = D} = R) ->
    {Checked, Disabled, InputAttrs} =
        ?L:single_input(R#ah_switch_button.attrs, C, D, R#ah_switch_button{attrs = []}),
    {TrackStyle, ThumbStyle} = switch_style(R#ah_switch_button.width,
                                            R#ah_switch_button.height,
                                            R#ah_switch_button.thumb_size),
    Track = ?H:el(span,
        [?L:label_span(<<"ah-switch-label ah-switch-label-on">>, R#ah_switch_button.on_label),
         ?L:label_span(<<"ah-switch-label ah-switch-label-off">>, R#ah_switch_button.off_label),
         ?H:el(span, [], [<<"ah-switch-thumb">>], [{style, ThumbStyle}])],
        [<<"ah-switch-track">>], [{style, TrackStyle}, {aria_hidden, <<"true">>}]),
    ?H:el(label,
        [?L:input(checkbox, Value, InputAttrs, [{role, switch}]), Track,
         ?L:label_span(<<"ah-switch-text">>, Content)],
        [?E:classes(?MODULE, R),
         ?L:state(Checked, <<"ah-switch-on">>),
         ?L:state(Disabled, <<"ah-switch-disabled">>)],
        [{data_ah, <<"switch-button">>},
         {data_ah_locked, ?L:truthy(R#ah_switch_button.locked)}]).

%% sigil: thumb = height - 4, travel = thumb - width + 4.
switch_style(Width, Height, ThumbSize) ->
    case {Width, Height, ThumbSize} of
        {undefined, undefined, undefined} -> {undefined, undefined};
        {W0, H0, T0} ->
            W = default_int(W0, 50), H = default_int(H0, 24),
            T = default_int(T0, H - 4),
            {iolist_to_binary(io_lib:format("width:~bpx;height:~bpx;--sw-travel:~bpx;",
                                            [W, H, T - W + 4])),
             iolist_to_binary(io_lib:format("width:~bpx;height:~bpx;", [T, T]))}
    end.

default_int(undefined, D) -> D;
default_int(N, _) when is_integer(N), N > 0 -> N;
default_int(N, _) -> error({aihtml, {bad_option, switch_button, N}}).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => switch_button, category => form,
       signature => <<"switch_button(Content, Value, Css, Attrs)">>,
       root => <<"ah-switch">>, groups => #{size => {[sm, md, lg], none}},
       options => [on_label, off_label, locked, width, height, thumb_size],
       behavior => <<"switch-button">>, events => [<<"change">>, <<"input">>],
       doc => <<"On/off switch: a native checkbox with role=switch; Attrs go to the "
                "<input>. Options: on_label, off_label, locked, width, height, "
                "thumb_size.">>,
       option_docs =>
           #{sm => <<"36 x 20 track.">>, md => <<"50 x 24 track (default).">>,
             lg => <<"60 x 30 track.">>,
             on_label => <<"Text in the track while on.">>,
             off_label => <<"Text in the track while off.">>,
             locked => <<"true: focusable but the user cannot toggle it.">>,
             width => <<"Track width in px (default 50).">>,
             height => <<"Track height in px (default 24).">>,
             thumb_size => <<"Thumb size in px (default height - 4).">>},
       methods => [?L:set_checked(<<"true | false">>), ?L:get_value(<<"true | false">>),
                   ?L:set_disabled()]}].
