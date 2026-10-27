%% @doc The calendar application example: a real calendar widget plus an
%% upcoming-events rail and a modal form whose submit adds an event to
%% the calendar without reloading the page.
-module(aihtml_example_calendar_app).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([page/0, action/4]).

-spec page() -> aihtml:html().
page() ->
    Head = ah_div([ah_div([ah_h1(<<"Calendar"/utf8>>,
                                 [<<"text-3xl font-bold tracking-tight">>], []),
                           ah_p(<<"Plan events, meetings, and trips in one view."/utf8>>,
                                [<<"mt-2 text-sm text-muted">>], [])],
                          [<<"min-w-0">>], []),
                   ah_button(<<"New event"/utf8>>, undefined, [primary, sm],
                             opens({id, <<"event-window">>}))],
                  [<<"flex flex-wrap items-end justify-between gap-4 pt-6">>], []),
    aihtml_example_dashboard:shell(calendar_app, Head,
                                   [ah_div([calendar_rail(), calendar_canvas()],
                                           [<<"grid grid-cols-1 gap-6 xl:grid-cols-[20rem_minmax(0,1fr)]">>], []),
                                    event_window()]).

calendar_canvas() ->
    ah_card([ah_calendar(<<"2026-10-26">>, [editable, selectable],
                         [{id, <<"app-calendar">>}, {height, 660}, {first_day, 1},
                          {day_max_events, 3}, {agenda_days, 30}, {events, events()}])],
            [], []).

calendar_rail() ->
    ah_div([ah_card([calendars()], [],
                    [{title, <<"My calendars"/utf8>>}, {subtitle, <<"Five calendars"/utf8>>}]),
            ah_card([upcoming()], [], [{title, <<"Upcoming"/utf8>>},
                                       {subtitle, <<"Next 30 days"/utf8>>}])],
           [<<"flex flex-col gap-6">>], []).

calendars() ->
    ah_div([switch(<<"Personal"/utf8>>, primary, true),
            switch(<<"Work"/utf8>>, info, true),
            switch(<<"Family"/utf8>>, warning, true),
            switch(<<"Travel"/utf8>>, success, true),
            switch(<<"Birthdays"/utf8>>, error, false)],
           [<<"flex flex-col gap-3">>], []).

switch(Label, Color, Checked) ->
    ah_checkbox([ah_span(<<>>, [<<"mr-2 inline-block size-3 rounded-full">>, dot(Color)], []),
                  Label],
                 [<<"cal-", (atom_to_binary(Color))/binary>>, <<"show">>], [<<"flex-1">>],
                 [{checked, Checked}]).

upcoming() ->
    Items = [{<<"Today"/utf8>>, <<"10:30"/utf8>>, <<"Product review"/utf8>>, primary},
             {<<"Tomorrow"/utf8>>, <<"14:00"/utf8>>, <<"Design handoff"/utf8>>, info},
             {<<"Oct 29"/utf8>>, <<"09:00"/utf8>>, <<"Quarterly planning"/utf8>>, warning},
             {<<"Nov 03"/utf8>>, <<"18:30"/utf8>>, <<"Dentist"/utf8>>, error},
             {<<"Nov 07"/utf8>>, <<"All day"/utf8>>, <<"Team offsite"/utf8>>, success}],
    ah_div([ah_div([ah_div([ah_p(Day, [<<"text-xs font-bold text-muted">>], []),
                            ah_p([Time, <<" · "/utf8>>, Title],
                                 [<<"mt-1 truncate text-sm font-semibold">>], [])],
                           [<<"min-w-0 flex-1">>], []),
                    ah_span(<<>>, [<<"mt-1 size-2.5 shrink-0 rounded-full">>, dot(Color)], [])],
                   [<<"flex items-start justify-between gap-3 border-b border-line py-3 last:border-0">>], [])
            || {Day, Time, Title, Color} <- Items],
           [<<"flex flex-col">>], []).

event_window() ->
    Fields = ah_div([ah_field(<<"Title">>,
                              ah_input(<<>>, [], [{name, title}, {placeholder, <<"Product review">>},
                                                  {autocomplete, off}]),
                              [top], [{for, title}]),
                     ah_div([ah_field(<<"Starts">>,
                                      ah_datetime_input(<<"2026-10-26 10:00">>, [show_time],
                                                        [{name, starts_at},
                                                         {format, <<"yyyy-MM-dd HH:mm">>}]),
                                      [top], []),
                             ah_field(<<"Ends">>,
                                      ah_datetime_input(<<"2026-10-26 11:00">>, [show_time],
                                                        [{name, ends_at},
                                                         {format, <<"yyyy-MM-dd HH:mm">>}]),
                                      [top], [])],
                            [<<"grid gap-4 sm:grid-cols-2">>], []),
                     ah_checkbox(<<"All day"/utf8>>, all_day, [], []),
                     ah_field(<<"Calendar"/utf8>>,
                              ah_div([swatch(<<"#6366f1">>, primary, true),
                                      swatch(<<"#0ea5e9">>, info, false),
                                      swatch(<<"#f59e0b">>, warning, false),
                                      swatch(<<"#22c55e">>, success, false),
                                      swatch(<<"#ef4444">>, error, false)],
                                     [<<"flex flex-wrap gap-3">>], []),
                              [top, <<"mt-2">>], [])],
                    [<<"flex flex-col gap-4">>], []),
    ah_form(#ah_window{id = <<"event-window">>, title = <<"New event"/utf8>>, body = Fields,
                        footer = ah_div([ah_button(<<"Cancel"/utf8>>, undefined, [borderless, sm],
                                                   [closes({id, <<"event-window">>})]),
                                         ah_button(<<"Create event"/utf8>>, create_event, [primary, sm],
                                                   [{type, submit}])],
                                        [<<"flex justify-end gap-2">>], []),
                        modal = true, draggable = false, resizable = false, width = 520},
           [], [on(submit, {?MODULE, create_event, #{}})]).

swatch(Hex, Value, Checked) ->
    ah_radiobutton(ah_span(<<>>, [<<"block size-6 rounded-full border-2 border-white shadow">>],
                              [{style, [<<"background-color: ">>, Hex]}]),
                   Value, [], [{name, color}, {title, Hex}, {checked, Checked}]).

events() ->
    [#{id => <<"review">>, title => <<"Product review"/utf8>>, start => <<"2026-10-26T10:30">>,
       'end' => <<"2026-10-26T11:30">>, color => <<"#6366f1">>},
     #{id => <<"handoff">>, title => <<"Design handoff"/utf8>>, start => <<"2026-10-27T14:00">>,
       'end' => <<"2026-10-27T15:30">>, color => <<"#0ea5e9">>},
     #{id => <<"planning">>, title => <<"Quarterly planning"/utf8>>,
       start => <<"2026-10-29T09:00">>, 'end' => <<"2026-10-29T12:00">>,
       color => <<"#f59e0b">>},
     #{id => <<"trip">>, title => <<"Team offsite"/utf8>>, start => <<"2026-11-07">>,
       'end' => <<"2026-11-09">>, color => <<"#22c55e">>},
     #{id => <<"birthday">>, title => <<"Ana's birthday"/utf8>>, start => <<"2026-11-12">>,
       all_day => true, color => <<"#ef4444">>}].

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), map(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(create_event, _, #{form := Form}, Ctx) ->
    case string:trim(maps:get(<<"title">>, Form, <<>>)) of
        <<>> ->
            aihtml_action:focus(Ctx, {name, title});
        Title ->
            Id = <<"event-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
            Event = #{id => Id,
                      title => unicode:characters_to_binary(Title),
                      start => iso_time(maps:get(<<"starts_at">>, Form, <<>>)),
                      'end' => iso_time(maps:get(<<"ends_at">>, Form, <<>>)),
                      all_day => truthy(maps:get(<<"all_day">>, Form, false)),
                      color => color(maps:get(<<"color">>, Form, primary))},
            add_event(Ctx, {id, app_calendar}, Event),
            aihtml_lib_overlay:close(Ctx, {id, <<"event-window">>}),
            aihtml_action:set_value(Ctx, {name, title}, <<>>)
    end.

truthy(true) -> true;
truthy(<<"true">>) -> true;
truthy(_) -> false.

iso_time(Value) ->
    Binary = unicode:characters_to_binary(Value),
    unicode:characters_to_binary(string:replace(Binary, <<" ">>, <<"T">>)).

color(primary) -> <<"#6366f1">>;
color(info) -> <<"#0ea5e9">>;
color(warning) -> <<"#f59e0b">>;
color(success) -> <<"#22c55e">>;
color(error) -> <<"#ef4444">>;
color(_) -> <<"#6366f1">>.

dot(primary) -> <<" bg-primary">>;
dot(info) -> <<" bg-info">>;
dot(warning) -> <<" bg-warning">>;
dot(success) -> <<" bg-success">>;
dot(error) -> <<" bg-error">>.
