%% @doc The file-manager example: folder and file cards with server-side
%% search/filter, list/grid view switching, starring and an upload dialog.
-module(aihtml_example_file_manager).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([page/0, action/4]).

-spec page() -> aihtml:html().
page() ->
    Head = ah_div([ah_div([ah_h1(<<"File manager"/utf8>>,
                                 [<<"text-3xl font-bold tracking-tight">>], []),
                           ah_p(<<"Organize folders, preview files and upload new assets."/utf8>>,
                                [<<"mt-2 text-sm text-muted">>], [])],
                          [<<"min-w-0">>], []),
                   ah_button(<<"Upload"/utf8>>, undefined, [primary, sm],
                             opens({id, <<"upload-window">>}))],
                  [<<"flex flex-wrap items-end justify-between gap-4 pt-6">>], []),
    aihtml_example_app_shell:page(files, Head,
                                  [toolbar(), folders(), file_section(grid, <<>>, all, all),
                                   upload_window()]).

toolbar() ->
    Include = #{include => [{id, file_search}, {id, type_filter}, {id, date_filter}]},
    ah_card([ah_div([ah_input(<<>>, [sm, <<"max-w-xs">>],
                              [{id, file_search}, {name, query}, {placeholder, <<"Search files…"/utf8>>},
                               {autocomplete, off},
                               on(input, {?MODULE, filter, #{kind => query}},
                                   Include#{debounce => 150})]),
                     ah_dropdownlist([{all, <<"All type"/utf8>>},
                                      {pdf, <<"PDF"/utf8>>}, {image, <<"Image"/utf8>>},
                                      {audio, <<"Audio"/utf8>>}, {video, <<"Video"/utf8>>}],
                                     all, [], [{id, type_filter}, {name, type},
                                               on(change, {?MODULE, filter, #{kind => type}}, Include)]),
                     ah_dropdownlist([{all, <<"All time"/utf8>>},
                                      {today, <<"Today"/utf8>>}, {week, <<"Last 7 days"/utf8>>},
                                      {month, <<"This month"/utf8>>}],
                                     all, [], [{id, date_filter}, {name, date},
                                               on(change, {?MODULE, filter, #{kind => date}}, Include)]),
                     ah_div([view_button(grid), view_button(list)],
                            [<<"ml-auto flex rounded-control border border-line p-1">>], [])],
                    [<<"flex flex-wrap items-center gap-3">>], [])],
            [], []).

view_button(View) ->
    Active = View =:= grid,
    ah_button(atom_to_binary(View), set_view,
              [sm, borderless,
               [<<"bg-primary-lighter font-semibold text-primary-dark">> || Active]],
              [on(click, {?MODULE, set_view, #{view => View}},
                  #{include => [{id, file_search}, {id, type_filter}, {id, date_filter}]})]).

folders() ->
    Folders = [{<<"Docs"/utf8>>, <<"2.4 GB"/utf8>>, <<"128 files"/utf8>>, primary, true},
               {<<"Foods"/utf8>>, <<"1.1 GB"/utf8>>, <<"56 files"/utf8>>, success, false},
               {<<"Projects"/utf8>>, <<"6.8 GB"/utf8>>, <<"312 files"/utf8>>, info, true},
               {<<"Sport"/utf8>>, <<"820 MB"/utf8>>, <<"24 files"/utf8>>, warning, false},
               {<<"Training"/utf8>>, <<"4.3 GB"/utf8>>, <<"78 files"/utf8>>, error, false},
               {<<"Work"/utf8>>, <<"9.6 GB"/utf8>>, <<"502 files"/utf8>>, primary, false}],
    ah_expander(ah_div([folder_card(F) || F <- Folders],
                       [<<"grid grid-cols-1 gap-4 sm:grid-cols-2 xl:grid-cols-3">>], []),
                [], [{header, ah_h2(<<"Folders"/utf8>>, [<<"text-lg font-semibold">>], [])},
                     {expanded, true}, {animation, none}, {id, folders_section}]).

folder_card({Name, Size, Count, Color, Starred}) ->
    ah_div([ah_div([ah_span(binary:part(Name, 0, 1), [<<"text-lg font-black text-primary">>], []),
                    star_button([<<"folder-">>, Name], Starred)],
                   [<<"flex items-start justify-between">>], []),
            ah_div([ah_p(Name, [<<"mt-3 truncate text-base font-semibold">>], []),
                    ah_p([Size, <<" · "/utf8>>, Count], [<<"text-xs text-muted">>], [])],
                   [], []),
            ah_div([#ah_avatar{body = <<"A"/utf8>>, size = sm, color = Color},
                    #ah_avatar{body = <<"D"/utf8>>, size = sm, color = info},
                    #ah_avatar{body = <<"+4">>, size = sm, color = success}],
                   [<<"mt-4 flex -space-x-2">>], [])],
           [<<"rounded-surface border border-line bg-surface p-4 hover:shadow-sm">>], []).

file_section(View, Query, Type, Date) ->
    Files = filtered(Query, Type, Date),
    Body = case View of
               list -> file_rows(Files);
               _ -> ah_div([file_card(F) || F <- Files],
                           [<<"grid grid-cols-1 gap-4 sm:grid-cols-2 xl:grid-cols-4">>], [])
           end,
    ah_expander(Body, [], [{header, ah_h2(<<"Files"/utf8>>, [<<"text-lg font-semibold">>], [])},
                           {expanded, true}, {animation, none}, {id, files_section}]).

filtered(Query, Type, Date) ->
    Q = string:lowercase(unicode:characters_to_binary(Query)),
    [F || F <- files(), matches_type(F, Type), matches_date(F, Date),
          string:find(string:lowercase(maps:get(name, F)), Q) =/= nomatch].

matches_type(_, all) -> true;
matches_type(#{kind := K}, K) -> true;
matches_type(_, _) -> false.

matches_date(_, all) -> true;
matches_date(#{recency := R}, R) -> true;
matches_date(_, _) -> false.

file_card(F) ->
    ah_div([ah_div([kind_badge(F), star_button(maps:get(id, F), maps:get(starred, F, false))],
                   [<<"flex items-start justify-between">>], []),
            ah_p(maps:get(name, F), [<<"mt-3 truncate text-sm font-semibold">>], []),
            ah_p([maps:get(size, F), <<" · "/utf8>>, maps:get(date, F)],
                 [<<"mt-1 text-xs text-muted">>], []),
            ah_div([#ah_avatar{body = maps:get(owner, F), size = sm, color = maps:get(color, F)},
                    ah_chip(string:titlecase(atom_to_binary(maps:get(kind, F))),
                            [maps:get(color, F), soft, small], [])],
                   [<<"mt-4 flex items-center justify-between">>], [])],
           [<<"rounded-surface border border-line bg-surface p-4 hover:shadow-sm">>], []).

file_rows(Files) ->
    Rows = [ah_tr([ah_td(kind_badge(F), [<<"w-16">>], []),
                   ah_td(maps:get(name, F), [<<"font-medium">>], []),
                   ah_td(maps:get(size, F), [], []),
                   ah_td(maps:get(date, F), [<<"text-muted">>], []),
                   ah_td(star_button(maps:get(id, F), maps:get(starred, F, false)), [], [])])
            || F <- Files],
    ah_table([ah_thead(ah_tr([ah_th(<<>>, [], []), ah_th(<<"Name"/utf8>>, [<<"text-left">>], []),
                              ah_th(<<"Size"/utf8>>, [<<"text-left">>], []),
                              ah_th(<<"Modified"/utf8>>, [<<"text-left">>], []),
                              ah_th(<<"Star"/utf8>>, [<<"text-right">>], [])])),
              ah_tbody(Rows)], [<<"w-full text-sm">>], []).

kind_badge(#{kind := Kind, color := Color}) ->
    ah_chip(string:titlecase(atom_to_binary(Kind)), [Color, soft, small], []).

star_button(Target, Starred) ->
    Id = [<<"star-">>, Target],
    ah_button(<<"★"/utf8>>, undefined,
              [borderless, sm, <<"px-2">>, [<<"text-warning">> || Starred]],
              [{id, Id}, {type, button}, {aria_label, <<"Star"/utf8>>},
               {aria_pressed, Starred andalso <<"true">>},
               on(click, {?MODULE, toggle_star,
                          #{target => Target, starred => Starred}})]).

upload_window() ->
    #ah_window{id = <<"upload-window">>, title = <<"Upload files"/utf8>>,
               body = ah_upload([], [],
                                [{url, <<"/upload">>}, {name, uploads},
                                 {drag_text, <<"Drag files here, or"/utf8>>},
                                 {browse_text, <<"browse"/utf8>>},
                                 {hint, <<"Any type, up to 5 MB each"/utf8>>}]),
               footer = ah_button(<<"Done"/utf8>>, undefined, [primary, sm],
                                  [closes({id, <<"upload-window">>})]),
               modal = true, draggable = false, resizable = false, width = 560}.

files() ->
    Data = [{<<"cover-12.jpg"/utf8>>, image, <<"18 MB"/utf8>>, <<"Today"/utf8>>, today,
             <<"AC"/utf8>>, info},
            {<<"design-suriname-2015.mp3"/utf8>>, audio, <<"7.3 MB"/utf8>>, <<"Today"/utf8>>, today,
             <<"MD"/utf8>>, primary},
            {<<"money-popup-crack.pdf"/utf8>>, pdf, <<"2.1 MB"/utf8>>, <<"Yesterday"/utf8>>, week,
             <<"LQ"/utf8>>, warning},
            {<<"roadmap-2026.pdf"/utf8>>, pdf, <<"4.8 MB"/utf8>>, <<"Yesterday"/utf8>>, week,
             <<"DA"/utf8>>, error},
            {<<"hero-animation.mp4"/utf8>>, video, <<"58 MB"/utf8>>, <<"3 days ago"/utf8>>, week,
             <<"AC"/utf8>>, success},
            {<<"team-photo.png"/utf8>>, image, <<"12 MB"/utf8>>, <<"3 days ago"/utf8>>, week,
             <<"SO"/utf8>>, info},
            {<<"quarterly-report.pdf"/utf8>>, pdf, <<"6.2 MB"/utf8>>, <<"Last week"/utf8>>, month,
             <<"DA"/utf8>>, primary},
            {<<"product-demo.mp4"/utf8>>, video, <<"124 MB"/utf8>>, <<"Last week"/utf8>>, month,
             <<"RS"/utf8>>, error},
            {<<"brand-guidelines.pdf"/utf8>>, pdf, <<"22 MB"/utf8>>, <<"Last week"/utf8>>, month,
             <<"MD"/utf8>>, warning},
            {<<"podcast-episode-18.mp3"/utf8>>, audio, <<"43 MB"/utf8>>, <<"This month"/utf8>>, month,
             <<"AC"/utf8>>, success},
            {<<"invoice-3412.pdf"/utf8>>, pdf, <<"1.1 MB"/utf8>>, <<"This month"/utf8>>, month,
             <<"LQ"/utf8>>, info},
            {<<"mockup-home.png"/utf8>>, image, <<"8.4 MB"/utf8>>, <<"This month"/utf8>>, month,
             <<"DA"/utf8>>, primary},
            {<<"kickoff-recording.mp4"/utf8>>, video, <<"244 MB"/utf8>>, <<"This month"/utf8>>, month,
             <<"SO"/utf8>>, error},
            {<<"interview-notes.pdf"/utf8>>, pdf, <<"760 KB"/utf8>>, <<"This month"/utf8>>, month,
             <<"RS"/utf8>>, success},
            {<<"cover-audio.mp3"/utf8>>, audio, <<"5.8 MB"/utf8>>, <<"This month"/utf8>>, month,
             <<"MD"/utf8>>, warning}],
    [#{id => integer_to_binary(I), name => Name, kind => Kind, size => Size, date => Date,
       recency => Recency, owner => Owner, color => Color, starred => I rem 5 =:= 1}
     || {I, {Name, Kind, Size, Date, Recency, Owner, Color}} <- lists:enumerate(1, Data)].

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), map(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(filter, #{kind := _}, #{values := Values}, Ctx) ->
    Query = maps:get(<<"query">>, Values, <<>>),
    Type = value(maps:get(<<"type">>, Values, <<"all">>)),
    Date = value(maps:get(<<"date">>, Values, <<"all">>)),
    aihtml_action:html(Ctx, {id, files_section}, file_section(grid, Query, Type, Date), outer);
action(set_view, #{view := View}, #{values := Values}, Ctx) ->
    Query = maps:get(<<"query">>, Values, <<>>),
    Type = value(maps:get(<<"type">>, Values, <<"all">>)),
    Date = value(maps:get(<<"date">>, Values, <<"all">>)),
    aihtml_action:html(Ctx, {id, files_section}, file_section(View, Query, Type, Date), outer);
action(toggle_star, #{target := Target, starred := Starred}, _Ev, Ctx) ->
    aihtml_action:html(Ctx, {id, [<<"star-">>, Target]},
                       star_button(Target, not Starred), outer),
    aihtml_toast:ah_toast(Ctx, <<"Updated favourites"/utf8>>, #{}).

value(<<"all">>) -> all;
value(<<"pdf">>) -> pdf;
value(<<"image">>) -> image;
value(<<"audio">>) -> audio;
value(<<"video">>) -> video;
value(<<"today">>) -> today;
value(<<"week">>) -> week;
value(<<"month">>) -> month;
value(V) when is_atom(V) -> V;
value(_) -> all.
