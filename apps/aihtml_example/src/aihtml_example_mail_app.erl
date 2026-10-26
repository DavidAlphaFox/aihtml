%% @doc A three-column mail application. Opening a message, changing a
%% folder, starring mail and sending a short draft are one stateless
%% postback each; the response morphs the affected server-rendered HTML.
-module(aihtml_example_mail_app).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([page/0, action/4]).

-spec page() -> aihtml:html().
page() ->
    Selected = <<"m1">>,
    Head = ah_div([ah_div([ah_h1(<<"Mail"/utf8>>, [<<"text-3xl font-bold tracking-tight">>], []),
                           ah_p(<<"A focused inbox with folders, reading pane and quick actions."/utf8>>,
                                [<<"mt-2 text-sm text-muted">>], [])],
                          [<<"min-w-0">>], []),
                   ah_button(<<"Compose"/utf8>>, undefined, [primary, sm],
                             opens({id, <<"compose-window">>}))],
                  [<<"flex flex-wrap items-end justify-between gap-4 pt-6">>], []),
    Body = [ah_div([folders(inbox), email_list(inbox, Selected), reader(email(Selected), Selected)],
                   [<<"grid grid-cols-1 gap-6 items-start 2xl:grid-cols-[16rem_minmax(20rem,1fr)_minmax(24rem,1.1fr)]">>],
                   []),
            compose_window()],
    aihtml_example_app_shell:page(mail, Head, Body).

folders(Active) ->
    Items = [{inbox, <<"Inbox"/utf8>>}, {sent, <<"Sent"/utf8>>}, {drafts, <<"Drafts"/utf8>>},
             {starred, <<"Starred"/utf8>>}, {important, <<"Important"/utf8>>},
             {archive, <<"Archive"/utf8>>}],
    ah_card([ah_div([folder_button(Key, Label, count(Key), Key =:= Active) || {Key, Label} <- Items],
                   [<<"flex flex-col gap-1">>], [])],
            [], [{id, mail_folders}, {title, <<"Folders"/utf8>>}]).

folder_button(Key, Label, Count, Active) ->
    ah_button([ah_span(Label, [<<"flex-1 text-left">>], []),
               ah_span(integer_to_binary(Count), [<<"text-xs font-bold text-muted">>], [])],
              open_folder, [sm, borderless, <<"w-full justify-start">>,
                            [<<"bg-primary-lighter font-semibold text-primary-dark">> || Active]],
              [on(click, {?MODULE, open_folder, #{folder => Key}})]).

count(Folder) ->
    length([E || E <- emails(), folder(E) =:= Folder, not maps:get(read, E, false)]).

email_list(Folder, Selected) ->
    Items = [E || E <- emails(), matches(Folder, E)],
    ah_card([ah_div([email_row(E, Selected) || E <- Items], [<<"flex flex-col">>], [])],
            [], [{id, mail_list}, {title, folder_title(Folder)},
                 {subtitle, [integer_to_binary(length(Items)), <<" messages"/utf8>>]}]).

matches(inbox, E) -> folder(E) =:= inbox;
matches(starred, E) -> maps:get(starred, E, false);
matches(important, E) -> maps:get(important, E, false);
matches(Folder, E) -> folder(E) =:= Folder.

folder_title(starred) -> <<"Starred"/utf8>>;
folder_title(important) -> <<"Important"/utf8>>;
folder_title(Folder) -> string:titlecase(atom_to_binary(Folder)).

email_row(E, Selected) ->
    Id = maps:get(id, E),
    Unread = not maps:get(read, E, false),
    ah_div([ah_div([star_button(E, Selected),
                    #ah_avatar{body = maps:get(initials, E), size = sm, shape = rounded,
                               color = maps:get(color, E)}],
                   [<<"flex items-center gap-2">>], []),
            ah_div([ah_div([ah_span(maps:get(name, E), [<<"truncate text-sm font-semibold">>], []),
                            ah_span(maps:get(time, E), [<<"shrink-0 text-xs text-muted">>], [])],
                           [<<"flex items-center justify-between gap-2">>], []),
                    ah_p(maps:get(subject, E),
                         [<<"mt-1 truncate text-sm font-semibold">>,
                          [<<"text-primary">> || Unread]], []),
                    ah_p(maps:get(preview, E), [<<"mt-0.5 truncate text-xs text-muted">>], [])],
                   [<<"min-w-0 flex-1">>], [])],
           [<<"flex cursor-pointer items-start gap-3 rounded-surface p-3 hover:bg-surface-2">>,
            [<<"bg-primary-lighter">> || Id =:= Selected]],
           [{id, [<<"mail-item-">>, Id]},
            on(click, {?MODULE, open_email, #{id => Id, selected => Selected}})]).

star_button(E, Selected) ->
    On = maps:get(starred, E, false),
    Id = maps:get(id, E),
    ah_button(<<"★"/utf8>>, undefined,
              [borderless, sm, <<"px-2">>, [<<"text-warning">> || On]],
              [{id, [<<"mail-star-">>, Id]}, {type, button}, {aria_label, <<"Star"/utf8>>},
               {aria_pressed, On andalso <<"true">>},
               on(click, {?MODULE, toggle_star, #{id => maps:get(id, E), selected => Selected}})]).

reader(E, Selected) ->
    Toolbar = [ah_button(<<"Archive"/utf8>>, undefined, [borderless, sm],
                         [on(click, {?MODULE, archive_email, #{id => maps:get(id, E)}})]),
               ah_button(<<"Reply"/utf8>>, undefined, [borderless, sm],
                         [on(click, {?MODULE, focus_reply, #{}})]),
               ah_button(<<"Forward"/utf8>>, undefined, [borderless, sm], [{type, button}]),
               star_button(E, Selected)],
    ah_card([ah_div([#ah_avatar{body = maps:get(initials, E), size = lg, shape = rounded,
                                color = maps:get(color, E)},
                     ah_div([ah_p(maps:get(name, E), [<<"text-base font-semibold">>], []),
                             ah_p(maps:get(email, E), [<<"text-xs text-muted">>], [])],
                            [<<"min-w-0 flex-1">>], []),
                     ah_span(maps:get(time, E), [<<"text-xs text-muted">>], []),
                     ah_div(Toolbar, [<<"flex items-center gap-1">>], [])],
                    [<<"flex flex-wrap items-center gap-3 border-b border-line pb-4">>], []),
             ah_h2(maps:get(subject, E), [<<"mt-5 text-xl font-bold">>], []),
             ah_div([ah_p(Para, [<<"text-sm leading-6 text-muted">>], []) || Para <- maps:get(body, E)],
                    [<<"mt-4 flex flex-col gap-3">>], []),
             reply_box()],
            [], [{id, mail_reader}, {title, <<"Message"/utf8>>}]).

reply_box() ->
    ah_form(ah_textarea(<<>>, [], [{name, reply}, {placeholder, <<"Write a reply…"/utf8>>},
                                   {rows, 4}]),
            [<<"mt-6">>], [on(submit, {?MODULE, send_reply, #{}})]).

compose_window() ->
    Body = ah_div([ah_input(<<>>, [], [{name, to}, {placeholder, <<"To"/utf8>>},
                                       {autocomplete, off}]),
                   ah_input(<<>>, [], [{name, subject}, {placeholder, <<"Subject"/utf8>>},
                                       {autocomplete, off}]),
                   ah_textarea(<<>>, [], [{name, body}, {rows, 6},
                                          {placeholder, <<"Write your message…"/utf8>>}])],
                  [<<"flex flex-col gap-3">>], []),
    ah_form(#ah_window{id = <<"compose-window">>, title = <<"New message"/utf8>>, body = Body,
                       footer = ah_div([ah_button(<<"Cancel"/utf8>>, undefined, [borderless, sm],
                                                  [closes({id, <<"compose-window">>})]),
                                        ah_button(<<"Send"/utf8>>, send_email, [primary, sm],
                                                  [{type, submit}])],
                                       [<<"flex justify-end gap-2">>], []),
                       modal = true, draggable = false, resizable = false, width = 520},
           [], [on(submit, {?MODULE, send_email, #{}})]).

email(Id) ->
    hd([E || E <- emails(), maps:get(id, E) =:= Id]).

first_id(Folder) ->
    case [maps:get(id, E) || E <- emails(), matches(Folder, E)] of
        [Id | _] -> Id;
        [] -> undefined
    end.

emails() ->
    [#{id => <<"m1">>, name => <<"Lara Quinn"/utf8>>, email => <<"lara@ailink.io"/utf8>>,
      initials => <<"LQ"/utf8>>, color => success, folder => inbox, read => false,
      starred => true, important => true, time => <<"12 min"/utf8>>,
      subject => <<"Q3 launch checklist"/utf8>>,
      preview => <<"Thanks — let's lock the dates by Friday."/utf8>>,
      body => [<<"Hi David,"/utf8>>,
               <<"The launch checklist is ready. We still need a final pass on pricing and "
                 "the announcement post before we can lock Friday."/utf8>>]},
     #{id => <<"m2">>, name => <<"Marco Diaz"/utf8>>, email => <<"marco@designlab.co"/utf8>>,
      initials => <<"MD"/utf8>>, color => info, folder => inbox, read => false,
      starred => false, important => true, time => <<"1 h"/utf8>>,
      subject => <<"Design review for billing"/utf8>>,
      preview => <<"Pushed v3 of the layout for review."/utf8>>,
      body => [<<"The third layout resolves the long invoice numbers and gives the mobile "
                 "summary its own section."/utf8>>]},
     #{id => <<"m3">>, name => <<"Sienna Ohira"/utf8>>, email => <<"sienna@northwind.com"/utf8>>,
      initials => <<"SO"/utf8>>, color => primary, folder => inbox, read => true,
      starred => false, important => false, time => <<"3 h"/utf8>>,
      subject => <<"Customer feedback summary"/utf8>>,
      preview => <<"Dark mode is the most requested change."/utf8>>,
      body => [<<"Across 82 interviews, dark mode, faster search and clearer errors came up most."/utf8>>]},
     #{id => <<"m4">>, name => <<"Roman Schultz"/utf8>>, email => <<"roman@ops.dev"/utf8>>,
      initials => <<"RS"/utf8>>, color => warning, folder => archive, read => true,
      starred => false, important => false, time => <<"Yesterday"/utf8>>,
      subject => <<"Database migration window"/utf8>>,
      preview => <<"Ops scheduled it for Saturday at 02:00."/utf8>>,
      body => [<<"The maintenance window is two hours, with a rollback plan attached."/utf8>>]},
     #{id => <<"m5">>, name => <<"Aimee Chen"/utf8>>, email => <<"aimee@studio.sg"/utf8>>,
      initials => <<"AC"/utf8>>, color => error, folder => drafts, read => true,
      starred => false, important => false, time => <<"Mon"/utf8>>,
      subject => <<"Contract redlines"/utf8>>,
      preview => <<"I marked the two clauses we should discuss."/utf8>>,
      body => [<<"Two clauses remain open: data ownership and the renewal notice period."/utf8>>]}].

folder(#{folder := F}) -> F.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), map(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(open_email, #{id := Id, selected := Old}, _Ev, Ctx) ->
    E = email(Id),
    Read = E#{read => true},
    aihtml_action:html(Ctx, {id, [<<"mail-item-">>, Old]}, email_row(email(Old), Old), outer),
    aihtml_action:html(Ctx, {id, [<<"mail-item-">>, Id]}, email_row(Read, Id), outer),
    aihtml_action:html(Ctx, {id, mail_reader}, reader(Read, Id), outer);
action(open_folder, #{folder := Folder}, _Ev, Ctx) ->
    Selected = first_id(Folder),
    aihtml_action:html(Ctx, {id, mail_folders}, folders(Folder), outer),
    aihtml_action:html(Ctx, {id, mail_list}, email_list(Folder, Selected), outer),
    aihtml_action:html(Ctx, {id, mail_reader}, reader_for(Selected), outer);
action(toggle_star, #{id := Id}, _Ev, Ctx) ->
    E = email(Id),
    Updated = E#{starred => not maps:get(starred, E, false)},
    aihtml_action:html(Ctx, {id, [<<"mail-star-">>, Id]}, star_button(Updated, Id), outer);
action(archive_email, #{id := Id}, _Ev, Ctx) ->
    aihtml_action:html(Ctx, {id, [<<"mail-item-">>, Id]}, <<>>, outer),
    aihtml_action:html(Ctx, {id, mail_reader}, empty_reader(), outer),
    aihtml_toast:ah_toast(Ctx, <<"Message archived"/utf8>>, #{});
action(focus_reply, _, _Ev, Ctx) ->
    aihtml_action:focus(Ctx, {name, reply});
action(send_reply, _, #{form := Form}, Ctx) ->
    case string:trim(maps:get(<<"reply">>, Form, <<>>)) of
        <<>> -> aihtml_action:focus(Ctx, {name, reply});
        _ -> aihtml_toast:ah_toast(Ctx, <<"Reply saved"/utf8>>, #{variant => success}),
             aihtml_action:set_value(Ctx, {name, reply}, <<>>)
    end;
action(send_email, _, #{form := Form}, Ctx) ->
    case maps:get(<<"to">>, Form, <<>>) of
        <<>> -> aihtml_action:focus(Ctx, {name, to});
        _ -> aihtml_lib_overlay:close(Ctx, {id, <<"compose-window">>}),
             aihtml_toast:ah_toast(Ctx, <<"Message sent"/utf8>>, #{variant => success})
    end.

empty_reader() ->
    ah_card([ah_p(<<"Select a message to read it."/utf8>>, [<<"text-sm text-muted">>], [])],
            [], [{id, mail_reader}, {title, <<"Reading pane"/utf8>>}]).

reader_for(undefined) -> empty_reader();
reader_for(Id) -> reader(email(Id), Id).
