%%%-------------------------------------------------------------------
%%% @doc File upload, ported from sigil (form/upload and its core,
%%% drop_zone and picker helpers). See designs/04-components.md.
%%%
%%%   upload(Value, Css, Attrs)   a drop zone, a file picker and a file list
%%%   uploaded_files(Event)       (in an action) the files of a change event
%%%
%%% Two transports:
%%%
%%% == With `url' ==
%%%
%%% Actions are JSON POSTs, so the bytes go elsewhere: every accepted file
%%% is POSTed by the browser to `url' (XMLHttpRequest, multipart/form-data,
%%% the file in field `field_name', plus `extra_data' fields, `headers'),
%%% one request per file, with a progress bar. The server answers each
%%% with JSON, typically `{"name": ..., "size": ..., "id": ...}'. The
%%% component's value is the list of these answers (plus the files given
%%% in `Value'): `data-ah-value' holds it as a JSON array, a hidden input
%%% carries it when `name' is set, and `change' fires on the root each
%%% time a file finishes or an uploaded file is removed, so
%%% `on(change, ...)' / `postback' reach an ordinary action, where
%%% `uploaded_files(Event)' decodes the list. A non-2xx answer marks the
%%% file as failed; its JSON `error' (or `message') is shown.
%%%
%%% == Without `url' ==
%%%
%%% The component is a styled `<input type="file" name=Name>': dropped and
%%% picked files are put into that input, and a normal multipart form
%%% submit sends them. The value (and `change') lists the selected files
%%% as `{"name", "size", "type"}'.
%%%
%%% Files are checked in the browser against `accept', `max_size' (bytes)
%%% and `max_count'; a rejected file shows in the list as an error and
%%% fires `ah:upload-error'. The list rows are built from the shared
%%% template templates/upload_item.mustache on both sides.
%%%
%%% The component function builds an #ah_upload{} record (include/
%%% aihtml_form_upload.hrl); render/1 turns it into HTML.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_form_upload).
-behaviour(aihtml_element).

-include("aihtml_form_upload.hrl").

-export([upload/3, uploaded_files/1, render/1, fields/1, catalog/0, facade_extras/0,
         format_size/1]).

-export_type([file/0, element/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% Shared template (see aihtml_tpl): also compiled to AH.tpl.upload_item.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_upload_item, "../templates/upload_item.mustache"}).

-type file() :: ah_upl_file().
-type element() :: #ah_upload{}.

-define(LABELS, #{remove => <<"Remove">>,
                  upload_failed => <<"Upload failed">>,
                  type_mismatch => <<"File type not accepted">>,
                  too_large => <<"File too large">>,
                  too_many => <<"Too many files">>,
                  network_error => <<"Network error">>}).

%%%===================================================================
%%% upload
%%%===================================================================

%% @doc A drop zone with a file list. `Value' is the list of files
%% already uploaded (names or maps as the upload URL answered them), may
%% be `[]'.
%%
%% Css: `disabled'. Options (in Attrs): `url' (where files are POSTed;
%% without it the component is a native file input), `field_name'
%% (multipart field, default "file"), `accept' (as the input attribute,
%% e.g. "image/*,.pdf"), `multiple' (default true), `max_size' (bytes),
%% `max_count', `auto_upload' (default true; false waits for the
%% `uploadAll' method), `drag_text', `browse_text', `hint',
%% `show_file_list' (default true), `headers', `extra_data' (maps),
%% `with_credentials', `labels' (a map of `remove', `upload_failed',
%% `type_mismatch', `too_large', `too_many', `network_error'). `name'
%% names the hidden input (with `url') or the file input (without).
-spec upload([file()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_upload{}.
upload(Value, Css, Attrs) when is_list(Value) ->
    build(#ah_upload{value = Value}, Css, Attrs).

render_upload(#ah_upload{value = Files, name = Name, disabled = Disabled, url = Url0} = R) ->
    Classes = classes(R),                       % checks the flag first
    is_list(Files) orelse error({aihtml, {bad_option, value, Files}}),
    Url = case Url0 of
              undefined -> undefined;
              _ -> nonempty(url, Url0)
          end,
    Field = nonempty(field_name, R#ah_upload.field_name),
    Multiple = bool(multiple, R#ah_upload.multiple),
    Auto = bool(auto_upload, R#ah_upload.auto_upload),
    ShowList = bool(show_file_list, R#ah_upload.show_file_list),
    Creds = bool(with_credentials, R#ah_upload.with_credentials),
    MaxSize = pos_opt(max_size, R#ah_upload.max_size),
    MaxCount = pos_opt(max_count, R#ah_upload.max_count),
    Headers = json_map(headers, R#ah_upload.headers),
    Extra = json_map(extra_data, R#ah_upload.extra_data),
    Labels = labels(R#ah_upload.labels),
    Accept = case R#ah_upload.accept of
                 undefined -> undefined;
                 A -> text(A)
             end,
    Json = encode(value, Files),
    Native = Url =:= undefined,
    Hint = R#ah_upload.hint,
    Dragger = ?H:el('div',
                    [?H:el('div', icon(), [<<"ah-upload-icon">>], [{aria_hidden, <<"true">>}]),
                     ?H:el('div', [?H:el(span, R#ah_upload.drag_text, [], []), <<" ">>,
                                   ?H:el(span, R#ah_upload.browse_text,
                                         [<<"ah-upload-browse">>], [])],
                           [<<"ah-upload-text">>], []),
                     [?H:el('div', Hint, [<<"ah-upload-hint">>], [])
                      || Hint =/= undefined, Hint =/= <<>>, Hint =/= []]],
                    [<<"ah-upload-dragger">>],
                    [{role, button}, {tabindex, case Disabled of true -> <<"-1">>; false -> <<"0">> end},
                     {aria_disabled, Disabled andalso <<"true">>}]),
    Input = ?H:void(input, [<<"ah-upload-input">>],
                    [{type, file}, {tabindex, <<"-1">>}, {aria_hidden, <<"true">>},
                     {name, case Native of true -> Name; false -> undefined end},
                     {accept, Accept}, {multiple, Multiple}, {disabled, Disabled}]),
    Hidden = [?H:void(input, [], [{type, hidden}, {name, Name}, {value, Json}])
              || not Native, Name =/= undefined],
    Items = [item_html(<<"s", (integer_to_binary(I))/binary>>, F, Labels, Disabled)
             || {I, F} <- lists:zip(lists:seq(0, length(Files) - 1), Files)],
    List = [?H:el('div', Items, [<<"ah-upload-list">>], [{aria_live, polite}]) || ShowList],
    ?H:el('div', [Dragger, Input, Hidden, List],
          [Classes],
          [[{data_ah, <<"upload">>}, {data_ah_value, Json},
            {data_ah_url, Url},
            {data_ah_field, Field},
            {data_ah_max_size, MaxSize}, {data_ah_max_count, MaxCount},
            {data_ah_manual, not Auto},
            {data_ah_headers, json_attr(headers, Headers)},
            {data_ah_extra, json_attr(extra_data, Extra)},
            {data_ah_credentials, Creds},
            {data_ah_labels, encode(labels, Labels)},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

%% sigil's upload arrow
icon() ->
    {safe, <<"<svg width=\"40\" height=\"40\" viewBox=\"0 0 24 24\" fill=\"none\" "
             "stroke=\"currentColor\" stroke-width=\"1.5\" stroke-linecap=\"round\" "
             "stroke-linejoin=\"round\"><path d=\"M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4\">"
             "</path><polyline points=\"17 8 12 3 7 8\"></polyline>"
             "<line x1=\"12\" y1=\"3\" x2=\"12\" y2=\"15\"></line></svg>">>}.

%% A row for a file uploaded earlier, from the template the browser uses.
item_html(Id, File, Labels, Disabled) ->
    {Name, Size, Type} = describe(File),
    aihtml_tpl:safe(tpl_upload_item(
                      #{cls => <<"ah-upload-item ah-upload-item-success">>, id => Id,
                        icon => icon_kind(Type), name => Name,
                        size => format_size(Size), uploading => false, percent => 100,
                        has_error => false, error => <<>>,
                        remove => maps:get(<<"remove">>, Labels), disabled => Disabled})).

describe(Name) when is_binary(Name) -> {Name, undefined, <<>>};
describe(M) ->
    is_map(M) orelse error({aihtml, {bad_upload_file, M}}),
    Get = fun(K) -> maps:get(K, M, maps:get(atom_to_binary(K), M, undefined)) end,
    Size = case Get(size) of
               S when is_integer(S), S >= 0 -> S;
               _ -> undefined
           end,
    Type = case Get(type) of
               undefined -> <<>>;
               T -> text(T)
           end,
    {case Get(name) of undefined -> <<>>; N -> text(N) end, Size, Type}.

icon_kind(<<"image/", _/binary>>) -> <<"image">>;
icon_kind(<<"video/", _/binary>>) -> <<"video">>;
icon_kind(<<"audio/", _/binary>>) -> <<"audio">>;
icon_kind(_) -> <<"file">>.

%% @doc A byte count as sigil shows it: "812 B", "1.2 KB", "3.0 MB"
%% (the browser formats new files the same way).
-spec format_size(undefined | non_neg_integer()) -> binary().
format_size(undefined) -> <<>>;
format_size(B) when B < 1024 -> <<(integer_to_binary(B))/binary, " B">>;
format_size(B) when B < 1024 * 1024 ->
    <<(float_to_binary(B / 1024, [{decimals, 1}]))/binary, " KB">>;
format_size(B) ->
    <<(float_to_binary(B / (1024 * 1024), [{decimals, 1}]))/binary, " MB">>.

%%%===================================================================
%%% In actions
%%%===================================================================

%% @doc The files of an upload's `change' event (or its `data-ah-value',
%% or the value of its hidden input in a form): the decoded JSON array,
%% each file a map with binary keys (or a name, for names given as
%% `Value').
-spec uploaded_files(aihtml_action:event() | binary()) -> [term()].
uploaded_files(#{value := V}) -> uploaded_files(V);
uploaded_files(null) -> [];
uploaded_files(undefined) -> [];
uploaded_files(<<>>) -> [];
uploaded_files(B) when is_binary(B) ->
    case json:decode(B) of
        L when is_list(L) -> L;
        Other -> error({aihtml, {bad_upload_value, Other}})
    end.

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{uploaded_files, 1}].

%%%===================================================================
%%% Records
%%%===================================================================

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_upload) -> record_info(fields, ah_upload).

-spec render(element()) -> aihtml_html:html().
render(#ah_upload{} = R) -> render_upload(R).

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

%%%===================================================================
%%% Option checks
%%%===================================================================

%% Records built by hand bypass the field types, hence the checks (written
%% as expressions: dialyzer trusts the field types).
bool(K, V) ->
    is_boolean(V) orelse error({aihtml, {bad_option, K, V}}),
    V.

pos_opt(K, V) ->
    (V =:= undefined orelse (is_integer(V) andalso V > 0))
        orelse error({aihtml, {bad_option, K, V}}),
    V.

nonempty(K, V) ->
    case text_or_error(K, V) of
        <<>> -> error({aihtml, {bad_option, K, V}});
        B -> B
    end.

text_or_error(K, V) ->
    try text(V) of
        B when is_binary(B) -> B
    catch
        _:_ -> error({aihtml, {bad_option, K, V}})
    end.

json_map(K, M) ->
    is_map(M) orelse error({aihtml, {bad_option, K, M}}),
    maps:from_list([{text_or_error(K, Key), json_value(K, Val)} || Key := Val <- M]).

json_value(_, N) when is_number(N) -> N;
json_value(K, V) -> text_or_error(K, V).

labels(M) ->
    is_map(M) orelse error({aihtml, {bad_option, labels, M}}),
    maps:fold(fun(K, V, Acc) ->
                      maps:is_key(K, ?LABELS) orelse error({aihtml, {bad_upload_label, K}}),
                      Acc#{atom_to_binary(K) => text_or_error(labels, V)}
              end,
              #{atom_to_binary(K) => V || K := V <- ?LABELS}, M).

json_attr(_, M) when map_size(M) =:= 0 -> undefined;
json_attr(K, M) -> encode(K, M).

encode(K, Term) ->
    try iolist_to_binary(json:encode(Term))
    catch _:_ -> error({aihtml, {bad_option, K, Term}})
    end.

text(B) when is_binary(B) -> B;
text(A) when is_atom(A) -> atom_to_binary(A);
text(I) when is_integer(I) -> integer_to_binary(I);
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error(badarg)
    end.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => upload, category => form,
       signature => <<"upload(Value, Css, Attrs)">>,
       root => <<"ah-upload">>,
       flags => [disabled],
       options => [url, field_name, accept, multiple, max_size, max_count, auto_upload,
                   drag_text, browse_text, hint, show_file_list, headers, extra_data,
                   with_credentials, labels],
       behavior => <<"upload">>,
       events => [<<"change">>, <<"ah:select">>, <<"ah:upload-progress">>,
                  <<"ah:upload-success">>, <<"ah:upload-error">>],
       doc => <<"A drag-and-drop zone with a file list: files go by XHR to a URL "
                "(progress, JSON answers as the value) or through a native file input.">>,
       option_docs =>
           #{disabled => <<"No picking, dropping or removing.">>,
             url => <<"Where each file is POSTed (multipart); the JSON answers make up the value. "
                      "Without it the files go with the form, through a native file input.">>,
             field_name => <<"Multipart field of the file (default \"file\").">>,
             accept => <<"Accepted types, as the input attribute: \"image/*,.pdf\".">>,
             multiple => <<"Several files (default true); false keeps the last one picked.">>,
             max_size => <<"Largest file in bytes; larger ones are rejected in the browser.">>,
             max_count => <<"Most files in the list.">>,
             auto_upload => <<"Upload as soon as files are added (default true); "
                              "false waits for the uploadAll method.">>,
             drag_text => <<"Drop zone text (default \"Drag files here, or\").">>,
             browse_text => <<"Highlighted link text (default \"click to upload\").">>,
             hint => <<"Small print under the text, e.g. the accepted types.">>,
             show_file_list => <<"Show the file list (default true).">>,
             headers => <<"Map of request headers of each upload.">>,
             extra_data => <<"Map of extra multipart fields sent with each file.">>,
             with_credentials => <<"Send cookies to a cross-origin url.">>,
             labels => <<"Map of remove, upload_failed, type_mismatch, too_large, too_many, "
                         "network_error.">>},
       methods =>
           [#{name => getValue, args => <<"()">>,
              doc => <<"Return the files of the value (parsed data-ah-value).">>},
            #{name => setValue, args => <<"([File])">>,
              doc => <<"Replace the uploaded files without firing change.">>},
            #{name => getFiles, args => <<"()">>,
              doc => <<"Return every row: {id, name, size, type, status, percent, value}.">>},
            #{name => uploadAll, args => <<"()">>,
              doc => <<"Upload the pending files (with auto_upload false).">>},
            #{name => upload, args => <<"(FileId)">>,
              doc => <<"Upload one pending file.">>},
            #{name => remove, args => <<"(FileId)">>,
              doc => <<"Remove a row, aborting its upload.">>},
            #{name => clear, args => <<"()">>,
              doc => <<"Abort the uploads, empty the list and fire change.">>},
            #{name => open, args => <<"()">>, doc => <<"Open the file dialog.">>},
            #{name => enable, args => <<"()">>, doc => <<"Enable the component.">>},
            #{name => disable, args => <<"()">>, doc => <<"Disable the component.">>}]}].
