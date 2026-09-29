%% Element record of aihtml_upload (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_upload_tests checks
%% that they agree).
-ifndef(AIHTML_UPLOAD_HRL).
-define(AIHTML_UPLOAD_HRL, true).

-include("aihtml_element.hrl").

%% A drop zone with a file list. With `url' each file is POSTed there
%% (XHR, multipart field `field_name') and the JSON answers make up the
%% value; without it a native <input type=file name=Name> carries the
%% files in a normal form submit. `value' holds files uploaded earlier.
%% Postback fires on change (value: a JSON array of the files).
-record(ah_upload, {?AH_BASE(aihtml_upload),
                    value = [] :: [aihtml_upload:file()],
                    name = undefined :: undefined | atom() | iodata(),
                    disabled = false :: boolean(),
                    url = undefined :: undefined | iodata(),
                    field_name = <<"file">> :: atom() | iodata(),
                    accept = undefined :: undefined | iodata(),
                    multiple = true :: boolean(),
                    max_size = undefined :: undefined | pos_integer(),
                    max_count = undefined :: undefined | pos_integer(),
                    auto_upload = true :: boolean(),
                    drag_text = <<"Drag files here, or">> :: unicode:chardata(),
                    browse_text = <<"click to upload">> :: unicode:chardata(),
                    hint = undefined :: undefined | aihtml_html:html(),
                    show_file_list = true :: boolean(),
                    headers = #{} :: #{atom() | binary() => iodata()},
                    extra_data = #{} :: #{atom() | binary() => iodata() | number()},
                    with_credentials = false :: boolean(),
                    labels = #{} :: aihtml_upload:labels()}).

-endif.
