%%%-------------------------------------------------------------------
%%% @doc The time_ago component (designs/04-components.md): `ah_time_ago/3'
%%% builds an #ah_time_ago{} element record (include/aihtml_time_ago.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%%
%%% Timestamps are Unix seconds (as returned by
%%% `erlang:system_time(second)'), a UTC `calendar:datetime()' or an
%%% RFC 3339 binary.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_time_ago).
-behaviour(aihtml_element).

-include("aihtml_time_ago.hrl").

-export([ah_time_ago/3, render/1, fields/1, catalog/0]).

-import(aihtml_lib_display, [to_bin/1, method/3]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Relative time ("3m ago") in sigil's format, rendered on the server
%% and kept current by the browser every 60 s. `Timestamp' is Unix
%% seconds, a UTC `calendar:datetime()' or an RFC 3339 binary.
%% Options: `now' (Unix seconds, for rendering), `labels' (map with
%% just_now, minutes, hours, days, months; "{n}" is the number),
%% `live' (true), `title' (true: absolute time on hover).
-spec ah_time_ago(integer() | calendar:datetime() | binary(), css(), attrs()) -> #ah_time_ago{}.
ah_time_ago(Timestamp, Css, Attrs) ->
    ?E:build(?MODULE, #ah_time_ago{timestamp = Timestamp}, Css, Attrs).

%% @doc The field names of #ah_time_ago{}.
-spec fields(atom()) -> [atom()].
fields(ah_time_ago) -> record_info(fields, ah_time_ago).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_time_ago{}) -> html().
render(#ah_time_ago{timestamp = Timestamp, labels = Custom} = R) ->
    Cls = ?E:classes(?MODULE, R),
    Secs = to_unix(Timestamp),
    Now = case R#ah_time_ago.now of
              undefined -> erlang:system_time(second);
              N -> N
          end,
    Labels = maps:merge(default_labels(), Custom),
    Iso = list_to_binary(calendar:system_time_to_rfc3339(Secs, [{offset, "Z"}])),
    Title = case R#ah_time_ago.title of
                false -> undefined;
                _ -> binary:replace(binary:replace(Iso, <<"T">>, <<" ">>), <<"Z">>, <<" UTC">>)
            end,
    LabelAttrs = [{<<"data-ah-label-", (dash(K))/binary>>, V}
                  || {K, V} <- lists:sort(maps:to_list(Custom))],
    ?H:el(time, format_ago(Now - Secs, Labels), Cls,
          [[{datetime, Iso}, {title, Title}, {data_ah, <<"time-ago">>},
            {data_ah_title, Title =/= undefined andalso <<"true">>},
            {data_ah_live, R#ah_time_ago.live =:= false andalso <<"false">>},
            LabelAttrs],
           ?E:root_attrs(R, none)]).

default_labels() ->
    #{just_now => <<"just now">>, minutes => <<"{n}m ago">>, hours => <<"{n}h ago">>,
      days => <<"{n}d ago">>, months => <<"{n}mo ago">>}.

format_ago(Secs, L) ->
    Mins = Secs div 60, Hours = Mins div 60, Days = Hours div 24, Months = Days div 30,
    Sub = fun(K, N) -> binary:replace(to_bin(maps:get(K, L)), <<"{n}">>,
                                      integer_to_binary(N), [global]) end,
    if Secs < 60 -> maps:get(just_now, L);
       Mins < 60 -> Sub(minutes, Mins);
       Hours < 24 -> Sub(hours, Hours);
       Days < 30 -> Sub(days, Days);
       true -> Sub(months, Months)
    end.

to_unix(S) when is_integer(S) -> S;
to_unix({{_, _, _}, {_, _, _}} = DT) ->
    calendar:datetime_to_gregorian_seconds(DT) - 62167219200;
to_unix(B) when is_binary(B) ->
    try calendar:rfc3339_to_system_time(binary_to_list(B))
    catch _:_ -> error({aihtml, {bad_timestamp, B}})
    end;
to_unix(Other) -> error({aihtml, {bad_timestamp, Other}}).

dash(A) -> binary:replace(to_bin(A), <<"_">>, <<"-">>, [global]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => time_ago, category => text, root => <<"ah-time-ago">>,
      signature => <<"ah_time_ago(Timestamp, Css, Attrs)">>,
      options => [now, labels, live, title], behavior => <<"time-ago">>,
      doc => <<"Relative time (\"3m ago\") from Unix seconds or a UTC datetime; "
               "updated every 60 s. Methods setDate(iso), refresh().">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{now => <<"Unix seconds to render against (default: the current time).">>,
                       labels => <<"Map overriding just_now, minutes, hours, days, months; "
                                   "\"{n}\" is the number.">>,
                       live => <<"Refresh every 60 s in the browser (default true).">>,
                       title => <<"Show the absolute time on hover (default true).">>},
      methods => [method(setDate, <<"(IsoOrMillis)">>, <<"Point at another time and re-render.">>),
                  method(refresh, <<"()">>, <<"Re-render against the current time.">>)]}.
