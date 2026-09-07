%%%-------------------------------------------------------------------
%%% @doc Mustache lexer: template binary -> token stream.
%%%
%%% The scanner does no nesting and no matching of open/close tags; `{{/x}}'
%%% is just another token. All structure is the parser's job (T08), which
%%% keeps both sides independently testable.
%%%
%%% Comments and delimiter switches are consumed here and never reach the
%%% parser, but they still take part in standalone-line processing, so they
%%% travel through the pipeline as internal `skip' placeholders that are
%%% filtered out at the very end.
%%%
%%% The tag-splitting logic, in particular the handling of {{{ }}} against a
%%% configurable delimiter, derives from bbmustache
%%% (https://github.com/soranoba/bbmustache), MIT, Copyright (c) 2015
%%% Hinagiku Soranoba.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_scanner).

-include("ai_mustache.hrl").

-export([scan/1, scan/2]).

-record(st, {start         = ?AI_MUSTACHE_START :: binary(),
             stop          = ?AI_MUSTACHE_STOP  :: binary(),
             file          = <<"nofile">>       :: binary(),
             markers       = []                 :: [char()],
             block_markers = []                 :: [char()]}).

%% Internal placeholder for a tag that produces no output but still
%% participates in standalone processing.
-type skip_token() :: {skip, ai_mustache_loc(), char()}.
-type wtoken()     :: ai_mustache_token() | skip_token().

%%%===================================================================
%%% API
%%%===================================================================

-spec scan(unicode:chardata()) -> {ok, [ai_mustache_token()]} | ai_mustache_error().
scan(Data) -> scan(Data, #{}).

%% The single place a template body enters the pipeline, and therefore the
%% single place it is normalised and checked. Everything after this point
%% holds a UTF-8 binary.
-spec scan(unicode:chardata(), map()) ->
          {ok, [ai_mustache_token()]} | ai_mustache_error().
scan(Data, Opts0) ->
    Opts = ai_html_text:opts(Opts0),
    File = ai_html_text:source(Opts),
    case ai_html_text:template(Data) of
        {error, Reason} ->
            {error, {File, 1, Reason}};
        {ok, Bin} ->
            {Markers, Blocks} = ext_markers(Opts),
            St = #st{file          = File,
                     markers       = Markers,
                     block_markers = Blocks},
            case do_scan(Bin, St, 1, 1, []) of
                {error, _} = E -> E;
                {ok, Raw}      -> {ok, drop_skips(standalone(Raw, St))}
            end
    end.

%% Extension markers come from modules listed under `extensions'. Modules that
%% are absent or do not export markers/0 are ignored here; the parse_transform
%% and the plugin are responsible for reporting misconfiguration (T21/T23).
-spec ext_markers(map()) -> {[char()], [char()]}.
ext_markers(Opts) ->
    Mods = maps:get(extensions, Opts, []),
    lists:foldl(
      fun(M, {Ms, Bs}) ->
              _ = code:ensure_loaded(M),
              M1 = call_markers(M, markers, Ms),
              B1 = call_markers(M, block_markers, Bs),
              {M1, B1}
      end, {[], []}, Mods).

-spec call_markers(module(), atom(), [char()]) -> [char()].
call_markers(M, F, Acc) ->
    case erlang:function_exported(M, F, 0) of
        true  -> lists:usort(Acc ++ M:F());
        false -> Acc
    end.

%%%===================================================================
%%% Main loop
%%%===================================================================

-spec do_scan(binary(), #st{}, pos_integer(), pos_integer(), [wtoken()]) ->
          {ok, [wtoken()]} | ai_mustache_error().
do_scan(<<>>, _St, _L, _C, Acc) ->
    {ok, lists:reverse(Acc)};
do_scan(Bin, St, L, C, Acc) ->
    case binary:match(Bin, St#st.start) of
        nomatch ->
            {ok, lists:reverse(add_text(Bin, L, C, Acc))};
        {0, SL} ->
            tag(Bin, SL, St, L, C, Acc);
        {P, SL} ->
            Text = binary:part(Bin, 0, P),
            {L1, C1} = advance(Text, L, C),
            tag(suffix(Bin, P), SL, St, L1, C1, add_text(Text, L, C, Acc))
    end.

%% Empty text tokens are never emitted; this mirrors the ?ADD macro of the
%% original implementation.
-spec add_text(binary(), pos_integer(), pos_integer(), [wtoken()]) -> [wtoken()].
add_text(<<>>, _L, _C, Acc) -> Acc;
add_text(Bin, L, C, Acc)    -> [{text, {L, C}, Bin} | Acc].

%% Bin starts with the opening delimiter.
-spec tag(binary(), pos_integer(), #st{}, pos_integer(), pos_integer(), [wtoken()]) ->
          {ok, [wtoken()]} | ai_mustache_error().
tag(Bin, SL, St, L, C, Acc) ->
    After = suffix(Bin, SL),
    case triple(St#st.start, After) of
        true  -> triple_tag(Bin, After, SL, St, L, C, Acc);
        false -> plain_tag(Bin, After, SL, St, L, C, Acc)
    end.

%% `{{{x}}}' is only special when the opening delimiter is itself all braces;
%% after a delimiter switch the triple form no longer applies and `&' is the
%% only way to get a raw interpolation.
-spec triple(binary(), binary()) -> boolean().
triple(Start, After) ->
    all_char(Start, ${) andalso After =/= <<>> andalso binary:first(After) =:= ${.

-spec triple_tag(binary(), binary(), pos_integer(), #st{},
                 pos_integer(), pos_integer(), [wtoken()]) ->
          {ok, [wtoken()]} | ai_mustache_error().
triple_tag(Bin, After, SL, St, L, C, Acc) ->
    Close = <<"}", (St#st.stop)/binary>>,
    case binary:match(After, Close) of
        nomatch ->
            {error, {St#st.file, L, {unclosed_tag, []}}};
        {P, CL} ->
            Content  = binary:part(After, 1, P - 1),
            Rest     = suffix(After, P + CL),
            Consumed = SL + P + CL,
            emit(${, trim(Content), Rest, Consumed, Bin, St, L, C, Acc)
    end.

-spec plain_tag(binary(), binary(), pos_integer(), #st{},
                pos_integer(), pos_integer(), [wtoken()]) ->
          {ok, [wtoken()]} | ai_mustache_error().
plain_tag(Bin, After, SL, St, L, C, Acc) ->
    case binary:match(After, St#st.stop) of
        nomatch ->
            {error, {St#st.file, L, {unclosed_tag, []}}};
        {P, CL} ->
            Raw      = binary:part(After, 0, P),
            Rest     = suffix(After, P + CL),
            Consumed = SL + P + CL,
            case marker_of(trim_left(Raw), St) of
                {error, Reason} -> {error, {St#st.file, L, Reason}};
                {Marker, Body}  -> emit(Marker, trim(Body), Rest, Consumed,
                                        Bin, St, L, C, Acc)
            end
    end.

%%%===================================================================
%%% Marker classification
%%%===================================================================

-spec marker_of(binary(), #st{}) -> {ai_mustache_marker(), binary()} | {error, term()}.
marker_of(<<C, Rest/binary>>, _St) when C =:= $&; C =:= ${ ->
    {${, Rest};
marker_of(<<C, Rest/binary>>, _St)
  when C =:= $#; C =:= $^; C =:= $/; C =:= $>; C =:= $!;
       C =:= $=; C =:= $+; C =:= $-; C =:= $* ->
    {C, Rest};
marker_of(<<C, Rest/binary>> = Bin, St) ->
    case lists:member(C, St#st.markers) of
        true  -> {C, Rest};
        false ->
            %% A punctuation lead-in that nobody registered is a typo, not a
            %% variable name. Letters, digits, `_' and `.' start plain
            %% interpolations, so {{a-b}} and {{.}} are unaffected.
            case name_char(C) of
                true  -> {none, Bin};
                false -> {error, {unknown_marker, C}}
            end
    end;
marker_of(<<>>, _St) ->
    {none, <<>>}.

-spec name_char(char()) -> boolean().
name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char(C) when C =:= $_; C =:= $. -> true;
name_char(C) -> C > 127.

%%%===================================================================
%%% Token emission
%%%===================================================================

-spec emit(ai_mustache_marker(), binary(), binary(), non_neg_integer(), binary(),
           #st{}, pos_integer(), pos_integer(), [wtoken()]) ->
          {ok, [wtoken()]} | ai_mustache_error().
emit($!, _Body, Rest, Consumed, Bin, St, L, C, Acc) ->
    {L1, C1} = advance(binary:part(Bin, 0, Consumed), L, C),
    do_scan(Rest, St, L1, C1, [{skip, {L, C}, $!} | Acc]);
emit($=, Body, Rest, Consumed, Bin, St, L, C, Acc) ->
    case delimiters(Body) of
        {error, Reason} ->
            {error, {St#st.file, L, Reason}};
        {ok, Start, Stop} ->
            {L1, C1} = advance(binary:part(Bin, 0, Consumed), L, C),
            do_scan(Rest, St#st{start = Start, stop = Stop}, L1, C1,
                    [{skip, {L, C}, $=} | Acc])
    end;
emit(Marker, Body, Rest, Consumed, Bin, St, L, C, Acc) ->
    {L1, C1} = advance(binary:part(Bin, 0, Consumed), L, C),
    do_scan(Rest, St, L1, C1, [{tag, {L, C}, Marker, Body, <<>>} | Acc]).

%% `{{=<% %>=}}' arrives here as `<% %>=' (the leading `=' was the marker).
-spec delimiters(binary()) -> {ok, binary(), binary()} | {error, term()}.
delimiters(Body0) ->
    Sz = byte_size(Body0) - 1,
    case Sz >= 0 andalso Body0 of
        <<Body:Sz/binary, "=">> ->
            case binary:match(Body, <<"=">>) of
                nomatch ->
                    case [S || S <- binary:split(trim(Body), [<<" ">>, <<"\t">>],
                                                 [global]), S =/= <<>>] of
                        [Start, Stop] -> {ok, Start, Stop};
                        _             -> {error, {invalid_delimiter, Body0}}
                    end;
                _ ->
                    {error, {invalid_delimiter, Body0}}
            end;
        _ ->
            {error, {invalid_delimiter, Body0}}
    end.

%%%===================================================================
%%% Standalone lines
%%%===================================================================
%% A tag stands alone when its marker is block-like, nothing but whitespace
%% precedes it on its line, and nothing but whitespace plus a line ending
%% follows it. The surrounding whitespace and the line ending are then removed
%% and the leading whitespace is attached to the tag as its Indent, which is
%% what lets a partial indent every line of its output rather than just the
%% first (designs/03-semantics.md section 6).
%%
%% This runs as a pass over the token stream rather than inside the scan loop:
%% the "what follows" half is only knowable once the next token exists, and a
%% pass is far easier to reason about than threading lookahead through scan.

-spec standalone([wtoken()], #st{}) -> [wtoken()].
standalone(Tokens, St) -> sa(Tokens, St, [], <<>>).

%% Pre is the whitespace seen since the last line start, or `false' once
%% anything else has appeared on the line. It cannot be derived from the token
%% immediately to the left, because the run of whitespace preceding a tag may
%% be split across several tokens or have no token at all:
%%
%%   {{#items}}\n    {{> p}}\n{{/items}}
%%
%% Once {{#items}} eats its own line the "    " before {{> p}} is a text token
%% with no newline in it, yet {{> p}} really is at the start of a line -- and
%% getting that wrong costs the partial its indentation.
-spec sa([wtoken()], #st{}, [wtoken()], binary() | false) -> [wtoken()].
sa([], _St, Acc, _Pre) ->
    lists:reverse(Acc);
sa([{text, _, Bin} = T | Rest], St, Acc, Pre) ->
    sa(Rest, St, [T | Acc], advance_pre(Pre, Bin));
sa([T | Rest], St, Acc, Pre) ->
    case eligible(T, St) of
        false -> sa(Rest, St, keep(T, Acc), false);
        true  -> sa_try(T, Rest, St, Acc, Pre)
    end.

-spec advance_pre(binary() | false, binary()) -> binary() | false.
advance_pre(Pre, Bin) ->
    case last_nl(Bin) of
        {ok, P} ->
            Tail = suffix(Bin, P + 1),
            case blank(Tail) of
                true  -> Tail;
                false -> false
            end;
        none when Pre =:= false ->
            false;
        none ->
            case blank(Bin) of
                true  -> <<Pre/binary, Bin/binary>>;
                false -> false
            end
    end.

-spec sa_try(wtoken(), [wtoken()], #st{}, [wtoken()], binary() | false) -> [wtoken()].
sa_try(T, Rest, St, Acc, Pre) ->
    case {sa_before(Acc, Pre), sa_after(Rest)} of
        {{ok, Indent, Acc1}, {ok, Rest1}} ->
            sa(Rest1, St, keep(with_indent(T, Indent), Acc1), <<>>);
        _ ->
            sa(Rest, St, keep(T, Acc), false)
    end.

%% A consumed comment or delimiter switch emits nothing, so it must not stay
%% in the accumulator either: a leftover placeholder would sit between a text
%% token and the next tag and defeat that tag's own standalone check.
-spec keep(wtoken(), [wtoken()]) -> [wtoken()].
keep({skip, _, _}, Acc) -> Acc;
keep(T, Acc)            -> [T | Acc].

-spec with_indent(wtoken(), binary()) -> wtoken().
with_indent({tag, Loc, M, Body, _}, Indent) -> {tag, Loc, M, Body, Indent};
with_indent({skip, _, _} = T, _Indent)      -> T.

%% Plain interpolations, raw interpolations and lambdas are never standalone.
-spec eligible(wtoken(), #st{}) -> boolean().
eligible({skip, _, _}, _St) ->
    true;
eligible({tag, _, M, _, _}, St) ->
    lists:member(M, [$#, $^, $/, $>, $+, $-])
        orelse lists:member(M, St#st.block_markers).

%% Strip the whitespace Pre describes off the tail of the accumulator. It may
%% span a run of blank text tokens plus the tail of the token holding the last
%% newline.
-spec sa_before([wtoken()], binary() | false) -> {ok, binary(), [wtoken()]} | false.
sa_before(_Acc, false) ->
    false;
sa_before(Acc, Pre) ->
    {ok, Pre, strip_pre(Acc)}.

-spec strip_pre([wtoken()]) -> [wtoken()].
strip_pre([{text, Loc, Bin} | Tail]) ->
    case last_nl(Bin) of
        {ok, P} -> [{text, Loc, binary:part(Bin, 0, P + 1)} | Tail];
        none    -> strip_pre(Tail)
    end;
strip_pre(Acc) ->
    Acc.

-spec sa_after([wtoken()]) -> {ok, [wtoken()]} | false.
sa_after([]) ->
    {ok, []};
sa_after([{text, {L, _}, Bin} | Tail]) ->
    case binary:match(Bin, <<"\n">>) of
        {P, 1} ->
            case blank(strip_cr(binary:part(Bin, 0, P))) of
                true ->
                    %% An empty remainder must be dropped, not kept as an empty
                    %% text token: the next tag's sa_before would then see a
                    %% text node with no newline in it.
                    case suffix(Bin, P + 1) of
                        <<>>  -> {ok, Tail};
                        Rest  -> {ok, [{text, {L + 1, 1}, Rest} | Tail]}
                    end;
                false ->
                    false
            end;
        nomatch when Tail =:= [] ->
            case blank(Bin) of
                true  -> {ok, []};
                false -> false
            end;
        nomatch ->
            false
    end;
sa_after(_) ->
    false.

-spec drop_skips([wtoken()]) -> [ai_mustache_token()].
drop_skips(Tokens) -> [T || T <- Tokens, element(1, T) =/= skip].

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Line and column of the position just past Seg. Columns count bytes, not
%% codepoints: templates are UTF-8 and a byte column is still precise enough
%% to point at a tag, without paying for decoding on the scan path.
-spec advance(binary(), pos_integer(), pos_integer()) -> {pos_integer(), pos_integer()}.
advance(Seg, L, C) ->
    case binary:matches(Seg, <<"\n">>) of
        [] ->
            {L, C + byte_size(Seg)};
        Ms ->
            {P, _} = lists:last(Ms),
            {L + length(Ms), byte_size(Seg) - P}
    end.

-spec suffix(binary(), non_neg_integer()) -> binary().
suffix(Bin, P) -> binary:part(Bin, P, byte_size(Bin) - P).

-spec last_nl(binary()) -> {ok, non_neg_integer()} | none.
last_nl(Bin) ->
    case binary:matches(Bin, <<"\n">>) of
        [] -> none;
        Ms -> {P, _} = lists:last(Ms), {ok, P}
    end.

-spec strip_cr(binary()) -> binary().
strip_cr(Bin) ->
    Sz = byte_size(Bin) - 1,
    case Sz >= 0 andalso Bin of
        <<Head:Sz/binary, "\r">> -> Head;
        _                        -> Bin
    end.

-spec blank(binary()) -> boolean().
blank(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t -> blank(Rest);
blank(<<>>) -> true;
blank(_)    -> false.

-spec trim_left(binary()) -> binary().
trim_left(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t -> trim_left(Rest);
trim_left(Bin) -> Bin.

-spec trim_right(binary()) -> binary().
trim_right(Bin) ->
    Sz = byte_size(Bin) - 1,
    case Sz >= 0 andalso Bin of
        <<Head:Sz/binary, C>> when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
            trim_right(Head);
        _ ->
            Bin
    end.

-spec trim(binary()) -> binary().
trim(Bin) -> trim_right(trim_left(Bin)).

-spec all_char(binary(), char()) -> boolean().
all_char(<<C, Rest/binary>>, C) -> all_char(Rest, C);
all_char(<<>>, _)               -> true;
all_char(_, _)                  -> false.
