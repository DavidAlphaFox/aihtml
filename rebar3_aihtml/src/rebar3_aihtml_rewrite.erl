%%%-------------------------------------------------------------------
%%% @doc v0.3.x flat-context templates -> standard context stack semantics.
%%%
%%% The rule (designs/03 7.1) is one line: inside the body of `{{#X}}', a
%%% reference that starts with `X.' loses that one prefix. Everything hard
%%% about this module is in obeying two constraints.
%%%
%%% First, the decision is made on the AST, never on a regex. A regex would
%%% happily rewrite `{{! items.x }}' inside a comment, or text that merely
%%% looks like a tag, or miss a custom delimiter section entirely.
%%%
%%% Second, the EDIT is made on the source text, never by re-serialising the
%%% AST. Re-serialising would silently normalise away comments, blank lines,
%%% the space in `{{# items}}' and any `{{=<% %>=}}' switch. So the AST decides
%%% which tag changes and to what, and a byte-exact tag index says where that
%%% tag lives; only the key text inside the tag is replaced.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml_rewrite).

%% Deliberately no -include("ai_mustache.hrl"): this module is part of the
%% plugin proper and must build even where the parent app's include directory
%% is not reachable. The two facts it needs from there -- the default
%% delimiters and what counts as a marker -- are restated below in the terms
%% that make them independently derivable rather than copied as constants.
-define(OPEN,  <<"{{">>).
-define(CLOSE, <<"}}">>).

-export([file/2, tags/1]).

-record(tag, {loc      :: {pos_integer(), pos_integer()},
              off      :: non_neg_integer(),   % start of the open delimiter
              len      :: non_neg_integer(),   % whole tag, delimiters included
              inner    :: {non_neg_integer(), non_neg_integer()},
              marker   :: none | char()}).

%% @doc Rewrite one template body.
%%
%% Returns {NewBody, RewriteCount, Manual} where Manual is a list of
%% {Line, Description} for the things a machine must not decide.
file(Body, Opts) ->
    case ai_mustache_parser:parse(Body, Opts) of
        {error, _} = E -> E;
        {ok, Nodes} ->
            Tags = tags(Body),
            Index = index(Tags),
            Sections = section_keys(Nodes, []),
            St = #{tags => Tags, index => Index, sections => Sections,
                   edits => [], manual => []},
            St1 = walk(Nodes, [], St),
            Edits = lists:usort(maps:get(edits, St1)),
            {apply_edits(Body, Edits), length(Edits),
             lists:usort(maps:get(manual, St1))}
    end.

%%%===================================================================
%%% Byte-exact tag index
%%%===================================================================

%% @doc Every tag in the source with its exact byte span.
%%
%% Mirrors ai_mustache_scanner's delimiter handling, including `{{{x}}}' only
%% being special while the opening delimiter is all braces and `{{=<% %>=}}'
%% switching them. The AST carries {Line, Col} but no span, so the span has to
%% be recovered here; {Line, Col} is unique per tag and is the join key.
tags(Bin) -> tags(Bin, 0, ?OPEN, ?CLOSE, 1, 1, []).

tags(Bin, Off, Start, Stop, L, C, Acc) ->
    Rest = binary:part(Bin, Off, byte_size(Bin) - Off),
    case binary:match(Rest, Start) of
        nomatch -> lists:reverse(Acc);
        {P, SL} ->
            {L1, C1} = advance(binary:part(Rest, 0, P), L, C),
            TagOff = Off + P,
            After = binary:part(Bin, TagOff + SL, byte_size(Bin) - TagOff - SL),
            case tag_at(After, Start, Stop) of
                nomatch -> lists:reverse(Acc);
                {InnerSkip, InnerLen, CloseLen} ->
                    Consumed = SL + InnerSkip + InnerLen + CloseLen,
                    InnerOff = TagOff + SL + InnerSkip,
                    Inner = binary:part(Bin, InnerOff, InnerLen),
                    Marker = marker_of(Inner),
                    T = #tag{loc = {L1, C1}, off = TagOff, len = Consumed,
                             inner = {InnerOff, InnerLen}, marker = Marker},
                    {Start1, Stop1} = maybe_switch(Marker, Inner, Start, Stop),
                    {L2, C2} = advance(binary:part(Bin, TagOff, Consumed), L1, C1),
                    tags(Bin, TagOff + Consumed, Start1, Stop1, L2, C2, [T | Acc])
            end
    end.

%% {InnerSkip, InnerLen, CloseLen} relative to the end of the open delimiter.
tag_at(After, Start, Stop) ->
    case all_brace(Start) andalso After =/= <<>>
        andalso binary:first(After) =:= ${ of
        true ->
            Close = <<"}", Stop/binary>>,
            case binary:match(After, Close) of
                nomatch -> nomatch;
                {P, CL} -> {1, P - 1, CL}
            end;
        false ->
            case binary:match(After, Stop) of
                nomatch -> nomatch;
                {P, CL} -> {0, P, CL}
            end
    end.

all_brace(<<${, Rest/binary>>) -> all_brace(Rest);
all_brace(<<>>)                -> true;
all_brace(_)                   -> false.

maybe_switch($=, Inner, Start, Stop) ->
    %% `{{=<% %>=}}' reaches here as inner text `=<% %>='.
    Body = trim(Inner),
    Sz = byte_size(Body) - 1,
    case Sz > 0 andalso Body of
        <<"=", Mid:(Sz - 1)/binary, "=">> ->
            case binary:split(trim(Mid), <<" ">>, [global]) of
                [A, B] when A =/= <<>>, B =/= <<>> -> {A, B};
                Parts ->
                    case [X || X <- Parts, X =/= <<>>] of
                        [A, B] -> {A, B};
                        _      -> {Start, Stop}
                    end
            end;
        _ -> {Start, Stop}
    end;
maybe_switch(_, _, Start, Stop) ->
    {Start, Stop}.

%% A tag either starts with a marker character or with a key. Rather than
%% carrying a copy of the builtin marker list -- which would then have to be
%% kept in step with ai_mustache.hrl, and would still miss the characters
%% extension modules register -- ask the complementary question: anything that
%% cannot begin a key is a marker. That is exactly the rule
%% ai_mustache_scanner:marker_of/2 applies, and it covers extension markers for
%% free.
marker_of(Inner) ->
    case trim_left(Inner) of
        <<C, _/binary>> ->
            case name_char(C) of
                true  -> none;
                false -> C
            end;
        <<>> -> none
    end.

name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char(C) when C =:= $_; C =:= $. -> true;
name_char(C) -> C > 127.

advance(Seg, L, C) ->
    case binary:matches(Seg, <<"\n">>) of
        [] -> {L, C + byte_size(Seg)};
        Ms -> {P, _} = lists:last(Ms), {L + length(Ms), byte_size(Seg) - P}
    end.

index(Tags) ->
    maps:from_list([{T#tag.loc, N} || {T, N} <- lists:zip(Tags, seq(Tags))]).

seq(L) -> lists:seq(1, length(L)).

%%%===================================================================
%%% AST walk
%%%===================================================================

section_keys([], Acc) -> Acc;
section_keys([{section, _, Keys, Body} | Rest], Acc) ->
    section_keys(Rest, section_keys(Body, [hd(Keys) | Acc]));
section_keys([Node | Rest], Acc) ->
    section_keys(Rest, section_keys(body_of(Node), Acc)).

body_of({section,  _, _, B})    -> B;
body_of({inverted, _, _, B})    -> B;
body_of({has,      _, _, B, _}) -> B;
body_of({ext,      _, _, _, B}) -> B;
body_of(_)                      -> [].

%% Scopes is the stack of section keys currently in effect, innermost first.
%% Only {{#x}} pushes: designs/03 2 is explicit that {{^x}} and {{+x}} test a
%% key without entering it, so their bodies are still in the outer scope.
walk([], _Scopes, St) ->
    St;
walk([{section, Loc, Keys, Body} | Rest], Scopes, St0) ->
    {St1, Eff} = consider(Loc, Keys, Scopes, block, St0),
    %% Judged on what the tag will SAY, not on what it says now: {{#a.b}}
    %% inside {{#a}} becomes {{#b}} and needs no human at all. Only a dot that
    %% survives the rewrite is a problem, because dotted section keys drill
    %% down without walking back up the stack.
    St2 = case Eff of
              [_] -> St1;
              _   -> manual(Loc, io_lib:format(
                                   "{{#~ts}} -- a dotted section key does not "
                                   "walk back up the stack in the new "
                                   "semantics; check it by hand",
                                   [keys(Eff)]), St1)
          end,
    %% The body's references were written against the ORIGINAL key, so that is
    %% what goes on the scope stack.
    St3 = walk(Body, [Keys | Scopes], St2),
    walk(Rest, Scopes, St3);
walk([{inverted, Loc, Keys, Body} | Rest], Scopes, St0) ->
    {St1, _} = consider(Loc, Keys, Scopes, block, St0),
    walk(Rest, Scopes, walk(Body, Scopes, St1));
walk([{has, Loc, Keys, Body, _} | Rest], Scopes, St0) ->
    {St1, _} = consider(Loc, Keys, Scopes, block, St0),
    walk(Rest, Scopes, walk(Body, Scopes, St1));
walk([{var, Loc, Keys, _} | Rest], Scopes, St0) ->
    {St1, _} = consider(Loc, Keys, Scopes, single, St0),
    walk(Rest, Scopes, St1);
walk([{lambda, Loc, Keys} | Rest], Scopes, St0) ->
    St1 = manual(Loc, io_lib:format(
                        "{{*~ts}} -- a lambda now receives the top frame "
                        "rather than the root context (designs/03 5); check "
                        "it by hand", [keys(Keys)]), St0),
    walk(Rest, Scopes, St1);
walk([{partial, Loc, Name, _} | Rest], Scopes, St0) ->
    St1 = manual(Loc, io_lib:format(
                        "{{> ~ts}} -- a partial renders with the stack of its "
                        "call site, so rewriting it needs cross-file "
                        "reasoning; check it by hand", [Name]), St0),
    walk(Rest, Scopes, St1);
walk([{ext, _, _, _, Body} | Rest], Scopes, St0) ->
    walk(Rest, Scopes, walk(Body, Scopes, St0));
walk([_ | Rest], Scopes, St) ->
    walk(Rest, Scopes, St).

%% Kind is `block' when the tag has a matching {{/x}} that must change with it.
%% Returns the updated state and the key path the tag will end up carrying.
consider(_Loc, Keys, [], _Kind, St) ->
    {St, Keys};
consider(Loc, Keys, [Scope | Outer], Kind, St) ->
    case strip(Scope, Keys) of
        {ok, New} ->
            {edit(Loc, New, Kind, St), New};
        no ->
            %% Not our prefix. Two shapes deserve a note rather than silence.
            St1 = case Keys of
                      [Head | _] when length(Keys) > 1 ->
                          case lists:any(fun([H | _]) -> H =:= Head end, Outer) of
                              true ->
                                  manual(Loc, io_lib:format(
                                                "{{~ts}} -- prefixed by an "
                                                "enclosing but not immediately "
                                                "enclosing section; the stack "
                                                "may resolve it, or an inner "
                                                "key may shadow it",
                                                [keys(Keys)]), St);
                              false ->
                                  sibling(Loc, Keys, Head, St)
                          end;
                      _ ->
                          St
                  end,
            {St1, Keys}
    end.

sibling(Loc, Keys, Head, St) ->
    case lists:member(Head, maps:get(sections, St)) of
        true ->
            manual(Loc, io_lib:format(
                          "{{~ts}} -- references the sibling section ~s; "
                          "check it by hand", [keys(Keys), Head]), St);
        false ->
            St
    end.

%% Only one level comes off, the top of the stack. Nesting resolves itself:
%% the walk is outside-in and each level matches its own prefix on the way
%% down. Stripping a key down to nothing ({{items}} inside {{#items}}) is not
%% a rewrite -- there would be nothing left to write.
strip(Scope, Keys) ->
    N = length(Scope),
    case length(Keys) > N andalso lists:prefix(Scope, Keys) of
        true  -> {ok, lists:nthtail(N, Keys)};
        false -> no
    end.

%%%===================================================================
%%% Edits
%%%===================================================================

edit(Loc, NewKeys, Kind, St) ->
    Index = maps:get(index, St),
    case maps:get(Loc, Index, undefined) of
        undefined -> St;
        N ->
            Tags = maps:get(tags, St),
            Open = lists:nth(N, Tags),
            Es = [tag_edit(Open, NewKeys)] ++
                case Kind of
                    single -> [];
                    block  ->
                        %% The closing tag has to stay identical to the
                        %% opening one, so it is rewritten in the same step.
                        case close_after(Tags, N) of
                            none  -> [];
                            Close -> [tag_edit(Close, NewKeys)]
                        end
                end,
            St#{edits := [E || E <- Es, E =/= skip] ++ maps:get(edits, St)}
    end.

close_after(Tags, N) ->
    close_of(lists:nthtail(N, Tags), 1).

close_of([], _) -> none;
close_of([#tag{marker = $/} | _] = L, 1) -> hd(L);
close_of([#tag{marker = $/} | Rest], D) -> close_of(Rest, D - 1);
close_of([#tag{marker = M} | Rest], D)
  when M =:= $#; M =:= $^; M =:= $+; M =:= $- ->
    close_of(Rest, D + 1);
close_of([_ | Rest], D) ->
    close_of(Rest, D).

%% Replace the key text and nothing else: the delimiters, the marker and the
%% author's spacing inside the tag all survive, so `{{ items.name }}' becomes
%% `{{ name }}' rather than `{{name}}'.
tag_edit(#tag{inner = {Off, Len}}, NewKeys) ->
    {Off, Len, {keys_bin(NewKeys), Off, Len}}.

keys_bin(Keys) ->
    unicode:characters_to_binary(lists:join(".", [atom_to_list(K) || K <- Keys])).

%% Edits are applied back to front so that an earlier replacement cannot move
%% a later offset.
apply_edits(Bin, Edits) ->
    lists:foldl(fun({Off, Len, {New, _, _}}, Acc) ->
                        Inner = binary:part(Acc, Off, Len),
                        Replaced = replace_key(Inner, New),
                        <<(binary:part(Acc, 0, Off))/binary,
                          Replaced/binary,
                          (binary:part(Acc, Off + Len,
                                       byte_size(Acc) - Off - Len))/binary>>
                end, Bin, lists:reverse(lists:sort(Edits))).

replace_key(Inner, New) ->
    {Lead, R1} = take_ws(Inner, <<>>),
    {Mark, R2} = case R1 of
                     <<C, T/binary>> ->
                         case name_char(C) of
                             true  -> {<<>>, R1};
                             false -> {<<C>>, T}
                         end;
                     <<>> -> {<<>>, R1}
                 end,
    {Mid, R3} = take_ws(R2, <<>>),
    Trail = trailing_ws(R3),
    TL = byte_size(Trail),
    _Key = binary:part(R3, 0, byte_size(R3) - TL),
    <<Lead/binary, Mark/binary, Mid/binary, New/binary, Trail/binary>>.

take_ws(<<C, Rest/binary>>, Acc) when C =:= $\s; C =:= $\t ->
    take_ws(Rest, <<Acc/binary, C>>);
take_ws(Bin, Acc) ->
    {Acc, Bin}.

trailing_ws(Bin) -> trailing_ws(Bin, byte_size(Bin)).

trailing_ws(Bin, N) when N > 0 ->
    case binary:at(Bin, N - 1) of
        C when C =:= $\s; C =:= $\t -> trailing_ws(Bin, N - 1);
        _ -> binary:part(Bin, N, byte_size(Bin) - N)
    end;
trailing_ws(Bin, 0) ->
    Bin.

%%%===================================================================
%%% Helpers
%%%===================================================================

manual({Line, _}, IoData, St) ->
    St#{manual := [{Line, lists:flatten(IoData)} | maps:get(manual, St)]}.

keys(Keys) -> lists:join(".", [atom_to_list(K) || K <- Keys]).

trim(Bin) -> trim_right(trim_left(Bin)).

trim_left(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t -> trim_left(Rest);
trim_left(Bin) -> Bin.

trim_right(Bin) ->
    Sz = byte_size(Bin) - 1,
    case Sz >= 0 andalso Bin of
        <<Head:Sz/binary, C>> when C =:= $\s; C =:= $\t -> trim_right(Head);
        _ -> Bin
    end.
