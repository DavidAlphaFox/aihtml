%% @doc The element record of a component, for the API tab of the docs
%% page: its fields with their types and defaults, taken from the group
%% module's debug_info, and the comment above the record in its header
%% (designs/05-records.md). Like the demo sources, what the page shows is
%% what the library was compiled with.
-module(aihtml_example_records).

-export([record/1, base_fields/0]).

-export_type([info/0, field/0]).

-type field() :: #{name := atom(), type := binary(), default := binary()}.
-type info() :: #{record := atom(), header := binary(), doc := binary(),
                  fields := [field()]}.

%% The ?AH_BASE fields, described once on the page instead of per record.
-define(BASE, [module, id, css, attrs, postback, delegate]).
%% Longest expanded type shown instead of the type's name.
-define(MAX_TYPE, 100).

-spec base_fields() -> [atom()].
base_fields() -> ?BASE.

%% @doc The record of component `Name' (ah_<Name>), or `undefined' when the
%% component has none (e.g. an action such as toast).
-spec record(atom()) -> info() | undefined.
record(Name) ->
    Tag = list_to_atom("ah_" ++ atom_to_list(Name)),
    find(Tag, aihtml_catalog:modules()).

find(_Tag, []) -> undefined;
find(Tag, [M | Ms]) ->
    case forms(M) of
        {ok, Forms} ->
            case scan(Tag, Forms, undefined) of
                {ok, File, Line, Fields} -> describe(Tag, File, Line, Fields, Forms);
                error -> find(Tag, Ms)
            end;
        error -> find(Tag, Ms)
    end.

forms(M) ->
    case code:which(M) of
        Beam when is_list(Beam) ->
            case beam_lib:chunks(Beam, [abstract_code]) of
                {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} -> {ok, Forms};
                _ -> error
            end;
        _ -> error
    end.

%% The record's definition and the file it was read from (the last `file'
%% attribute before it: the header that defines it).
scan(_Tag, [], _File) -> error;
scan(Tag, [{attribute, _, file, {File, _}} | Rest], _) -> scan(Tag, Rest, File);
scan(Tag, [{attribute, Anno, record, {Tag, Fields}} | _], File) ->
    {ok, File, erl_anno:line(Anno), Fields};
scan(Tag, [_ | Rest], File) -> scan(Tag, Rest, File).

describe(Tag, File, Line, Fields, Forms) ->
    Types = maps:from_list([{N, Def} || {attribute, _, type, {N, Def, []}} <- Forms]),
    Header = list_to_binary(filename:basename(File)),
    #{record => Tag,
      header => Header,
      doc => comment(File, Header, Line),
      fields => [F || RF <- Fields,
                      #{name := N} = F <- [field(RF, Types)],
                      not lists:member(N, ?BASE)]}.

field({typed_record_field, RF, Type}, Types) ->
    (field(RF, Types))#{type := type_text(Type, Types)};
field({record_field, _, {atom, _, N}}, _) ->
    #{name => N, type => <<"term()">>, default => <<"undefined">>};
field({record_field, _, {atom, _, N}, Default}, _) ->
    #{name => N, type => <<"term()">>,
      default => text(erl_pp:expr(Default, [{encoding, utf8}]))}.

%% Header types (ah_*) are written out when that stays short, e.g. an
%% enumeration such as `sm | md | lg'; longer ones (item lists, recursive
%% menus) keep their name, which is defined in the same header.
type_text(Type, Types) ->
    Expanded = pp_type(expand(Type, Types, 1)),
    case byte_size(Expanded) =< ?MAX_TYPE of
        true -> Expanded;
        false -> pp_type(Type)
    end.

expand({user_type, _, N, []} = T, Types, Depth) when Depth > 0 ->
    case {atom_to_list(N), maps:find(N, Types)} of
        {"ah_" ++ _, {ok, Def}} -> expand(Def, Types, Depth - 1);
        _ -> T
    end;
expand(T, Types, Depth) when is_tuple(T) ->
    list_to_tuple([expand(E, Types, Depth) || E <- tuple_to_list(T)]);
expand(L, Types, Depth) when is_list(L) ->
    [expand(E, Types, Depth) || E <- L];
expand(X, _, _) -> X.

pp_type(Type) ->
    Text = text(erl_pp:attribute({attribute, erl_anno:new(0), type, {t, Type, []}}, [{encoding, utf8}])),
    %% "-type t() :: ... ." on one line
    Body = re:replace(Text, <<"^-type t\\(\\)\\s*::\\s*|\\.\\s*$">>, <<>>, [global, {return, binary}]),
    re:replace(Body, <<"\\s+">>, <<" ">>, [global, {return, binary}]).

%% The %% lines right above the record in its header. The header is looked
%% up where it was compiled, then in the aihtml application's include dir
%% (a release may keep only that).
comment(File, Header, Line) ->
    Candidates = [File | [filename:join([D, "include", Header])
                          || D <- [code:lib_dir(aihtml)], is_list(D)]],
    case [B || P <- Candidates, {ok, B} <- [file:read_file(P)]] of
        [Bin | _] ->
            Lines = lists:sublist(binary:split(Bin, <<"\n">>, [global]), Line - 1),
            Comment = lists:reverse(lists:takewhile(fun is_comment/1, lists:reverse(Lines))),
            iolist_to_binary(lists:join(<<" ">>, [string:trim(strip(L)) || L <- Comment]));
        [] ->
            <<>>
    end.

is_comment(<<"%%", _/binary>>) -> true;
is_comment(_) -> false.

strip(<<"%%", Rest/binary>>) -> Rest.

text(Chars) -> unicode:characters_to_binary(Chars).
