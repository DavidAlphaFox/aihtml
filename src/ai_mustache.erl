-module(ai_mustache).

-export([compile/1]).

-record(state, {
  section_regex = undefined,
  tag_regex = undefined
}).

compile(Body) ->
  State = state(),
  CompiledTemplate = replace(Body),
  tag(CompiledTemplate,State).


state()->
    %% 匹配以下模式
    %% {{# Section}} Content {{/ Section}}
    SectionRegex = "^(.*?){{(#|\\^)([^}]*)}}\\s*(.+?){{/\\3}}\\s*(.*)",
	{ok, CompiledSectionRegex} = re:compile(SectionRegex, [dotall]),
	%% 匹配
	%% {{ Content }},{{{ Content }}},{{& Content }},{{! Content }}
	%% {{> Content }}
    TagRegex = "^(.*?){{(!|>|{|&)?(.+?)\\2?}}+(.*)",
    {ok, CompiledTagRegex} = re:compile(TagRegex, [dotall]),
    #state{section_regex = CompiledSectionRegex, tag_regex = CompiledTagRegex}.

replace(T) ->
  re:replace(T, "\"", "\\\"", [global,{return,binary}]).


tag(T, State) ->
	Regex = State#state.tag_regex,
	Res = re:run(T,Regex,[{capture,all,binary}]),
	tag(Res,T,Regex,[]).
tag(T,Regex,Acc)->
	Res = re:run(T,Regex,[{capture,all,binary}]),
	tag(Res,T,Regex,Acc).
tag({match,[_,Front,K,Content,Back]},_T,Regex,Acc)->
	Kind = tag_kind(K),
	Tag1 = compile_tag(Kind, Content),
	tag(Back,Regex,[Tag1,{binary,Front}|Acc]);  
tag(nomatch,T,_Regex,Acc)->
	Tag = lists:reverse([{binary,T}|Acc]),
	compile_tag_function(Tag).

tag_kind(<<>>)-> none;
tag_kind(<<">">>)-> partial;
tag_kind(<<"{">>)-> raw;
tag_kind(<<"&">>)-> raw;
tag_kind(<<"!">>)-> comment.
compile_tag(comment,_Key)->{binary,<<"">>};
compile_tag(_,Key)->
	Fun = fun(Ctx)->
    		case maps:get(Key, Ctx,undefined) of
				undefined -> <<"">>;
	    		Value -> ai_string:to_string(Value)
		 	end
		end,
	{function,Fun}.
compile_tag_function(List) ->
    fun(Ctx)-> 
        lists:foldl(fun(Item,Acc)->
            RB = case Item of
                    {function,TagFun}-> TagFun(Ctx);
                    {binary,Bin}-> Bin
                end,
            <<Acc/binary,RB/binary>>
        end,<<"">>,List)
    end.