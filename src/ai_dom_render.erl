-module(ai_dom_render).
-export([render/1]).

render(Node)->
		Tag = ai_dom_node:tag(Node),
		render(Tag,Node).
render(meta,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<meta ",Attributes/binary,">">>;
render(undefined,Node)->ai_string:to_string(ai_dom_node:value(Node));
render(Tag,Node) ->
		TagBin = ai_string:to_string(Tag),
		Inner = 
				case {ai_dom_node:children(Node),ai_dom_node:value(Node)} of 
					{[],undefined} -> <<>>;
					{Children,undefined} -> children(Children);
					{[],Value}-> ai_string:to_string(Value)
				end,
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<",TagBin/binary,Attributes/binary,">",Inner/binary,"</",TagBin/binary,">">>.
children(Children)->
	lists:foldl(fun(Child,Acc)->
								R = render(Child),
								<<Acc/binary,R/binary>> 
							end,<<>>,Children).
attributes(Attributes)->	
		lists:foldl(fun({K,V},Acc)->
												Attr = ai_string:to_string(K),
												AttrValue = ai_string:to_string(V),
												<<Acc/binary,Attr/binary,"=\"",AttrValue/binary,"\" ">>
								end,<<>>,maps:to_list(Attributes)).
		
