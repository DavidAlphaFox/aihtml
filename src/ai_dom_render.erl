-module(ai_dom_render).
-export([render/1]).

render(Node)-> render([Node],[],<<>>).
render(undefined,Node)-> ai_string:to_string(ai_dom_node:value(Node));
render(Tag,Node) ->
		TagBin = ai_string:to_string(Tag),
		Attributes = attributes(Node),
		case ai_dom_node:opening(Tag) of 
				false ->
					Value = 
						case ai_dom_node:value(Node) of 
							undefined -> <<"">>;
							NV -> ai_string:to_string(NV)
						end,
					<<"<",TagBin/binary,Attributes/binary,">",Value/binary,"</",TagBin/binary,">">>;
				true ->
					EndTag = 
						case ai_dom_node:slash(Node) of 
							true -> <<" />">>;
							false -> <<" >">>
						end,
					<<"<",TagBin/binary,Attributes/binary,EndTag/binary>>
		end.
render([],[],Acc)-> Acc;
render([],[{Parent,Rest,OldAcc}|Stack],Acc)->
		TagBin = ai_string:to_string(ai_dom_node:tag(Parent)),
		Attributes = attributes(Parent),
		Acc1 = <<"<",TagBin/binary,Attributes/binary,">",Acc/binary,"</",TagBin/binary,">">>,
		render(Rest,Stack,<<OldAcc/binary,Acc1/binary>>);
render([El|Rest],Stack,Acc) ->
	case ai_dom_node:children(El) of
		[]->
				NodeBin = render(ai_dom_node:tag(El),El),
				render(Rest,Stack,<<Acc/binary,NodeBin/binary>>);
		Children -> render(Children,[{El,Rest,Acc}|Stack],<<>>)
	end.

attributes(Node)->
    Attributes = ai_dom_node:attributes(Node),
    Attributes0 =
        case ai_dom_node:id(Node) of
            undefined -> Attributes;
            ID -> maps:put(id,ID,Attributes)
        end,
		lists:foldl(fun({K,V},Acc)->
									Attr = ai_string:to_string(K),
									case V of
										ture -> <<Acc/binary,Attr/binary," ">>;
										_->
											AttrValue = ai_string:to_string(V),
											<<Acc/binary,Attr/binary,"=\"",AttrValue/binary,"\" ">>
									end
								end,<<" ">>,maps:to_list(Attributes0)).

