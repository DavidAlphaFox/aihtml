-module(ai_dom_render).
-export([render/1]).

render(Node)->
		render([Node],[],<<>>).
render(undefined,Node)-> ai_string:to_string(ai_dom_node:value(Node));
render(meta,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<meta",Attributes/binary,"/>">>;
render(area,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<area",Attributes/binary,"/>">>;
render(base,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<base",Attributes/binary,"/>">>;
render(br,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<br",Attributes/binary,"/>">>;
render(col,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<col",Attributes/binary,"/>">>;
render(embed,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<embed",Attributes/binary,"/>">>;
render(hr,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<hr",Attributes/binary,"/>">>;
render(img,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<img",Attributes/binary,"/>">>;
render(input,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<input",Attributes/binary,"/>">>;
render(link,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<link",Attributes/binary,"/>">>;
render(param,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<param",Attributes/binary,"/>">>;
render(source,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<source",Attributes/binary,"/>">>;
render(track,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<track",Attributes/binary,"/>">>;
render(wbr,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<wbr",Attributes/binary,"/>">>;
render(script,Node)->
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<script",Attributes/binary,"/>">>;
render(Tag,Node) ->
		TagBin = ai_string:to_string(Tag),
		Value = ai_string:to_string(ai_dom_node:value(Node)),
		Attributes = attributes(ai_dom_node:attributes(Node)),
		<<"<",TagBin/binary,Attributes/binary,">",Value/binary,"</",TagBin/binary,">">>.
render([],[],Acc)-> Acc;
render([],[{Parent,Rest,OldAcc}|Stack],Acc)->
		TagBin = ai_string:to_string(ai_dom_node:tag(Parent)),
		Attributes = attributes(ai_dom_node:attributes(Parent)),
		Acc1 = <<"<",TagBin/binary,Attributes/binary,">",Acc/binary,"</",TagBin/binary,">">>,
		render(Rest,Stack,<<OldAcc/binary,Acc1/binary>>);
render([El|Rest],Stack,Acc) -> 
	case ai_dom_node:children(El) of 
		[]->
				NodeBin = render(ai_dom_node:tag(El),El),
				render(Rest,Stack,<<Acc/binary,NodeBin/binary>>);
		Children -> render(Children,[{El,Rest,Acc}|Stack],<<>>)
	end.
attributes(Attributes)->	
		lists:foldl(fun({K,V},Acc)->
									Attr = ai_string:to_string(K),
									case V of
										ture -> <<Acc/binary,Attr/binary," ">>;
										_->
											AttrValue = ai_string:to_string(V),
											<<Acc/binary,Attr/binary,"=\"",AttrValue/binary,"\" ">>
									end
								end,<<" ">>,maps:to_list(Attributes)).

		
