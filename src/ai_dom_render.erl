-module(ai_dom_render).
-export([render/1]).

render(Node)->
		render([Node],[],<<>>).
render(undefined,Node)-> ai_string:to_string(ai_dom_node:value(Node));
render(meta,Node)->
		Attributes = attributes(Node),
		<<"<meta",Attributes/binary,"/>">>;
render(area,Node)->
		Attributes = attributes(Node),
		<<"<area",Attributes/binary,"/>">>;
render(base,Node)->
		Attributes = attributes(Node),
		<<"<base",Attributes/binary,"/>">>;
render(br,Node)->
		Attributes = attributes(Node),
		<<"<br",Attributes/binary,"/>">>;
render(col,Node)->
		Attributes = attributes(Node),
		<<"<col",Attributes/binary,"/>">>;
render(embed,Node)->
		Attributes = attributes(Node),
		<<"<embed",Attributes/binary,"/>">>;
render(hr,Node)->
		Attributes = attributes(Node),
		<<"<hr",Attributes/binary,"/>">>;
render(img,Node)->
		Attributes = attributes(Node),
		<<"<img",Attributes/binary,"/>">>;
render(input,Node)->
		Attributes = attributes(Node),
		<<"<input",Attributes/binary,"/>">>;
render(link,Node)->
		Attributes = attributes(Node),
		<<"<link",Attributes/binary,"/>">>;
render(param,Node)->
		Attributes = attributes(Node),
		<<"<param",Attributes/binary,"/>">>;
render(source,Node)->
		Attributes = attributes(Node),
		<<"<source",Attributes/binary,"/>">>;
render(track,Node)->
		Attributes = attributes(Node),
		<<"<track",Attributes/binary,"/>">>;
render(wbr,Node)->
		Attributes = attributes(Node),
		<<"<wbr",Attributes/binary,"/>">>;
render(script,Node)->
		Attributes = attributes(Node),
		<<"<script",Attributes/binary,"/>">>;
render(Tag,Node) ->
		TagBin = ai_string:to_string(Tag),
		Value = ai_string:to_string(ai_dom_node:value(Node)),
		Attributes = attributes(Node),
		<<"<",TagBin/binary,Attributes/binary,">",Value/binary,"</",TagBin/binary,">">>.
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

