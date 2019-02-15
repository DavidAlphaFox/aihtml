-module(dom_render).
-compile(export_all).

build()->
		Tree = ai_dom_node:new(html),
    Tree0 = ai_dom_node:insert_attributes(#{<<"lang">> => <<"zh_cn">>},Tree),
		Header = ai_dom_node:new(header),
		Body = ai_dom_node:new(body),
		Table = ai_dom_node:new(table),
		Tr = ai_dom_node:new(tr),
		Thead = ai_dom_node:new(thead),
		Ths = lists:map(fun(El)->
						ai_dom_node:set_value(ai_dom_node:id(El),El)
				end,[ai_dom_node:new(ID,th) || ID
 <- lists:seq(1,10)]),
		Tr0 = ai_dom_node:append_children(Ths,Tr),
		Thead0 = ai_dom_node:append_child(Tr0,Thead),
		Table0 = ai_dom_node:append_child(Thead0,Table),
		Body0 = ai_dom_node:append_child(Table0,Body),
		ai_dom_node:append_children([Header,Body0],Tree0).

start() ->
		code:add_patha("../ebin"),
		code:add_patha("../deps/ailib/ebin"),
		application:start(ailib),
		application:start(aihtml),
		Tree = build(),
		R = ai_dom_render:render(Tree),	
		io:format("~ts~n",[R]).
		
	
