-module(ai_dom_node).

-export([new/0,new/1,new/2]).
-export([insert_child/3,append_child/2,remove_child/2,
         append_children/2,remove_children/1,children/1]).
-export([insert_attribute/3,insert_attributes/2,remove_attribute/2,
         attribute/2,remove_attributes/1, attributes/1]).
-export([set_value/2,set_id/2,set_tag/2,value/1,id/1,tag/1]).

-record(ai_dom_node,{
										 id :: binary(), %% 节点ID
										 tag :: atom(), %% 节点名字
										 attributes :: maps:maps(), %% 节点属性
										 children :: [], %% 子节点
										 value :: term() %% 节点值
										}).

-opaque ai_dom_node() :: #ai_dom_node{}.
-export_type([ai_dom_node/0]).

new()-> new(undefined,undefined).
new(Tag)-> new(undefined,Tag).
new(ID,Tag)->
		#ai_dom_node{
			 id = ID,tag = Tag,
			 attributes = maps:new(),children = [],
			 value = undefined
			}.		
insert_child(Index,Child,Node)->
	case Node#ai_dom_node.children of 
		[] when Index == 0 ->
			Node#ai_dom_node{children = [Child]};
		[] ->
			throw({error,out_of_range});
		Children ->
			{_N,Children0} =
				lists:foldl(fun(C,{N,Acc})->	
														if N == Index -> {N + 2,[C,Child|Acc]};
															 true -> {N+1,[C|Acc]}
														end
										end,{0,[]},Children),
					Node#ai_dom_node{children = lists:reverse(Children0)}
	end.
    
append_child(Child,Node)->
	Node#ai_dom_node{
		children = lists:append(Node#ai_dom_node.children,[Child])
	}.
remove_child(Index,Node)->
    {CHead,[_I|CTail]} = lists:split(Index,Node#ai_dom_node.children),
    Node#ai_dom_node{
      children = lists:append(CHead,CTail)
     }.
remove_children(Node)-> Node#ai_dom_node{children = []}.
append_children(Children,Node)->
    Node#ai_dom_node{
      children = lists:append(Node#ai_dom_node.children,Children)
     }.
children(Node)-> Node#ai_dom_node.children.

insert_attribute(Name,Value,Node)->	
		Node#ai_dom_node{
			attributes = maps:put(Name,Value,Node#ai_dom_node.attributes)
		}.
remove_attribute(Name,Node)->
    Node#ai_dom_node{
			attributes = maps:remove(Name,Node#ai_dom_node.attributes)
    }.
insert_attributes(Attributes,Node)->	
		Node#ai_dom_node{
			attributes = maps:merge(Attributes,Node#ai_dom_node.attributes)
     }.
remove_attributes(Node)->
    Node#ai_dom_node{
			attributes = maps:new()
     }.
attribute(Name,Node)->
		Attributes = Node#ai_dom_node.attributes,
		maps:get(Name,Attributes,undefined).

attributes(Node)->Node#ai_dom_node.attributes.


set_value(Value,Node)-> Node#ai_dom_node{value = Value}.
set_tag(Tag,Node)->Node#ai_dom_node{tag = Tag}.
set_id(ID,Node)-> Node#ai_dom_node{id = ID}.
value(Node)-> Node#ai_dom_node.value.
tag(Node)-> Node#ai_dom_node.tag.
id(Node)-> Node#ai_dom_node.id.

