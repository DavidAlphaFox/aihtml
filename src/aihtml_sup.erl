-module(aihtml_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
          
    TemplateLoader
         = #{id => ai_mustache_loader,
                 start => {ai_mustache_loader, start_link, []},
                 restart => transient,
                 shutdown => 5000,
                 type => worker,
                 modules => [ai_mustache_loader]},  
    
	{ok, {SupFlags,[TemplateLoader]}}.
