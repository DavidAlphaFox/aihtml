%%%-------------------------------------------------------------------
%%% @author  <david@laptop-02.local>
%%% @copyright (C) 2018, 
%%% @doc
%%% 模板加载器，单写入ets
%%% @end
%%% Created : 19 Dec 2018 by  <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ai_mustache_loader).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3, format_status/2]).

-export([prepare/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
prepare(Template)->
	TBin = ai_string:to_string(Template),
	{Path,Name} = split_path(TBin),
	Key = {Name,Path},
	Match = ets:lookup(ai_mustache,Key),
	case Match of 
		[] -> 
			load(TBin),
			prepare(TBin);
		[{Key,Partials}]->
			{Same,Other} = partials(Path,Partials),
			M0 = lists:foldl(fun({I,_V},Acc)->
					[{I,IR}] = ets:lookup(ai_mustache_ir,I),
					{PName,_PPath} = I,
					Acc#{PName => IR}
				end,#{},maps:to_list(Same)),
			M1 = lists:foldl(fun({I,V},Acc)->
					[{I,IR}] = ets:lookup(ai_mustache_ir,I),
					ai_maps:put(V,IR,Acc)
				end,M0,maps:to_list(Other)),
			[{Key,TIR}] = ets:lookup(ai_mustache_ir,Key),
			{TIR,M1}
	end.
partials(Path,Partials)->
	lists:foldl(fun(P,{Acc,Other})->
		{Paths,PPath,PName} = split_path(P),
		case PPath of 
			[] -> %% 同路径下模板
				Key = {PName,Path},
				[{Key,PPartials}] = ets:lookup(ai_mustache,Key),
				{PP,PPOther} = partials(Path,PPartials),
				{maps:merge(Acc#{Key => Path ++ [PName]},PP),maps:merge(Other,PPOther)};
			_-> %% 非同路径下模板
				Key = {PName,PPath},
				[{Key,PPartials} ] = ets:lookup(ai_mustache,Key),
				{PP,PPOther} = partials(PPath,PPartials),
				{Acc,maps:merge(maps:merge(Other,PP#{Key => Paths}),PPOther)}

		end
	end,{#{},#{}},Partials).



%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	{error, Error :: {already_started, pid()}} |
	{error, Error :: term()} |
	ignore.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	{ok, State :: term(), Timeout :: timeout()} |
	{ok, State :: term(), hibernate} |
	{stop, Reason :: term()} |
	ignore.
init([]) ->
	ai_mustache = ets:new(ai_mustache,[set,named_table,protected,
			{write_concurrency,false},{read_concurrency,true}]),
	ai_mustache_ir = ets:new(ai_mustache_ir,[set,named_table,protected,
	{write_concurrency,false},{read_concurrency,true}]),
	process_flag(trap_exit, true),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	{reply, Reply :: term(), NewState :: term()} |
	{reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	{reply, Reply :: term(), NewState :: term(), hibernate} |
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	{stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
	State :: term()) -> any().
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
	State :: term(),
	Extra :: term()) -> {ok, NewState :: term()} |
	{error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
	Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
	Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
split_path(File)->
	Paths = binary:split(File,<<"/">>,[global]),
	Name = lists:last(Paths),
	Path = lists:droplast(Paths),
	{Paths,Path,Name}.

