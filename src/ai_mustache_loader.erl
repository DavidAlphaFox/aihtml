%%%-------------------------------------------------------------------
%%% @author  <david@laptop-02.local>
%%% @copyright (C) 2018, 
%%% @doc
%%% 模板加载器，单写入ets
%%% @end
%%% Created : 19 Dec 2018 by  <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ai_mustache_loader).
-compile({inline, [remove_suffix/2,add_suffix/2,has_suffix/2,partial_file/3]}).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3, format_status/2]).

-export([template/1]).
-export([prepare/0,prepare/1,prepare/2]).

-define(SERVER, ?MODULE).

-record(state, {view_path,suffix = <<".mustache">>}).

%%%===================================================================
%%% API
%%%===================================================================
template(Template)->
    TBin = ai_string:to_string(Template),
    {_Paths,Path,Name} = split_path(TBin),
    Key = {Name,Path},
    Match = ets:lookup(ai_mustache,Key),
    case Match of 
        [] -> 
            ok = load(TBin),
            template(TBin);
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

load(Template)-> gen_server:call(?SERVER,{load,Template}).
prepare()-> gen_server:call(?SERVER,prepare).
prepare(ViewPath)-> gen_server:call(?SERVER,{prepare,ViewPath}).
prepare(ViewPath,Suffix)-> gen_server:call(?SERVER,{prepare,ViewPath,Suffix}).


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
    State = #state{},
    {ok,CWD} = file:get_cwd(),
    ViewPath = filename:join(CWD,"views"),
    {ok,State#state{view_path = ai_string:to_string(ViewPath)}}.

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
handle_call({load,Template},_From,#state{view_path = ViewPath,suffix = Suffix} = State)->
    Reply = 
        try
            {_Paths,Path,Name} = split_path(Template),
            case ets:lookup(ai_mustache,{Name,Path}) of 
                [] -> load(ViewPath,Template,Suffix);
                _ -> ok
            end
        catch
            _Error:Reason -> {error,Reason}
        end,
    {reply,Reply,State};
handle_call({prepare,ViewPath0},_From,#state{suffix = Suffix} = State)->
    ViewPath = ai_string:to_string(ViewPath0),
    Reply = 
        try
            prepare_loader(ViewPath,Suffix)
        catch
            _Error:Reason -> {error,Reason}
        end,
    {reply,Reply,State#state{view_path = ViewPath}};
handle_call({prepare,ViewPath0,Suffix0},_From,State)->
    ViewPath = ai_string:to_string(ViewPath0),
    Suffix = ai_string:to_string(Suffix0),
    Reply = 
        try
            prepare_loader(ViewPath,Suffix)
        catch
            _Error:Reason -> {error,Reason}
        end,
    {reply,Reply,
     State#state{view_path = ViewPath,suffix = Suffix}
    };
handle_call(prepare,_From,#state{view_path = ViewPath,suffix = Suffix}= State)->
    Reply = 
        try
            prepare_loader(ViewPath,Suffix)
        catch
            _Error:Reason -> {error,Reason}
        end,
    {reply,Reply,State};
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
remove_suffix(Name,Suffix)->
    if 
        erlang:byte_size(Suffix) > 0 -> binary:replace(Name,Suffix,<<"">>);
        true -> Name 
    end.
has_suffix(Name,Suffix)->
    if 
        erlang:byte_size(Suffix) > 0 -> 
            case string:find(Name,Suffix,trailing) of 
                nomatch -> false;
                _ -> true 
            end;
        true -> true 
    end.
add_suffix(Name,Suffix)-> 
    if 
        erlang:byte_size(Suffix) > 0 -> <<Name/binary,Suffix/binary>>;
        true -> Name 
    end.
	
partial_file(ViewPath,Path,Name)->
    case Path of 
        [] -> filename:join(ViewPath,<<"_",Name/binary>>);
        _ -> 
            filename:join([ViewPath,
                           filename:join(Path),<<"_",Name/binary>>])
    end.
load(ViewPath,Template,Suffix)->
    RT = add_suffix(Template,Suffix),
    File = filename:join(ViewPath,RT),
    case file:read_file(File) of 
        {ok,Body}->
            {IR,Partials} = ai_mustache_parser:parse(Body),
            {_Paths,Path,Name} = split_path(Template),
            Key = {Name,Path},
            ok = load(Partials,ViewPath,Path,Suffix),
            ets:insert(ai_mustache_ir,{Key,IR}),
            ets:insert(ai_mustache,{Key,Partials}),
            ok;
        Error -> Error 
	end.
			
load([],_ViewPath,_Path,_Suffix)->ok;
load([H|T],ViewPath,Path,Suffix)->
    {_PPaths,PPath,PName} = split_path(H),
    PRName = add_suffix(PName,Suffix),
    case PPath of 
        [] ->
            case ets:lookup(ai_mustache,{PName,Path}) of 
                [] -> 
                    File = partial_file(ViewPath,Path,PRName),
                    case file:read_file(File) of 
                        {ok,Body}->
                            {IR,Partials} = ai_mustache_parser:parse(Body),
                            Key = {PName,Path},
                            load(Partials,ViewPath,Path,Suffix),
                            load(T,ViewPath,Path,Suffix),
                            ets:insert(ai_mustache_ir,{Key,IR}),
                            ets:insert(ai_mustache,{Key,Partials});
                        Error -> Error 
                    end;
                _ -> load(T,ViewPath,Path,Suffix)
            end;
        _ ->
            case ets:lookup(ai_mustache,{PName,PPath}) of 
                [] -> 
                    File = partial_file(ViewPath,PPath,PRName),
                    case file:read_file(File) of 
                        {ok,Body}->
                            {IR,Partials} = ai_mustache_parser:parse(Body),
                            Key = {PName,PPath},
                            load(Partials,ViewPath,PPath,Suffix),
                            load(T,ViewPath,Path,Suffix),
                            ets:insert(ai_mustache_ir,{Key,IR}),
                            ets:insert(ai_mustache,{Key,Partials});
                        Error -> Error 
                    end;
                _ ->
                    load(T,ViewPath,Path,Suffix)
            end
    end.


recursive_dir(Dir) ->
    recursive_dir(Dir, true). % default value of FilesOnly is true

recursive_dir(Dir, FilesOnly) ->
    case filelib:is_file(Dir) of
        true ->
            case filelib:is_dir(Dir) of
                true -> {ok, recursive_dir([Dir], FilesOnly, [])};
                false -> {error, enotdir}
            end;
        false -> {error, enoent}
    end.



recursive_dir([], _FilesOnly, Acc) -> Acc;
recursive_dir([Path|Paths], FilesOnly, Acc) ->
   case filelib:is_dir(Path) of
        false -> recursive_dir(Paths,FilesOnly,[Path | Acc]);
        true ->
        	{ok, Listing} = file:list_dir(Path),
			SubPaths = [filename:join(Path, Name) || Name <- Listing],
			Acc0 = case FilesOnly of
					true -> Acc;
					false -> [Path | Acc]
			end,
			recursive_dir(Paths ++ SubPaths, FilesOnly,Acc0)
	end.


prepare_loader(ViewPath,Suffix)->
    MaybeFiles = recursive_dir(ViewPath),
    case MaybeFiles of 
        {error,_} -> MaybeFiles;
        {ok,Files}->
            Prefix0 = filename:join(ViewPath,<<"./">>),
            Prefix = <<Prefix0/binary,"/">>,
            Templates = 
                lists:foldl(fun(I0,Acc)->
                                    I = ai_string:to_string(I0),
                                    case has_suffix(I,Suffix) of 
                                        false -> Acc;
                                        _->
                                            %% 找非子模板                 
                                            case string:find(I,<<"_">>,leading) of 
                                                nomatch ->
                                                    T0 = string:prefix(I,Prefix),
                                                    T1 = remove_suffix(T0,Suffix),
                                                    [T1|Acc];
                                                _ -> Acc 
                                            end
                                    end
                            end,[],Files),
            lists:foldl(fun(Template,Acc)->
                                case Acc of 
                                    ok -> load(ViewPath,Template,Suffix);
                                    _ -> Acc 
                                end
                        end,ok,Templates)
    end.
	
