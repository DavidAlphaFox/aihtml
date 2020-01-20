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

-export([template/1]).
-export([bootstrap/0,bootstrap/1]).
-export([reload/0]).

-define(SERVER, ?MODULE).

-record(state, {view_path,suffix = <<".mustache">>}).

%%%===================================================================
%%% API
%%%===================================================================
reload() ->
   gen_server:call(?SERVER, reload).

template(Template)->
   Name = erlang:binary_to_atom(ai_string:to_string(Template), utf8),
   TKey = template_key(Name),
   CKey = code_key(Name),
   Match = ets:lookup(ai_mustache,TKey),
   case Match of
        [] -> 
            ok = load(Name),
            template(Name);
        [{TKey,Partials}]->
            PK = template_partial(Partials),
            M1 = lists:foldl(fun({I,V},Acc)->
                                     [{I,IR}] = ets:lookup(ai_mustache,I),
                                     maps:put(V,IR,Acc)
                             end,#{},maps:to_list(PK)),
            [{CKey,TIR}] = ets:lookup(ai_mustache,CKey),
            {TIR,M1}
    end.

template_partial(Partials)->
    lists:foldl(
      fun(P,Acc) ->
              TKey = template_key(P),
              CKey = code_key(P),
              [{TKey,PPartials}] = ets:lookup(ai_mustache,TKey),
              NewPartials = template_partial(PPartials),
              maps:merge(Acc#{CKey => P},NewPartials)
      end,#{},Partials).

load(Template)-> gen_server:call(?SERVER,{load,Template}).
bootstrap()-> gen_server:call(?SERVER,{bootstrap,undefined,undefined}).
bootstrap(Settings)-> 
    ViewPath = maps:get(views,Settings,undefined),
    Suffix = maps:get(suffix,Settings,undefined),
    gen_server:call(?SERVER,{bootstrap,ViewPath,Suffix}).


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
            TKey = template_key(ai_string:to_string(Template)),
            case ets:lookup(ai_mustache,TKey) of
                [] -> load(Template,ViewPath,Suffix);
                _ -> ok
            end
        catch
            _Error:Reason -> {error,Reason}
        end,
    {reply,Reply,State};
handle_call(reload,_From,#state{suffix = Suffix, view_path = ViewPath} = State)->
   Reply =
      try
         ets:delete_all_objects(ai_mustache),
         bootstrap_load(ViewPath,Suffix)
      catch
         _Error:Reason -> { error,Reason }
      end,
   {reply,Reply,State};

handle_call({bootstrap,ViewPath0,Suffix0},_From,#state{suffix = Suffix, view_path = ViewPath} = State)->
    ViewPath1 =
      if ViewPath0 == undefined -> ViewPath;
         true -> ai_string:to_string(ViewPath0)
      end,
    Suffix1 =
      if Suffix0 == undefined -> Suffix;
         true -> ai_string:to_string(Suffix0)
      end,
    Reply = 
        try
            bootstrap_load(ViewPath1,Suffix1)
        catch
            _Error:Reason -> {error,Reason}
        end,
    {reply,Reply,
     State#state{view_path = ViewPath1,suffix = Suffix1}
    };

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
template_key(Name)->{t,Name}.
code_key(Name)->{c,Name}.

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

load(Template,ViewPath,Suffix)->
   File = filename:join(ViewPath,Template),
   Name = erlang:binary_to_atom(remove_suffix(Template,Suffix),utf8),
   case file:read_file(File) of
      {ok,Body}->
         {IR,Partials} = ai_mustache_parser:parse(Body),
         TKey = template_key(Name),
         CKey = code_key(Name),
         ok = load_partial(Partials,ViewPath,Suffix),
         ets:insert(ai_mustache,{CKey,IR}),
         ets:insert(ai_mustache,{TKey,Partials}),
         ok;
      Error -> Error
    end.


load_partial([],_ViewPath,_Suffix)-> ok;
load_partial([H|T],ViewPath,Suffix)->
   File = ai_string:to_string(H),
   Name = erlang:binary_to_atom(remove_suffix(File,Suffix),utf8),
   TKey = template_key(Name),
   case ets:lookup(ai_mustache,TKey) of
      [] -> load(File,ViewPath,Suffix);
      _ -> ok
   end,
   load_partial(T,ViewPath,Suffix).


%% 找出特定目录下所有的文件
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


bootstrap_templates(Files, Prefix,Suffix) ->
    lists:foldl(
      fun(I0,Acc)->
              I = ai_string:to_string(I0),
              case has_suffix(I,Suffix) of
                  false -> Acc;
                  _->
                      T0 = string:prefix(I,Prefix),
                      [T0|Acc]
              end
      end,[],Files).

bootstrap_load(ViewPath,Suffix)->
    MaybeFiles = recursive_dir(ViewPath),
    case MaybeFiles of
        {error,_} -> MaybeFiles;
        {ok,Files}->
            Prefix0 = filename:join(ViewPath,<<"./">>),
            Prefix = <<Prefix0/binary,"/">>,
            Templates = bootstrap_templates(Files,Prefix,Suffix),
            lists:foldl(
              fun(Template,Acc)->
                      case Acc of
                          ok -> load(Template,ViewPath,Suffix);
                          _ -> Acc
                      end
              end,ok,Templates)
   end.
