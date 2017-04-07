%%-------------------------------------------------------------------
%% @doc sserl local storage for saving server opened port and traffic 
%%      statistics
%% 
%% @author JianBo He <heeejianbo@gmail.com>
%% 
%% @end
%%-------------------------------------------------------------------

-module(sserl_storage).

-behaviour(gen_server).

-include("sserl.hrl").

%% API
-export([start_link/0, add_port/1, remove_port/1, all_ports/0, incrs_traffic/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(PORT_TAB, port).
-define(TRAFFIC_TAB, traffic).
-define(DATA_PORT_FILE, "./data/port.dets").
-define(DATA_TRAFFIC_FILE, "./data/traffic.dets").

-record(state, {}).


%%===================================================================
%% API
%%===================================================================

%%-------------------------------------------------------------------
%% @doc start server
%% 
%% @spec start_link() -> {ok, Pid}
%%
%% @end
%%-------------------------------------------------------------------
start_link() -> 
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%-------------------------------------------------------------------
%% @doc insert port info to dets, or updating if has existed
%% 
%% @spec function -> return
%% where
%%  return
%% @end
%%-------------------------------------------------------------------
add_port(PortInfo) ->
	lager:debug("storage add port: ~p~n", [PortInfo]),
	gen_server:cast(?SERVER, {add_port, PortInfo}).


remove_port(Port) ->
	lager:debug("storage remove port: ~p~n", [Port]),
	gen_server:cast(?SERVER, {remove_port, Port}).


%% Return :: [portinfo()]
all_ports() ->
	lager:debug("storage get all ports~n"),
	gen_server:call(?SERVER, ports).

%%-------------------------------------------------------------------
%% @doc storate flow traffic to dets
%% 
%% @spec incrs_traffic(Port, Download, Upload) -> ok
%%
%% @end
%%-------------------------------------------------------------------
incrs_traffic(Port, Download, Upload) ->
	lager:debug("storage incrs ~p traffic D: ~pB, U: ~pB~n", [Port, Download, Upload]),
	gen_server:cast(?SERVER, {incrs_traffic, {Port, Download, Upload}}).



%%===================================================================
%% gen_server callbacks
%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	case filelib:ensure_dir(?DATA_PORT_FILE) of
		ok ->
			{ok, _} = dets:open_file(?PORT_TAB, [{file, ?DATA_PORT_FILE},
												 {auto_save, 60 * 1000},
												 {keypos, #portinfo.port}]),
			{ok, _} = dets:open_file(?TRAFFIC_TAB, [{file, ?DATA_TRAFFIC_FILE}, {auto_save, 60 * 1000}]),
			{ok, #state{}};
		{error, Reason} ->
			{stop, Reason}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------	
handle_call(_Request, _From, State) ->
	Func = fun (PortInfo, Acc) ->
		[PortInfo | Acc]
    end,
    Reply = dets:foldl(Func, [], ?PORT_TAB),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add_port, PortInfo}, State) -> 
	dets:insert(?PORT_TAB, PortInfo),
	{noreply, State};

handle_cast({remove_port, Port}, State) -> 
	dets:delete(?PORT_TAB, Port),
	dets:delete(?TRAFFIC_TAB, Port),
	{noreply, State};
	
handle_cast({incrs_traffic,{Port, D, U}}, State) ->
	case dets:lookup(?TRAFFIC_TAB, Port) of
		[] ->
			dets:insert(?TRAFFIC_TAB, {Port, D, U});
		_ ->
			dets:update_counter(?TRAFFIC_TAB, Port, {2, D}),
			dets:update_counter(?TRAFFIC_TAB, Port, {3, U})
	end,
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal function
%%===================================================================
