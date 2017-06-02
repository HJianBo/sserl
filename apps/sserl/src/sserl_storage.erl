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
-export([start_link/0, add_port/1, remove_port/1, all_ports/0, write_traffic/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

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

write_traffic(Traffic) ->
	lager:debug("write traffic~p~n", [Traffic]),
	gen_server:cast(?SERVER, {write_traffic, Traffic}).


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
	mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(portinfo,
                        [{attributes, record_info(fields, portinfo)}, 
                         {disc_copies, [node()]}]),
    mnesia:create_table(traffic,
                        [{attributes, record_info(fields, traffic)},
                         {disc_copies, [node()]}]),
	
	{ok, #state{}}.
	% case filelib:ensure_dir(?DATA_PORT_FILE) of
	% 	ok ->
	% 		{ok, _} = dets:open_file(?PORT_TAB, [{file, ?DATA_PORT_FILE},
	% 											 {auto_save, 60 * 1000},
	% 											 {keypos, #portinfo.port}]),
	% 		{ok, _} = dets:open_file(?TRAFFIC_TAB, [{file, ?DATA_TRAFFIC_FILE},
	% 												{auto_save, 60 * 1000}]),
	% 		{ok, #state{}};
	% 	{error, Reason} ->
	% 		{stop, Reason}
	% end.

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
handle_call(ports, _From, State) ->
	% Func = fun (PortInfo, Acc) ->
	% 	[PortInfo | Acc]
    % end,
    % Reply = dets:foldl(Func, [], ?PORT_TAB),
	Reply = [],
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
	% Func = fun (PortInfo, Acc) ->
	% 	[PortInfo | Acc]
    % end,
    % Reply = dets:foldl(Func, [], ?PORT_TAB),
	Reply = ok,
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
	mnesia:dirty_write(portinfo, PortInfo),
	{noreply, State};

handle_cast({remove_port, Port}, State) ->
	%% XXX:
	mnesia:dirty_delete(portinfo, Port),
	mnesia:dirty_delete(traffic, Port),
	{noreply, State};

handle_cast({write_traffic, Traffic}, State) ->
	mnesia:dirty_write(traffic, Traffic),
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
