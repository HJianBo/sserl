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


%% FIXME: storage should not be a gen_server

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
	gen_server:cast(?SERVER, {add_port, PortInfo}).


remove_port(Port) ->
	gen_server:cast(?SERVER, {remove_port, Port}).


%% Return :: [portinfo()]
all_ports() ->
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
	% some opertion will return 'already_exists', when had created table or schema
	% now, igore this error
	mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(portinfo,
                        [{attributes, record_info(fields, portinfo)}, 
                         {disc_copies, [node()]},
						 {type, set}]),
	mnesia:create_table(traffic_counter4day,
                        [{attributes, record_info(fields, traffic_counter4day)}, 
                         {disc_copies, [node()]},
						 {type, set}]),
	
    mnesia:create_table(traffic,
                        [{attributes, record_info(fields, traffic)},
                         {disc_copies, [node()]},
						 {type, bag}]),
	
	% Applications need to wait for certain tables to be accessible to do useful work
	case mnesia:wait_for_tables([portinfo, traffic, traffic_counter4day], 5000) of
		ok ->
			{ok, #state{}};
		_ ->
			{stop, mnesia_not_working}
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
handle_call(ports, _From, State) ->
	lager:debug("storage get all ports~n"),
	Reply = mnesia:dirty_match_object(portinfo, #portinfo{_='_'}),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
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
	lager:debug("storage add port: ~p~n", [PortInfo]),
	mnesia:dirty_write(portinfo, PortInfo),
	{noreply, State};

handle_cast({remove_port, Port}, State) ->
	lager:debug("storage remove port: ~p~n", [Port]),

	DelTraffic = 
		fun() ->
			mnesia:delete(portinfo, Port, write),
			DelFunc = 
				fun(Traffic) -> 
					mnesia:delete_object(traffic, Traffic, write)
				end,

			All = mnesia:match_object(traffic, #traffic{port=Port, _='_'}, read),
			lists:foreach(DelFunc, All)
		end,
	case catch mnesia:activity(transaction, DelTraffic) of
		{'EXIT', Reason} ->
			lager:error("remove_port mnesia transaction exit, reason: ~p~n", [Reason]),
			{stop, badtransaction, State};
		ok ->
			{noreply, State}
	end;
	

handle_cast({write_traffic, Traffic=#traffic{time=Timestamp,
						port=Port, down=Down, up=Up}}, State) ->						
	InsertFunc = 
		fun() ->
			Date = get_datestring(Timestamp),
			Key = gen_counterkey(Timestamp, Port),
			mnesia:write(traffic, Traffic, write),
			case mnesia:match_object(traffic_counter4day, #traffic_counter4day{id=Key, _='_'}, read) of
				[] ->
					TD = #traffic_counter4day{id=Key, date=Date, port=Port, down=Down, up=Up},
					mnesia:write(traffic_counter4day, TD, write);
				[Already=#traffic_counter4day{down=HadDown, up=HadUp}] ->
					NewCounter = Already#traffic_counter4day{down=HadDown+Down, up=HadUp+Up},
					mnesia:write(traffic_counter4day, NewCounter, write)
			end
		end,
	
	case catch mnesia:activity(transaction, InsertFunc) of
		{'EXIT', Reason} ->
			lager:error("write_traffic mnesia transaction exit, reason: ~p~n", [Reason]),
			{stop, badtransaction, State};
		ok ->
			{noreply, State}
	end;

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
gen_counterkey(Timestamp, Port) ->
	lists:concat([get_datestring(Timestamp), "@", Port]).
	
get_datestring(Timestamp) ->
	{{Year, Month, Day}, _} = sserl_utils:timestamp_to_datetime(Timestamp),
	lists:flatten(
		io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day])).

	