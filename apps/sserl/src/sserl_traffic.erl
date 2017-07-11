%%-------------------------------------------------------------------
%% @doc sserl traffic statistics
%% 
%% @author JianBo He <heeejianbo@gmail.com>
%% @end
%%-------------------------------------------------------------------
-module(sserl_traffic).

-behaviour(gen_event).

-include ("sserl.hrl").

%% API
-export([add_handler/0]).

-export([flow_usage/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(FLOW_TRAFFIC_TAB, sserl_flow_traffic).

-record(state, {}).

%%===================================================================
%% API
%%===================================================================

add_handler() ->
	gen_event:add_handler(?TRAFFIC_EVENT, ?MODULE, []).

%% @doc return flow usage in current month
%% Return :: integer()
flow_usage(Port) ->
	% 1. 查询 mnesia traffic_counter4day 表, 将本月每天的用量相加
	% 2. 查询 ets 表, 将当前正在使用的流量相加
	{{Year, Mon, _}, _} = calendar:universal_time(),
    DayMax = calendar:last_day_of_the_month(Year, Mon),
    DayMin = calendar:last_day_of_the_month(Year, Mon-1),

    DateMax = lists:flatten(
		        io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Mon, DayMax])),
    DateMin = lists:flatten(
		        io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Mon-1, DayMin])),

    MatchHead = #traffic_counter4day{port=Port, date='$1', _='_'},
    Guards = [{'=<', '$1', DateMax}, {'>', '$1', DateMin}],
    TCs = mnesia:dirty_select(traffic_counter4day, [{MatchHead, Guards, ['$_']}]),
    
    FunCount = 
        fun(#traffic_counter4day{down=Down, up=Up}, Count) ->
            Count+Down+Up
        end,
    FlowTotal = lists:foldl(FunCount, 0, TCs),
	case ets:match(?FLOW_TRAFFIC_TAB, #traffic{port=Port, _='_'}) of
		[] ->
			FlowTotal;
		ETraffics ->
			lists:foldl(FunCount, FlowTotal, ETraffics)
	end.

%%===================================================================
%% gen_event callbacks
%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	%% 1. get configuration, enable flow traffic
	case application:get_env(traffic_enable) of
		{ok, true} ->
			%% 2. setup/init ets/dets/mnesia
			ets:new(?FLOW_TRAFFIC_TAB, [named_table, {keypos, 2}]),
			lager:debug("initialized sserl_traffic"),
			{ok, #state{}};
		_ ->
			stop
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({sending, Traffic}, State) ->
	lager:debug("report sending traffic ~p traffic: ~p~n", [Traffic#traffic.port, Traffic]),
	saveto_ets(Traffic),
	{ok, State};

handle_event({complete, Traffic=#traffic{id=ConnId, port=Port}}, State) ->
	lager:debug("report end traffic ~p traffic: ~p~n", [Port, Traffic]),
	saveto_ets(Traffic),
	self() ! {save, ConnId},
	{ok, State};

handle_event(Event, State) ->
	lager:debug("unexpected event ~p~n", [Event]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info({save, ConnId}, State) ->
	case ets:lookup(?FLOW_TRAFFIC_TAB, ConnId) of
		[Traffic] ->
			saveto_mnesia(Traffic),
			% delete
			ets:delete(?FLOW_TRAFFIC_TAB, ConnId);
		_ ->
			ok
	end,
	{ok, State};

handle_info(Info, State) ->
    lager:info("info:~p", [Info]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	%% TODO:
	%% save to storage all
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
%% Internel function
%%===================================================================
saveto_ets(TrafficSlice=#traffic{id=ConnId, down=Download, up=Upload}) ->
	case ets:lookup(?FLOW_TRAFFIC_TAB, ConnId) of
		[] ->
			ets:insert(?FLOW_TRAFFIC_TAB, TrafficSlice);
		[_HadTraffic] ->
			ets:update_counter(?FLOW_TRAFFIC_TAB, ConnId, [{6, Download}, {7, Upload}])
	end.

saveto_mnesia(Traffic) ->
	sserl_storage:write_traffic(Traffic).
