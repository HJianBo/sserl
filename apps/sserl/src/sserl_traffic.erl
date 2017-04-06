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

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(FLOW_TRAFFIC_TAB, sserl_flow_traffic).

-define(SWAP_MIN, 1024). % 1KB

-record(flow, {port = undefined,
			   download = 0,
			   upload = 0}).

-record(state, {}).

%%===================================================================
%% API
%%===================================================================

add_handler() ->
	gen_event:add_handler(?TRAFFIC_EVENT, ?MODULE, []).


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
			ets:new(?FLOW_TRAFFIC_TAB, [named_table]),
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
handle_event({report, Port, Download, Upload}, State) ->
	lager:debug("report traffic ~p: d: ~p, u: ~p~n", [Port, Download, Upload]),
	case ets:lookup(?FLOW_TRAFFIC_TAB, Port) of
		[] ->
			ets:insert(?FLOW_TRAFFIC_TAB, {Port, Download, Upload});
		_ ->
			ets:update_counter(?FLOW_TRAFFIC_TAB, Port, [{2, Download}, {3, Upload}])
	end,
	%% TODO:
    %% 1. update ets counter
    %% 2. maybe replace traffic to storage
    self() ! {swap, Port},
    %% 3. maybe disable port

    {ok, State};
handle_event(_Event, State) ->
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
handle_info({swap, Port}, State) ->
	%% maybe swap ets statistic data to disk
	case ets:lookup(?FLOW_TRAFFIC_TAB, Port) of
		[{_, D, U}] when D + U >= ?SWAP_MIN ->
			do_swap(Port, D, U),
			% reset to zero
			ets:insert(?FLOW_TRAFFIC_TAB, {Port, 0, 0});
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


do_swap(Port, D, U) ->
	sserl_storage:incrs_traffic(Port, D, U).


