%%-------------------------------------------------------------------
%% @author JianBo He <heeejianbo@gmail.com>
%% @doc sserl mutil-user manager udp interface
%% 
%% @end
%%-------------------------------------------------------------------

-module (sserl_mutil).

-behaviour(gen_server).

-include("sserl.hrl").

%% API
-export ([start_link/0, stop/0, status/0]).

%% gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

%% default value with mananger port enable 
-define(MANAGER_ENABLE, false).

%% default value with mananger port
-define(DEFAULT_PORT, 6001).

-record(state, {socket}).

%%===================================================================
%% API
%%===================================================================

%%-------------------------------------------------------------------
%% @doc start sserl_mudp server, when configuration is enable
%% 
%% @spec start_link() -> {ok, Pid}
%% @end
%%-------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


stop() ->
	gen_server:cast(?MODULE, stop).

status() ->
	ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init([]) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	Enable = application:get_env(sserl, mutil_enabled, ?MANAGER_ENABLE),
	Port = application:get_env(sserl, mutil_manager_port, ?DEFAULT_PORT),
	case Enable of 
		true ->
			case gen_udp:open(Port, [binary, {active, true}]) of
				{ok, Socket} ->
					{ok, #state{socket = Socket}};
				{error, Reason} ->
					{stop, Reason}
			end;
		false ->
			{ok, #state{}}
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
handle_cast(stop, State) ->
	case State#state.socket of
		Sock when is_port(Sock) ->
			gen_udp:close(Sock),
			{stop, normal, #state{socket = undefined}};
		_Other ->
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
handle_info({udp, Socket, Addr, Port, RawData}, State) ->
	handle_data({Socket, Addr, Port}, RawData),
	{noreply, State};

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
%% Internel Function
%%===================================================================

%%-------------------------------------------------------------------
%% @doc 
%% Handling udp packet, for example:
%%  - add: {"server_port": 8001, "password":"7cd308cc059"}
%%  - remove: {"server_port": 8001}
%%  - ping
%%  - stat: {"8001":11370}
%% @spec handle_data(SocketInfo, RawData) -> ok | {error, Reason}
%%
%% @end
%%-------------------------------------------------------------------
handle_data({Socket, Addr, Port}, RawData) ->
	gen_event:notify(?STAT_EVENT, {mutil, {recv, {Addr, Port, RawData}}}),
	
	case parse_cmd(RawData) of
		{ok, {Cmd, Data}} ->
			case Cmd of
				ping ->
					gen_udp:send(Socket, Addr, Port, <<"pong">>);
				stat ->
					status();
				add ->
					case sserl_listener_sup:start(Data) of 
						{ok, _Pid} ->
							gen_udp:send(Socket, Addr, Port, <<"ok">>);
						{error, {badargs, Reason}} ->
							gen_udp:send(Socket, Addr, Port, atom_to_binary(Reason, utf8));
						{error, Other} ->
							gen_udp:send(Socket, Addr, Port, atom_to_binary(Other, utf8))
					end;
				remove ->
					SSPort = proplists:get_value(port, Data),
					ok = sserl_listener_sup:stop(SSPort),
					gen_udp:send(Socket, Addr, Port, <<"ok">>)
			end;
		{error, Reason} ->
			lager:info("parse recved data: ~p, error: ~p~n", [RawData, Reason])
	end.

parse_cmd(RawData) when is_binary(RawData)->
	Res = binary:split(RawData, <<":">>),
	case length(Res) of
		1 ->
			[Cmd|_] = Res,
			case Cmd of
				<<"ping">> -> {ok, {ping, nodata}};
				<<"stat">> -> {ok, {stat, nodata}};
				_ -> {error, "invaild cmd"}
			end;
		2 ->
			[Cmd | Data] = Res,
			case Cmd  of
				<<"add">> ->
					[Data2|_] = Data,
					{ok, Data3} = parse_data(Data2),
					{ok, {add, Data3}};
				<<"remove">> ->
					[Data2|_] = Data,
					{ok, Data3} = parse_data(Data2),
					{ok, {remove, Data3}};
				_ ->
					{error, "invaild cmd"}
			end;
		_ ->
			{error, "invaild cmd"}
	end.


%% -> {ok, Data}| {error, Reason}
%%		Data = proplist
parse_data(Data) ->
	Portinfo = jsx:decode(Data),
	Func = fun(P, PL) ->
		{K, V} = P,
		K2 = binary_to_atom(K, utf8),
		[{K2, normalized_value(V)} | PL]
	end,
	PL = lists:foldl(Func, [], Portinfo),
	{ok, PL}.

normalized_value(V) when is_binary(V) ->
	binary_to_list(V);
normalized_value(V) when is_atom(V) ->
	V;
normalized_value(V) when is_integer(V) ->
	V.
