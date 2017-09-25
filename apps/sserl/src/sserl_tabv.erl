%%-----------------------------------------------------------------------
%% @doc for review saved data
%% 
%% @end
%%-----------------------------------------------------------------------
-module(sserl_tabv).

-include("sserl.hrl").

-export([traffic_counter/0, traffic_counter/1,
		 traffic/0, traffic/1]).

traffic_counter() ->
	TCs = mnesia:dirty_match_object(traffic_counter4day, #traffic_counter4day{_='_'}),
	io:format("traffic_counter4day, ~p records~n", [length(TCs)]),
	io:format("~p~n", [TCs]).

traffic_counter(Port) ->
	TCs = mnesia:dirty_match_object(traffic_counter4day, #traffic_counter4day{port=Port, _='_'}),
	io:format("traffic_counter4day for ~p, ~p records~n", [Port, length(TCs)]),
	io:format("~p~n", [TCs]).

traffic() ->
	Traffics = mnesia:dirty_match_object(traffic, #traffic{_='_'}),
	io:format("traffic all data, ~p records~n", [length(Traffics)]).

traffic(Port) ->
	Traffics = mnesia:dirty_match_object(traffic, #traffic{port=Port, _='_'}),
	io:format("traffic for  ~p, ~p records~n", [Port, length(Traffics)]).
