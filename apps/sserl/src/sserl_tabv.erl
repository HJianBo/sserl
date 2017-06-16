-module(sserl_tabv).

-include("sserl.hrl").

-compile([export_all]).


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
	%io:format("~p~n", [Traffics]).

traffic(Port) ->
	Traffics = mnesia:dirty_match_object(traffic, #traffic{port=Port, _='_'}),
	io:format("traffic for  ~p, ~p records~n", [Port, length(Traffics)]),
	io:format("~p~n", [Traffics]).

e_all() ->
	Fun = fun (Tab) ->
		PF = fun (Obj, Acc) ->
			io:format("~p: ~p~n", [Acc, Obj]),
			Acc + 1
		end,
		io:format("~p:~n", [Tab]),
		io:format("========================================~n"),
		Total = ets:foldl(PF, 0, Tab),
		if Total =:= 0 ->
				io:format("dont have data~n");
		   true ->
		   		ok
		end,
		io:format("========================================~n~n")
	end,
	lists:map(Fun, [sserl_flow_traffic]),
	ok.
	