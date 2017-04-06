-module(sserl_tabv).

-compile([export_all]).


%% display all dets data
d_all() ->
	Fun = fun (Tab) ->
		PF = fun (Obj, Acc) ->
			io:format("~p: ~p~n", [Acc, Obj]),
			Acc + 1
		end,
		io:format("~p:~n", [Tab]),
		io:format("========================================~n"),
		Total = dets:foldl(PF, 0, Tab),
		if Total =:= 0 ->
				io:format("dont have data~n");
		   true ->
		   		ok
		end,
		io:format("========================================~n~n")
	end,
	lists:map(Fun, dets:all()),
	ok.

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
	