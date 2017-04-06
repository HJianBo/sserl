%% flow event report: {report, Port, Download, Upload}
%% 
%% -define(FLOW_EVENT, sserl_flow_event).

%% stat event: {Sender :: atom(), Event :: any()}
%%      new listener 	{listener, {new, Port}}
%%      listener stoped {listener, {stop, Port}}
%%      accept    :  {listener, {accept, Addr, Port}}
%%
%%      open      :  {conn, {open, Pid}}
%%      close     :  {conn, {close, Pid, Reason}}
%%      connect   :  {conn, {connect, Addr, Port}}
%% 
-define(STAT_EVENT, sserl_stat_event).


%% flow traffic statistics event
%%   - report traffic: {report, Port, Download, Upload}
-define(TRAFFIC_EVENT, flow_traffic_event).