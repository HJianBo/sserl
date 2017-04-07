%% flow event report: {report, Port, Download, Upload}
%% 
%% -define(FLOW_EVENT, sserl_flow_event).


%% 使用该数据结构, 规范代码中, 对该结构的引用
-record(portinfo, {port,					% shadowsocks port
				   password,				% shadowsocks password
				   method = "rc4-md5",		% default encrypt method
				   expire_time,				% [optional] server must disable port when expired
				   conn_limit = 1,			% [require ] supported max connection for port
				   max_flow = 10*1024*1024,	% [optional] 
				   ota 	= false,			% [require ]
				   type = server,			% [require ] server / client
				   %ip = {0,0,0,0},			% [optional] listen ip
				   server					% [optional] shadowsocks server (client only)
				   }).

%% stat event: {Sender :: atom(), Event :: any()}
%%      new listener 	{listener, {new, portinfo()}}
%%	    update listener {listener, {update, portinfo()}}
%%      stop listener   {listener, {stop, Port}}
%%
%%      accept    :  	{listener, {accept, Addr, Port}}
%%
%%      open      :  {conn, {open, Pid}}
%%      close     :  {conn, {close, Pid, Reason}}
%%      connect   :  {conn, {connect, Addr, Port}}
%% 
-define(STAT_EVENT, sserl_stat_event).


%% flow traffic statistics event
%%   - report traffic: {report, Port, Download, Upload}
-define(TRAFFIC_EVENT, flow_traffic_event).
