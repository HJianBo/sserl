%% flow event report: {report, Port, Download, Upload}
%% 
%% -define(FLOW_EVENT, sserl_flow_event).

% rd(portinfo, {port,password,method,expire_time,conn_limit,max_flow,ota,type,server})
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
% rd(traffic, {id, port,source,target,down,up,time}).
-record(traffic, {id,			% id = rand number
				  port, 
                  source,
                  target,
                  down,
                  up,
                  time}).

% rd(traffic_counter4day, {id,date,port,down,up}).
-record(traffic_counter4day, {id,				% id = date+port
							  date,
							  port,
							  down = 0,
							  up = 0}).

%% stat event: {Sender :: atom(), Event :: any()}
%%      new listener 	{listener, {new, portinfo()}}
%%	    update listener {listener, {update, portinfo()}}
%%      stop listener   {listener, {stop, Port}}
%%
%%      accept    :  	{listener, {accept, Port, ClientAddr}}
%%
%%      open      :  {conn, {open, Pid}}
%%      close     :  {conn, {close, Pid, Reason}}
%%      connect   :  {conn, {connect, Pid, Source, Tagert}}
%% 
-define(STAT_EVENT, sserl_stat_event).


%% flow traffic statistics event
%%	 - report traffic: {complete, traffic()}
-define(TRAFFIC_EVENT, flow_traffic_event).
