%%%-------------------------------------------------------------------
%% @doc sserl listener supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sserl_listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start/1, stop/1, running_ports/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Return :: {ok, Pid} | {error, Reason} | {error, {badargs, Reason}}
start(Args) ->
    Port = proplists:get_value(port, Args),
    Children = supervisor:which_children(?SERVER),
    case [P || {_, P, _, _} <- Children, is_pid(P), sserl_listener:get_port(P) =:= Port] of
        [Pid] ->
            lager:info("[~p] updating port ~p", [?MODULE, Args]),
            sserl_listener:update(Pid, Args);
        _ ->
            lager:info("[~p] starting port ~p", [?MODULE, Args]),
            supervisor:start_child(?SERVER, [Args])
    end.

%% Return :: ok
stop(Port) ->
    Children = supervisor:which_children(?SERVER),
    case [P || {_, P, _, _} <- Children, is_pid(P), sserl_listener:get_port(P) =:= Port] of
        [Pid] ->
            lager:info("[~p] stopping port ~p", [?MODULE, Port]),
            supervisor:terminate_child(?SERVER, Pid),
            ok;
        _ ->
            lager:info("[~p] port ~p is not running, stop nothing", [?MODULE, Port]),
            ok
    end.    

%% Return :: [number()]
running_ports() ->
    Children = supervisor:which_children(?SERVER),
    [sserl_listener:get_port(P) || {_, P, _, _} <- Children, is_pid(P)].

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    %  Shutdown = 500 ms
    {ok, { {simple_one_for_one, 1, 5}, 
           [{sserl_listener, {sserl_listener, start_link, []},
           transient, 500, worker, [sserl_listener]}]} }.

%%====================================================================
%% Internal functions
%%====================================================================
