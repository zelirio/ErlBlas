-module(computation_server).

-export([start_servers/2, stop_servers/1, run/0]).

start_servers(0, Acc) ->
    Acc;

start_servers(N, Acc) ->
    Pid = spawn(computation_server, run, []),
    start_servers(N-1, [Pid|Acc]).

stop_servers(Servers) ->
    case Servers of 
        [] -> ok;
        [H|T] -> H ! {stop},
                 stop_servers(T)
    end.

run() ->
    receive
        {stop} -> ok;
        {Pid, F} -> erlang:display({first, self()}), erlang:display({core, self(), erlang:system_info(scheduler_id)}), A=F(), Pid ! {A, self()}, erlang:display({last, self()}), run()
    end.
