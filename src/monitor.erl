-module(monitor).
-export([monitor/0]).

monitor() ->
    monitor(time_now()).
monitor(Start) ->
    NumWorkers = length(processes:all(worker)),
    io:format("~w ~w ~w~n", [time_now()-Start, NumWorkers, core:queue_length()]),
    timer:sleep(100),
    monitor(Start).

time_now() ->
    {MegaSecs, Secs, Microseconds} = now(),
    1000000*MegaSecs + Secs + Microseconds/1000000.
