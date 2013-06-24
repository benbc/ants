-module(ants).
-export([run/0, start/0, stop/0]).
-export([monitor/0, loader/0]).

run() ->
    start(),
    timer:sleep(get_runtime()),
    stop().

start() ->
    lists:foreach(fun processes:spawn_one/1, [monitor,
                                              {module, core, queue},
                                              {module, regulators, launcher},
                                              {module, regulators, reaper},
                                              loader]),
    ok.

stop() ->
    lists:foreach(fun processes:kill_all/1, [launcher, reaper, loader, worker, queue, monitor]),
    ok.

monitor() ->
    monitor(time_now()).
monitor(Start) ->
    NumWorkers = length(processes:all(worker)),
    io:format("~w ~w ~w~n", [time_now()-Start, NumWorkers, core:queue_length()]),
    timer:sleep(100),
    monitor(Start).

loader() ->
    timer:sleep(get_loader_sleep()),
    queue ! {work, message},
    loader().

time_now() ->
    {MegaSecs, Secs, Microseconds} = now(),
    1000000*MegaSecs + Secs + Microseconds/1000000.

get_runtime() ->
    utils:getenv_int("ANTS_RUNTIME")*1000.
get_loader_sleep() ->
    utils:getenv_int("ANTS_LOADER_SLEEP").
