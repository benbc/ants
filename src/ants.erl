-module(ants).
-export([run/0, start/0, stop/0]).
-export([launcher/0, reaper/0, monitor/0, loader/0]).

run() ->
    start(),
    timer:sleep(get_runtime()),
    stop().

start() ->
    lists:foreach(fun processes:spawn_one/1, [monitor, {module, core, queue}, launcher, reaper, loader]),
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

launcher() ->
    timer:sleep(get_regulator_sleep()),
    case core:queue_length() of
        N when N > 100 ->
            processes:spawn_one({module, core, worker});
        _ ->
            ok
    end,
    launcher().

reaper() ->
    timer:sleep(get_regulator_sleep()),
    case core:queue_length() of
        N when N < 10 ->
            processes:kill_one(worker);
        _ ->
            ok
    end,
    reaper().

time_now() ->
    {MegaSecs, Secs, Microseconds} = now(),
    1000000*MegaSecs + Secs + Microseconds/1000000.

get_runtime() ->
    utils:getenv_int("ANTS_RUNTIME")*1000.
get_loader_sleep() ->
    utils:getenv_int("ANTS_LOADER_SLEEP").
get_regulator_sleep() ->
    utils:getenv_int("ANTS_REGULATOR_SLEEP").
