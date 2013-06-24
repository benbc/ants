-module(ants).
-export([run/0, start/0, stop/0]).

run() ->
    start(),
    timer:sleep(get_runtime()),
    stop().

start() ->
    lists:foreach(fun processes:spawn_one/1, [{monitor, monitor},
                                              {core, queue},
                                              {regulators, launcher},
                                              {regulators, reaper},
                                              {core, loader}]),
    ok.

stop() ->
    lists:foreach(fun processes:kill_all/1, [launcher, reaper, loader, worker, queue, monitor]),
    ok.

get_runtime() ->
    utils:getenv_int("ANTS_RUNTIME")*1000.
