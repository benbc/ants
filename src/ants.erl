-module(ants).
-export([run/0, start/0, stop/0]).
-export([queue/0, launcher/0, reaper/0, worker/0, monitor/0, loader/0]).

run() ->
    start(),
    timer:sleep(get_runtime()),
    stop().

start() ->
    lists:foreach(fun spawn_process/1, [monitor, queue, launcher, reaper, loader]),
    ok.

stop() ->
    lists:foreach(fun kill_processes/1, [launcher, reaper, loader, worker, queue, monitor]),
    ok.

monitor() ->
    monitor(time_now()).
monitor(Start) ->
    NumWorkers = length(all_processes(worker)),
    io:format("~w ~w ~w~n", [time_now()-Start, NumWorkers, queue_length()]),
    timer:sleep(100),
    monitor(Start).

queue() ->
    receive
        {work, Work} ->
            queue(Work);
        Other ->
            self() ! Other,
            queue()
    end.
queue(Work) ->
    receive
        {worker, Worker} ->
            Worker ! Work,
            queue();
        Other ->
            self() ! Other,
            queue(Work)
    end.
queue_length() ->
    case whereis(queue) of
        undefined ->
            0;
        Pid ->
            {messages, Messages} = process_info(Pid, messages),
            Work = lists:filter(fun({work, _}) -> true; (_) -> false end, Messages),
            length(Work)
    end.

loader() ->
    timer:sleep(get_loader_sleep()),
    queue ! {work, message},
    loader().

launcher() ->
    timer:sleep(get_regulator_sleep()),
    case queue_length() of
        N when N > 100 ->
            spawn_process(worker);
        _ ->
            ok
    end,
    launcher().

reaper() ->
    timer:sleep(get_regulator_sleep()),
    case queue_length() of
        N when N < 10 ->
            kill_one_process(worker);
        _ ->
            ok
    end,
    reaper().

worker() ->
    queue ! {worker, self()},
    receive
        _ ->
            timer:sleep(get_worker_sleep())
    end,
    worker().

spawn_process(Type) ->
    spawn(fun() -> catch register(Type, self()),
                   put(type, Type),
                   apply(?MODULE, Type, [])
          end).
kill_processes(Type) ->
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, all_processes(Type)).
kill_one_process(Type) ->
    case all_processes(Type) of
        [] ->
            ok;
        [Pid | _ ] ->
            exit(Pid, kill)
    end.
is_process(Pid, Type) ->
    case process_info(Pid, dictionary) of
        {dictionary, Dict} ->
            Type == proplists:get_value(type, Dict);
        undefined ->
            false
    end.
all_processes(Type) ->
    lists:filter(fun(Pid) -> is_process(Pid, Type) end, processes()).

time_now() ->
    {MegaSecs, Secs, Microseconds} = now(),
    1000000*MegaSecs + Secs + Microseconds/1000000.

get_runtime() ->
    getenv_int("ANTS_RUNTIME")*1000.

get_loader_sleep() ->
    getenv_int("ANTS_LOADER_SLEEP").

get_worker_sleep() ->
    getenv_int("ANTS_WORKER_SLEEP").

get_regulator_sleep() ->
    getenv_int("ANTS_REGULATOR_SLEEP").

getenv_int(Name) ->
    list_to_integer(os:getenv(Name)).