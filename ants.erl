-module(ants).
-export([run/0, start/0, stop/0]).
-export([queue/0, launcher/0, reaper/0, worker/0, monitor/0, loader/0]).

run() ->
    start(),
    timer:sleep(60000),
    stop().

start() ->
    lists:foreach(fun spawn_process/1, [monitor, queue, launcher, reaper, loader]),
    ok.

stop() ->
    lists:foreach(fun kill_processes/1, [launcher, reaper, loader, queue, worker, monitor]),
    ok.

monitor() ->
    monitor(unixtime()).
monitor(Start) ->
    NumWorkers = length(all_processes(worker)),
    io:format("~w ~w ~w~n", [unixtime()-Start, NumWorkers, queue_length()]),
    timer:sleep(1000),
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
    timer:sleep(5),
    queue ! {work, message},
    loader().

launcher() ->
    case queue_length() of
        N when N > 100 ->
            spawn_process(worker);
        _ ->
            ok
    end,
    launcher().

reaper() ->
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
            timer:sleep(5000)
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

unixtime() ->
    {MegaSecs, Secs, _} = now(),
    100000*MegaSecs + Secs.
