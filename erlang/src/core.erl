-module(core).
-export([loader/0, queue/0, queue_length/0, worker/0]).

loader() ->
    timer:sleep(get_loader_sleep()),
    queue ! {work, message},
    loader().

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

worker() ->
    queue ! {worker, self()},
    receive
        _ ->
            timer:sleep(get_worker_sleep())
    end,
    worker().

get_worker_sleep() ->
    utils:getenv_int("ANTS_WORKER_SLEEP").
get_loader_sleep() ->
    utils:getenv_int("ANTS_LOADER_SLEEP").
