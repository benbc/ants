-module(core).
-export([queue/0, queue_length/0]).

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
