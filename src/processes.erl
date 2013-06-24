-module(processes).
-export([spawn_one/1, kill_all/1, kill_one/1, all/1]).

spawn_one({module, Module, Type}) ->
    spawn(fun() -> catch register(Type, self()),
                   put(type, Type),
                   apply(Module, Type, [])
          end);
spawn_one(Type) ->
    spawn_one({module, ants, Type}).

kill_all(Type) ->
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, all(Type)).

kill_one(Type) ->
    case all(Type) of
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

all(Type) ->
    lists:filter(fun(Pid) -> is_process(Pid, Type) end, processes()).
