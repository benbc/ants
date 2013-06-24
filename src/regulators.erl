-module(regulators).
-export([launcher/0, reaper/0]).

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

get_regulator_sleep() ->
    utils:getenv_int("ANTS_REGULATOR_SLEEP").
