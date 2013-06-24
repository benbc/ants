-module(utils).
-export([getenv_int/1]).

getenv_int(Name) ->
    list_to_integer(os:getenv(Name)).
