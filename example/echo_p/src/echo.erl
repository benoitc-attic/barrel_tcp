-module(echo).

-export([start/0]).

start() ->
    ok = barrel:start(),
    ok = application:start(echo).
