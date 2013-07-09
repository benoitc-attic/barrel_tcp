-module(echo_handler).


-export([start_link/4]).
-export([init/3]).


start_link(Ref, Transport, Socket, _Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Socket]),
    {ok, Pid}.

init(Ref, Transport, Socket) ->
    ok = barrel:accept_ack(Ref),
    wait_request(Transport, Socket).

wait_request(Transport, Socket) ->
    case  Transport:recv(Socket, 0, 30000) of
        {ok, <<".\r\n">>} ->
            io:format("remote exited", []),
            Transport:close(Socket);
        {ok, Data} ->
            io:format("got ~p~n", [Data]),
            Transport:send(Socket, Data),
            wait_request(Transport, Socket);
        {error, _Reason} ->
            Transport:close(Socket)
    end.
