%%% -*- erlang -*-
%%%
%%% This file is part of barrel released under the MIT license.
%%% See the NOTICE for more information.

-module(barrel_acceptor).

-export([start_link/5]).

-export([accept/6]).


start_link(Listener, Transport, ListenSocket, Opts, Protocol) ->
    Ref = proplists:get_value(ref, Opts),

    spawn_link(?MODULE, accept, [Listener, Ref, Transport, ListenSocket, Opts,
                                 Protocol]).

%% accept on the socket until a client connect
accept(Listener, Ref, Transport, ListenSocket, Opts,
       {ProtocolHandler, ProtoOpts, Spawn}=Protocol) ->

    %% here we call the listener to make sure we can handle the
    %% connection now. if not it will sleep until we get the
    %% answer.
    ok = barrel_listener:start_accepting(Listener),

    AcceptTimeout = proplists:get_value(accept_timeout, Opts, 10000),
    case catch Transport:accept(ListenSocket, AcceptTimeout) of
        {ok, Socket} when Spawn /= true ->
            gen_server:cast(Listener, {accepted, self()}),
            ProtocolHandler:init(Ref, Transport, Socket, ProtoOpts);
        {ok, Socket} ->
            %% The protocol want to spawn a new process instead of using
            %% the acceptor one so pass it the control of the socket
            case ProtocolHandler:start_link(Ref, Transport, Socket,
                                            ProtoOpts) of
                {ok, Pid} ->
                    ok = Transport:controlling_process(Socket, Pid),
                    gen_server:cast(Listener, {accepted, Pid}),
                    Pid ! {accept_ack, Ref};
                _ ->
                    ok
            end,
            %% we don't have to exit here since we spwaned a new process
            %% to handle the request
            ?MODULE:accept(Listener, Ref, Transport, ListenSocket, Opts,
                           Protocol);
        {error, timeout} ->
            ?MODULE:accept(Listener, Ref, Transport, ListenSocket, Opts,
                           Protocol);
        {error, econnaborted} ->
            ?MODULE:accept(Listener, Ref, Transport, ListenSocket, Opts,
                           Protocol);
        {error, esslaccept} ->
            exit(normal);
        {error, Other} ->
            exit({error, Other})
    end.
