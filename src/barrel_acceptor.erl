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
       {ProtocolHandler, ProtoOpts}=Protocol) ->

    %% here we call the listener to make sure we can handle the
    %% connection now. if not it will sleep until we get the
    %% answer.
    ok = barrel_listener:start_accepting(Listener),

    AcceptTimeout = proplists:get_value(accept_timeout, Opts, 10000),
    case catch Transport:accept(ListenSocket, AcceptTimeout) of
        {ok, Socket} ->


            gen_server:cast(Listener, {accepted, self()}),
            ProtocolHandler:init(Ref, Transport, Socket, ProtoOpts);
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
