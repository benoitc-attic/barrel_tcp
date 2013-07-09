%%% -*- erlang -*-
%%%
%%% This file is part of barrel released under the MIT license.
%%% See the NOTICE for more information.

-module(barrel_acceptor).

-export([start_link/5]).

-export([accept/5]).


start_link(Listener, Transport, ListenSocket, Opts, Protocol) ->
    spawn_link(?MODULE, accept, [Listener, Transport, ListenSocket, Opts,
                                 Protocol]).

%% accept on the socket until a client connect
accept(Listener, Transport, ListenSocket, Opts,
       {ProtocolHandler, ProtoOpts}=Protocol) ->

    AcceptTimeout = proplists:get_value(accept_timeout, Opts, 10000),
    case catch Transport:accept(ListenSocket, AcceptTimeout) of
        {ok, Socket} ->
            gen_server:cast(Listener, {accepted, self()}),
            ProtocolHandler:init(Transport, Socket, ProtoOpts);
        {error, timeout} ->
            ?MODULE:accept(Listener, Transport, ListenSocket, Opts,
                           Protocol);
        {error, econnaborted} ->
            ?MODULE:accept(Listener, Transport, ListenSocket, Opts,
                           Protocol);
        {error, esslaccept} ->
            exit(normal);
        {error, Other} ->
            exit({error, Other})
    end.
