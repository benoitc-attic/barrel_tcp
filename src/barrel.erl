%%% -*- erlang -*-
%%%
%%% This file is part of barrel released under the MIT license.
%%% See the NOTICE for more information.

-module(barrel).

-export([start/0, stop/0]).
-export([start_listener/6, start_listener/7,
         stop_listener/1]).


% --- Application ---

%% @doc Start the barrel application. Useful when testing using the shell.
start() ->
    barrel_deps:ensure(),
    application:load(barrel),
    barrel_app:ensure_deps_started(),
    application:start(barrel).

%% @doc Start the coffer application. Useful when testing using the shell.
stop() ->
    application:stop(barrel).


% --- barrel API ---


start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
               ProtocolOpts) ->
        start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
                       ProtocolOpts, []).

start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
               ProtoOpts, ListenerOpts) ->
    barrel_listener:start_listener(Ref, NbAcceptors, Transport, TransOpts,
                                    Protocol, ProtoOpts, ListenerOpts).

stop_listener(Ref) ->
    barrel_listener:stop_listener(Ref).
