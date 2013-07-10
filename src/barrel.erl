%%% -*- erlang -*-
%%%
%%% This file is part of barrel released under the MIT license.
%%% See the NOTICE for more information.

-module(barrel).

-export([start/0, stop/0]).
-export([start_listener/6, start_listener/7,
         stop_listener/1,
         child_spec/2,
         get_port/1,
         info/1, info/2,
         set_max_clients/2, get_max_clients/1,
         set_nb_acceptors/2, get_nb_acceptors/1,
         set_protocol_conf/3, set_protocol_conf/4,
         get_protocol_conf/1]).

-export([accept_ack/1]).

-type ref() :: any().
-export_type([ref/0]).

-type info_key() :: ip | port | open_reqs | nb_acceptors | max_clients.
-export_type([info_key/0]).

-type info_keys() :: [info_key()].
-export_type([info_keys/0]).

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

-spec start_listener(barrel:ref(), integer(), any(), any(), any(),
                     any()) -> {ok, pid()} | {error, term()}.
%% @doc start a listener
%%
%% ```
%%   Ref = term()
%%   NbAcceptors = integer()
%%   Transport = barrel_tcp | barrel_ssl | any
%%   TransOpts = any()
%%   Protocol = any()
%%   ProtocolOpts = any()
%%   ListenerOpts - any(),
%% '''
%%
%% A Ref can be any Erlang term used to identify a listener. A listener
%% is a gen_server that manage all acceptors workers and handle
%% connections shutdown.
%%
%% A protocol is the  protocol used to handle a connection. It can be
%% any module following the protocol behaviour. Barrel offers
%% to handle the TCP ans SSL/TLS protocol for now.
%%
%% A protocol is what will be used to handle the data coming from the
%% socket. You can pass to it some options (ProtocolOpts).
%%
%% Optionnaly you can pass custom options to the listener. This is where
%% you pass the SSL options for example.
start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
               ProtocolOpts) ->
        start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
                       ProtocolOpts, []).

-spec start_listener(barrel:ref(), integer(), any(), any(), any(),
                     any(), any()) -> {ok, pid()} | {error, term()}.
start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
               ProtoOpts, ListenerOpts0) ->
    _ = code:ensure_loaded(Transport),
    case erlang:function_exported(Transport, name, 0) of
		false ->
			{error, badarg};
		true ->
            ListenerOpts = [{ref, Ref} | ListenerOpts0],
            supervisor:start_child(barrel_sup,
                                   child_spec(Ref, [NbAcceptors, Transport,
                                                     TransOpts, Protocol,
                                                     ProtoOpts,
                                                     ListenerOpts]))
    end.

%% @doc stop a listener
%% All connections and acceptors for this listener are killed
%% immediately.
-spec stop_listener(barrel:ref()) -> ok | {error, term()}.
stop_listener(Ref) ->
    case supervisor:terminate_child(barrel_sup, Ref) of
        ok ->
            supervisor:delete_child(barrel_sup, Ref);
        Error ->
            Error
    end.

%% @doc return a child spec suitable for embeding your listener in the
%% supervisor

-spec child_spec(barrel:ref(), any()) -> any().
child_spec(Ref, Options) ->
    {Ref, {barrel_listener, start_link, [Options]},
            permanent, 5000, worker, [Ref]}.

%% @doc get current port of a listener
%%
%% Ref = term()
-spec get_port(barrel:ref()) -> integer().
get_port(Ref) ->
    ListenerPid = barrel_server:get_listener(Ref),
    barrel_listener:get_port(ListenerPid).

%% @doc get all infos of a listener
%%
%% %% Ref = term()
-spec info(barrel:ref()) -> any().
info(Ref) ->
    ListenerPid = barrel_server:get_listener(Ref),
    barrel_listener:info(ListenerPid).

%% @doc get info for some keys
%%
%% Ref = term()
%% Key = ip | port | open_reqs | nb_acceptors | max_clients
-spec info(barrel:ref(), info_keys()) -> any().
info(Ref, Key) ->
    ListenerPid = barrel_server:get_listener(Ref),
    barrel_listener:info(ListenerPid, Key).

%% @doc set max number of concurrent clients
set_max_clients(Ref, MaxClients) ->
    ListenerPid = barrel_server:get_listener(Ref),
    barrel_listener:set_max_clients(ListenerPid, MaxClients).

%% @doc get max number of concurrent clients
get_max_clients(Ref) ->
    ListenerPid = barrel_server:get_listener(Ref),
    barrel_listener:get_max_clients(ListenerPid).

%% @doc set the number of acceptors for a listener. By default 100.
set_nb_acceptors(Ref, Nb) ->
    ListenerPid = barrel_server:get_listener(Ref),
    barrel_listener:set_nb_acceptors(ListenerPid, Nb).

%% @doc get the number of acceptors set for a listener
get_nb_acceptors(Ref) ->
    ListenerPid = barrel_server:get_listener(Ref),
    barrel_listener:get_nb_acceptors(ListenerPid).

%% @doc update the protocol configuration and kill the connections after
%% 30s.
set_protocol_conf(Ref, Handler, Options) ->
    set_protocol_conf(Ref, Handler, Options, 30000).

%% @doc update the protocol configuration and kill the connections after
%% a timeout. If timeout is none then the connections will continue
%% until they die.
set_protocol_conf(Ref, Handler, Options, GracefulTimeout) ->
    ListenerPid = barrel_server:get_listener(Ref),
    barrel_listener:set_protocol_conf(ListenerPid, Handler, Options,
                                      GracefulTimeout).

%% @doc get the protocol configuration
get_protocol_conf(Ref) ->
    ListenerPid = barrel_server:get_listener(Ref),
    barrel_listener:get_protocol_conf(ListenerPid).

%% used to start to handle the connection in a spawned protocol process.
%% It is needed to use this function first so the control of the socket
%% is given to the process.
accept_ack(Ref) ->
    receive {accept_ack, Ref} -> ok end.
