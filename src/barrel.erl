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
         info/1, info/2]).

-type ref() :: any().
-export_type([ref/0]).

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

stop_listener(Ref) ->
    case supervisor:terminate_child(barrel_sup, Ref) of
        ok ->
            supervisor:delete_child(barrel_sup, Ref);
        Error ->
            Error
    end.

%% @doc return a child spec suitable for embeding your listener in the
%% supervisor
child_spec(Ref, Options) ->
    {Ref, {barrel_listener, start_link, [Options]},
            permanent, 5000, worker, [Ref]}.

get_port(Ref) ->
    ListenerPid = barrel_server:get_listener(Ref),
    barrel_listener:get_port(ListenerPid).

info(Ref) ->
    ListenerPid = barrel_server:get_listener(Ref),
    barrel_listener:info(ListenerPid).

info(Ref, Key) ->
    ListenerPid = barrel_server:get_listener(Ref),
    barrel_listener:info(ListenerPid, Key).

