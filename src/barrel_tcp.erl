%%% -*- erlang -*-
%%%
%%% This file is part of barrel released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>
%%%
-module(barrel_tcp).
-export([name/0,
	messages/0,
	listen/1,listen/2,
	accept/2,
	connect/3, connect/4,
	recv/2, recv/3,
	send/2,
	setopts/2,
	controlling_process/2,
	peername/1,
	close/1,
	sockname/1,
	sendfile/2]).

%% @doc Name of this transport, <em>tcp</em>.
name() -> tcp.

%% @doc Atoms used to identify messages in {active, once | true} mod.
messages() -> {tcp, tcp_closed, tcp_error}.

%% @doc Listen for connections on the given port number.
%% @see gen_tcp:listen/2
%%

listen(Opts) ->
    listen(0, Opts).

listen(Port, Opts) ->
    BaseOpts = [binary,
                {backlog, 1024},
                {active, false},
                {packet, raw},
                {reuseaddr, true},
                {nodelay, true}],
    Opts1 = barrel_util:filter_props(Opts, [backlog, ip, nodelay, port,
                                            packet, raw], BaseOpts),
    gen_tcp:listen(Port, barrel_util:fix_ip(Opts1)).

%% @doc Accept connections with the given listening socket.
%% @see gen_tcp:accept/2
-spec accept(inet:socket(), timeout())
	-> {ok, inet:socket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	gen_tcp:accept(LSocket, Timeout).

connect(Host, Port, Opts) ->
	connect(Host, Port, Opts, infinity).

connect(Host, Port, Opts, Timeout) when is_list(Host), is_integer(Port),
	(Timeout =:= infinity orelse is_integer(Timeout)) ->
	gen_tcp:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}], Timeout).

recv(Socket, Length) ->
    recv(Socket, Length, infinity).

%% @doc Receive a packet from a socket in passive mode.
%% @see gen_tcp:recv/3
-spec recv(inet:socket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	gen_tcp:recv(Socket, Length, Timeout).


%% @doc Send a packet on a socket.
%% @see gen_tcp:send/2
-spec send(inet:socket(), iolist()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	gen_tcp:send(Socket, Packet).

%% @doc Set one or more options for a socket.
%% @see inet:setopts/2
-spec setopts(inet:socket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	inet:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see gen_tcp:controlling_process/2
-spec controlling_process(inet:socket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	gen_tcp:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see inet:peername/1
-spec peername(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
	inet:peername(Socket).

%% @doc Close a TCP socket.
%% @see gen_tcp:close/1
-spec close(inet:socket()) -> ok.
close(Socket) ->
	gen_tcp:close(Socket).

%% @doc Get the local address and port of a socket
%% @see inet:sockname/1
-spec sockname(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
	inet:sockname(Socket).


%% @doc Send a file on a socket.
%%
%% This is the optimal way to send files using TCP. It uses a syscall
%% which means there is no context switch between opening the file
%% and writing its contents on the socket.
%%
%% @see file:sendfile/2
-spec sendfile(inet:socket(), file:name())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename) ->
	try file:sendfile(Filename, Socket) of
		Result -> Result
	catch
		error:{badmatch, {error, enotconn}} ->
			%% file:sendfile/2 might fail by throwing a {badmatch, {error, enotconn}}
			%% this is because its internal implementation fails with a badmatch in
			%% prim_file:sendfile/10 if the socket is not connected.
			{error, closed}
	end.
