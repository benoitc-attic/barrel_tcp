%%% -*- erlang -*-
%%%
%%% This file is part of barrel released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>

-module(barrel_ssl).
-export([name/0,
         listen/1, listen/2,
         accept/2,
         connect/3, connect/4,
         recv/3, recv/2,
         send/2,
         setopts/2,
         controlling_process/2,
         peername/1,
         close/1,
         sockname/1]).

%% @doc Name of this transport, <em>tcp</em>.
name() -> ssl.

%% @doc Listen for connections on the given port number.
%% @see ssl:listen/2
%%
listen(Opts) ->
    listen(0, Opts).


listen(Port, Opts) ->
    barrel_util:require([crypto, public_key, ssl]),
    BaseOpts = [binary,
                {backog, 1024},
                {active, false},
                {packet, raw},
                {reuseaddr, true},
                {nodelay, true}],

    % do we have required options
    true = lists:member(cert, 1, Opts)
           orelse lists:keymember(certfile, 1, Opts),

    Opts1 = barrel_util:filter_props(Opts, [backlog, cacertfile,
                                            cacerts, cert, certfile,
                                            ciphers, fail_if_no_peer_cert,
                                            ip, key, keyfile,
                                            next_protocols_advertised,
                                            verify_fun, depth,
                                            nodelay, password, port,
                                            raw, verify], BaseOpts),
    ssl:listen(Port, barrel_util:fix_ip(Opts1)).


%% @doc Accept connections with the given listening socket.
%%
%% Note that this function does both the transport accept and
%% the SSL handshake. The returned socket is thus fully connected.
%%
%% @see ssl:transport_accept/2
%% @see ssl:ssl_accept/2
-spec accept(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, closed | timeout | atom() | tuple()}.
accept(LSocket, Timeout) ->
	case ssl:transport_accept(LSocket, Timeout) of
		{ok, CSocket} ->
			ssl_accept(CSocket, Timeout);
		{error, Reason} ->
			{error, Reason}
	end.

connect(Host, Port, Opts) ->
	connect(Host, Port, Opts, infinity).

connect(Host, Port, Opts, Timeout) when is_list(Host), is_integer(Port),
	(Timeout =:= infinity orelse is_integer(Timeout)) ->
	ssl:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}], Timeout).

recv(Socket, Length) ->
    recv(Socket, Length, infinity).

%% @doc Receive a packet from a socket in passive mode.
%% @see ssl:recv/3
-spec recv(ssl:sslsocket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).

%% @doc Send a packet on a socket.
%% @see ssl:send/2
-spec send(ssl:sslsocket(), iolist()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	ssl:send(Socket, Packet).

%% @doc Set one or more options for a socket.
%% @see ssl:setopts/2
-spec setopts(ssl:sslsocket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	ssl:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see ssl:controlling_process/2
-spec controlling_process(ssl:sslsocket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	ssl:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see ssl:peername/1
-spec peername(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
	ssl:peername(Socket).

%% @doc Close a TCP socket.
%% @see ssl:close/1
-spec close(ssl:sslsocket()) -> ok.
close(Socket) ->
	ssl:close(Socket).

%% @doc Get the local address and port of a socket
%% @see ssl:sockname/1
-spec sockname(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
	ssl:sockname(Socket).



%% Internal
%%
%% This call always times out, either because a numeric timeout value
%% was given, or because we've decided to use 5000ms instead of infinity.
%% This value should be reasonable enough for the moment.
-spec ssl_accept(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, {ssl_accept, atom()}}.
ssl_accept(Socket, infinity) ->
	ssl_accept(Socket, 5000);
ssl_accept(Socket, Timeout) ->
	case ssl:ssl_accept(Socket, Timeout) of
		ok ->
			{ok, Socket};
		{error, Reason} ->
			{error, {ssl_accept, Reason}}
	end.
