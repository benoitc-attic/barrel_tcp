

# barrel - generic TCP acceptor pool #

Copyright (c) 2013 Benoît Chesneau.

__Version:__ 2.0

# barrel

barrel is a generic TCP acceptor pool with low latency in Erlang.

**Main Features**:

- start/stop TCP and SSL listener
- can be use with different transports based on an "accept" model. (can
be stcp, uTCP...)
- Low latency when accepting connection
- Any protocol can be used, can be HTTP, IRC or anything you want.
- Graceful reload of protocol configurations
- Scale the number of concurrent connection accepted at the same time
- Scale the number of concurrent connections handled by a listener

## Design

> The design of barrel differs from
> [ranch](http://github.com/extend/ranch). Instead of spawning with a
> new which can be slow, Barrel is only spawning a new acceptors. The
> accepted socket will continue to be used in the same process that
> accepted it before. Optionnaly you can also launch a process to handle
> the accepted socket if you want. Choice is yours.

## Usage

### Create a simple TCP echo server.

1. Create a simple echo handler

```
-module(echo_handler).

-export([init/3]).

init(_ListenerRef, Transport, Socket, _Opts) ->
    wait_request(Transport, Socket).

wait_request(Transport, Socket) ->
    case  Transport:recv(Socket, 0, 5000) of
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
```

`init/4` is the function that received the transport (`barrel_tcp` if TCP or
`barrel_ssl` if SSL) socket.

Note that by default the design of barrel use the process that accepted
the connection to handle the request. In short the acceptor process
become the request process. In some case you may want to launch a new
process instead and pass the control of the socket to it (which is the
only way to do it in ranch). In this case instead of `init/4` use a
`start_link/4` function. Eg. :

```
start_link(Ref, Transport, Socket, _Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Socket]),
    {ok, Pid}.

init(Ref, Transport, Socket) ->
    ok = barrel:accept_ack(Ref),
    wait_request(Transport, Socket).

    ...
```

To start the listener do the following:

```
Ref = echo,
NbAcceptors = 100,
Transport = barrel_tcp,
TransporOptions =  [{port, 10001}],
Protocol = echo_handler,
ProtocolOptions = [],
barrel:start_listener(Ref, NbAcceptors, Transport, TransportOptions,
                      Protocol, ProtocolOptions).
```

A Ref can be any Erlang term used to identify a listener. A listener is
a gen_server that manage all acceptors workers and handle connections
shutdown.

A protocol is the  protocol used to handle a connection. It can be any
module following the protocol behaviour. Barrel offers to handle the TCP
(`barel_tcp`) ans SSL/TLS (`barel_ssl`) protocol
for now.

A protocol is what will be used to handle the data coming from the
socket. You can pass to it some options (ProtocolOpts).

Optionnaly you can pass custom options to the listener. This is where
you pass the SSL options for example.

The full example can be found in the [example folder](http://github.com/benoitc/barrel/tree/master/example/echo).

### Scale

#### Number of acceptors

There are 2 way to scale a listener in barrel. The first one is to
increase the number of acceptors. By default the the number of acceptors
is 100.

To do it use the `barrel:set_nb_acceptors/2` function.

Increasing the number of acceptors will increase the number of
concurrent connections you can **accept** at the same time

### Number of clients

While increasing the number of acceptors increase the number of
concurrent connections you **accept** at the same time, you can also fix
the number of concurrent conncections (clients) you want to handle. This
can be useful to limit the usage of the resources (memory & number of
file descriptors).

To do it use the `barrel:set_max_client/2` function.

### Load a new protocol configuration

barrel allows you to either change the protocol handler or its
configuration without closing immediately running connections.

What happen here is that once you pass a new protocol configuration
using the function `barrel:set_protocol_conf/4` to the
listener, new acceptors will be launched with the new configurations and
old acceptors will be killed right after. Once it's done a graceful
shutdown will be sent to the connections. If the graceful timeout is
none, then the connections will continue to run until they die, in other
case the connections will be killed after this time. This behaviour is
quite similar to the one you can find in
[nginx](http://wiki.nginx.org/CommandLine#Loading_a_New_Configuration_Using_Signals).

## Contribute

For issues, comments or feedback please [create an
issue](http://github.com/benoitc/barrel/issues).

### Notes for developers

If you want to contribute patches or improve the doc, you will need to
build barrel using the `rebar_dev.config`  file. It can also be built
using the **Makefile**:

```
$ make dev ; # compile & get deps
$ make devclean ; # clean all files
```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel.md" class="module">barrel</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_acceptor.md" class="module">barrel_acceptor</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_app.md" class="module">barrel_app</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_connections.md" class="module">barrel_connections</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_deps.md" class="module">barrel_deps</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_listener.md" class="module">barrel_listener</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_server.md" class="module">barrel_server</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_ssl.md" class="module">barrel_ssl</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_sup.md" class="module">barrel_sup</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_tcp.md" class="module">barrel_tcp</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_util.md" class="module">barrel_util</a></td></tr></table>

