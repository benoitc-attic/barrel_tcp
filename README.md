

# barrel - generic TCP acceptor pool #

Copyright (c) 2013 Benoît Chesneau.

__Version:__ 1.1

# barrel

barrel is a generic TCP acceptor pool with low latency in Erlang.

> The design of barrel differs from
> [ranch](http://github.com/extend/ranch). Instead of spawning w new
> process once a connectino and passing the control of the socket to it
> which can be slow, Barrel ois only spawning a new acceptors. The
> accepted socket will continue to be used in the same process that
> accepted it before.

## Usage

Create a simple TCP echo server.

1. Create a simple echo handler

```
    -module(echo_handler).

    -export([init/3]).

    init(Transport, Socket, _Opts) ->
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

Note the init command that received the transport (barrel_tcp if TCP or
barrel_ssl if SSL), Socket, the client socket to use and the transport
options.

To start the listener do the following:

```
    barrel:start_listener(http, 100, barrel_tcp,
                         [{port, 10001}], echo_handler, []).
```

The full example can be found in the [example folder](http://github.com/benoitc/barrel/tree/master/example/echo).

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
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_deps.md" class="module">barrel_deps</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_listener.md" class="module">barrel_listener</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_ssl.md" class="module">barrel_ssl</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_sup.md" class="module">barrel_sup</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_tcp.md" class="module">barrel_tcp</a></td></tr>
<tr><td><a href="http://github.com/benoitc/barrel/blob/master/doc/barrel_util.md" class="module">barrel_util</a></td></tr></table>

