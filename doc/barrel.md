

# Module barrel #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-info_key">info_key()</a> ###



<pre><code>
info_key() = ip | port | open_reqs | nb_acceptors | max_clients
</code></pre>





### <a name="type-info_keys">info_keys()</a> ###



<pre><code>
info_keys() = [<a href="#type-info_key">info_key()</a>]
</code></pre>





### <a name="type-ref">ref()</a> ###



<pre><code>
ref() = any()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accept_ack-1">accept_ack/1</a></td><td></td></tr><tr><td valign="top"><a href="#child_spec-2">child_spec/2</a></td><td>return a child spec suitable for embeding your listener in the
supervisor.</td></tr><tr><td valign="top"><a href="#get_max_clients-1">get_max_clients/1</a></td><td>get max number of concurrent clients.</td></tr><tr><td valign="top"><a href="#get_nb_acceptors-1">get_nb_acceptors/1</a></td><td>get the number of acceptors set for a listener.</td></tr><tr><td valign="top"><a href="#get_port-1">get_port/1</a></td><td>get current port of a listener.</td></tr><tr><td valign="top"><a href="#get_protocol_conf-1">get_protocol_conf/1</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>get all infos of a listener.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td>get info for some keys.</td></tr><tr><td valign="top"><a href="#set_max_clients-2">set_max_clients/2</a></td><td>set max number of concurrent clients.</td></tr><tr><td valign="top"><a href="#set_nb_acceptors-2">set_nb_acceptors/2</a></td><td>set the number of acceptors for a listener.</td></tr><tr><td valign="top"><a href="#set_protocol_conf-3">set_protocol_conf/3</a></td><td>update the protocol configuration and kill the connections after
30s.</td></tr><tr><td valign="top"><a href="#set_protocol_conf-4">set_protocol_conf/4</a></td><td>update the protocol configuration and kill the connections after
a timeout.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start the barrel application.</td></tr><tr><td valign="top"><a href="#start_listener-6">start_listener/6</a></td><td>start a listener
<pre>    Ref = term()
    NbAcceptors = integer()
    Transport = barrel_tcp | barrel_ssl | any
    TransOpts = any()
    Protocol = any()
    ProtocolOpts = any()
    ListenerOpts - any(),</pre>.</td></tr><tr><td valign="top"><a href="#start_listener-7">start_listener/7</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Start the coffer application.</td></tr><tr><td valign="top"><a href="#stop_listener-1">stop_listener/1</a></td><td>stop a listener
All connections and acceptors for this listener are killed
immediately.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accept_ack-1"></a>

### accept_ack/1 ###

`accept_ack(Ref) -> any()`


<a name="child_spec-2"></a>

### child_spec/2 ###


<pre><code>
child_spec(Ref::<a href="barrel.md#type-ref">barrel:ref()</a>, Options::any()) -&gt; any()
</code></pre>

<br></br>


return a child spec suitable for embeding your listener in the
supervisor
<a name="get_max_clients-1"></a>

### get_max_clients/1 ###

`get_max_clients(Ref) -> any()`

get max number of concurrent clients
<a name="get_nb_acceptors-1"></a>

### get_nb_acceptors/1 ###

`get_nb_acceptors(Ref) -> any()`

get the number of acceptors set for a listener
<a name="get_port-1"></a>

### get_port/1 ###


<pre><code>
get_port(Ref::<a href="barrel.md#type-ref">barrel:ref()</a>) -&gt; integer()
</code></pre>

<br></br>



get current port of a listener


Ref = term()
<a name="get_protocol_conf-1"></a>

### get_protocol_conf/1 ###

`get_protocol_conf(Ref) -> any()`


<a name="info-1"></a>

### info/1 ###


<pre><code>
info(Ref::<a href="barrel.md#type-ref">barrel:ref()</a>) -&gt; any()
</code></pre>

<br></br>



get all infos of a listener


%% Ref = term()
<a name="info-2"></a>

### info/2 ###


<pre><code>
info(Ref::<a href="barrel.md#type-ref">barrel:ref()</a>, Key::<a href="#type-info_keys">info_keys()</a>) -&gt; any()
</code></pre>

<br></br>



get info for some keys


Ref = term()
Key = ip | port | open_reqs | nb_acceptors | max_clients
<a name="set_max_clients-2"></a>

### set_max_clients/2 ###

`set_max_clients(Ref, MaxClients) -> any()`

set max number of concurrent clients
<a name="set_nb_acceptors-2"></a>

### set_nb_acceptors/2 ###

`set_nb_acceptors(Ref, Nb) -> any()`

set the number of acceptors for a listener. By default 100.
<a name="set_protocol_conf-3"></a>

### set_protocol_conf/3 ###

`set_protocol_conf(Ref, Handler, Options) -> any()`

update the protocol configuration and kill the connections after
30s.
<a name="set_protocol_conf-4"></a>

### set_protocol_conf/4 ###

`set_protocol_conf(Ref, Handler, Options, GracefulTimeout) -> any()`

update the protocol configuration and kill the connections after
a timeout. If timeout is none then the connections will continue
until they die.
<a name="start-0"></a>

### start/0 ###

`start() -> any()`

Start the barrel application. Useful when testing using the shell.
<a name="start_listener-6"></a>

### start_listener/6 ###


<pre><code>
start_listener(Ref::<a href="barrel.md#type-ref">barrel:ref()</a>, NbAcceptors::integer(), Transport::any(), TransOpts::any(), Protocol::any(), ProtocolOpts::any()) -&gt; {ok, pid()} | {error, term()}
</code></pre>

<br></br>


start a listener

```
    Ref = term()
    NbAcceptors = integer()
    Transport = barrel_tcp | barrel_ssl | any
    TransOpts = any()
    Protocol = any()
    ProtocolOpts = any()
    ListenerOpts - any(),
```



A Ref can be any Erlang term used to identify a listener. A listener
is a gen_server that manage all acceptors workers and handle
connections shutdown.



A protocol is the  protocol used to handle a connection. It can be
any module following the protocol behaviour. Barrel offers
to handle the TCP ans SSL/TLS protocol for now.



A protocol is what will be used to handle the data coming from the
socket. You can pass to it some options (ProtocolOpts).


Optionnaly you can pass custom options to the listener. This is where
you pass the SSL options for example.
<a name="start_listener-7"></a>

### start_listener/7 ###


<pre><code>
start_listener(Ref::<a href="barrel.md#type-ref">barrel:ref()</a>, NbAcceptors::integer(), Transport::any(), TransOpts::any(), Protocol::any(), ProtoOpts::any(), ListenerOpts0::any()) -&gt; {ok, pid()} | {error, term()}
</code></pre>

<br></br>



<a name="stop-0"></a>

### stop/0 ###

`stop() -> any()`

Start the coffer application. Useful when testing using the shell.
<a name="stop_listener-1"></a>

### stop_listener/1 ###


<pre><code>
stop_listener(Ref::<a href="barrel.md#type-ref">barrel:ref()</a>) -&gt; ok | {error, term()}
</code></pre>

<br></br>


stop a listener
All connections and acceptors for this listener are killed
immediately.
