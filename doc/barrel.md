

# Module barrel #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-ref">ref()</a> ###



<pre><code>
ref() = any()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accept_ack-1">accept_ack/1</a></td><td></td></tr><tr><td valign="top"><a href="#child_spec-2">child_spec/2</a></td><td>return a child spec suitable for embeding your listener in the
supervisor.</td></tr><tr><td valign="top"><a href="#get_max_clients-1">get_max_clients/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_port-1">get_port/1</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td></td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_max_clients-2">set_max_clients/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start the barrel application.</td></tr><tr><td valign="top"><a href="#start_listener-6">start_listener/6</a></td><td></td></tr><tr><td valign="top"><a href="#start_listener-7">start_listener/7</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Start the coffer application.</td></tr><tr><td valign="top"><a href="#stop_listener-1">stop_listener/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accept_ack-1"></a>

### accept_ack/1 ###

`accept_ack(Ref) -> any()`


<a name="child_spec-2"></a>

### child_spec/2 ###

`child_spec(Ref, Options) -> any()`

return a child spec suitable for embeding your listener in the
supervisor
<a name="get_max_clients-1"></a>

### get_max_clients/1 ###

`get_max_clients(Ref) -> any()`


<a name="get_port-1"></a>

### get_port/1 ###

`get_port(Ref) -> any()`


<a name="info-1"></a>

### info/1 ###

`info(Ref) -> any()`


<a name="info-2"></a>

### info/2 ###

`info(Ref, Key) -> any()`


<a name="set_max_clients-2"></a>

### set_max_clients/2 ###

`set_max_clients(Ref, MaxClients) -> any()`


<a name="start-0"></a>

### start/0 ###

`start() -> any()`

Start the barrel application. Useful when testing using the shell.
<a name="start_listener-6"></a>

### start_listener/6 ###

`start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtocolOpts) -> any()`


<a name="start_listener-7"></a>

### start_listener/7 ###

`start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts, ListenerOpts0) -> any()`


<a name="stop-0"></a>

### stop/0 ###

`stop() -> any()`

Start the coffer application. Useful when testing using the shell.
<a name="stop_listener-1"></a>

### stop_listener/1 ###

`stop_listener(Ref) -> any()`


