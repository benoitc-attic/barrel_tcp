

# Module barrel_listener #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_port-1">get_port/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td></td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_listener-6">start_listener/6</a></td><td></td></tr><tr><td valign="top"><a href="#start_listener-7">start_listener/7</a></td><td></td></tr><tr><td valign="top"><a href="#stop_listener-1">stop_listener/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`


<a name="get_port-1"></a>

### get_port/1 ###

`get_port(Ref) -> any()`


<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Msg, From, State) -> any()`


<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(X1, State) -> any()`


<a name="info-1"></a>

### info/1 ###

`info(Ref) -> any()`


<a name="info-2"></a>

### info/2 ###

`info(Ref, Keys) -> any()`


<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`


<a name="start_link-1"></a>

### start_link/1 ###

`start_link(Options) -> any()`


<a name="start_listener-6"></a>

### start_listener/6 ###

`start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtocolOpts) -> any()`


<a name="start_listener-7"></a>

### start_listener/7 ###

`start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts, ListenerOpts0) -> any()`


<a name="stop_listener-1"></a>

### stop_listener/1 ###

`stop_listener(Ref) -> any()`


<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


