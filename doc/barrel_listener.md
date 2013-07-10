

# Module barrel_listener #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_max_clients-1">get_max_clients/1</a></td><td>get max number of concurrent clients.</td></tr><tr><td valign="top"><a href="#get_nb_acceptors-1">get_nb_acceptors/1</a></td><td>get the number of acceptors.</td></tr><tr><td valign="top"><a href="#get_port-1">get_port/1</a></td><td>get current port.</td></tr><tr><td valign="top"><a href="#get_protocol_conf-1">get_protocol_conf/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>get all infos.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td>get info for some keys.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_max_clients-2">set_max_clients/2</a></td><td>set max number of concurrent clients.</td></tr><tr><td valign="top"><a href="#set_nb_acceptors-2">set_nb_acceptors/2</a></td><td>set the number of acceptors.</td></tr><tr><td valign="top"><a href="#set_protocol_conf-4">set_protocol_conf/4</a></td><td>update the protocol configuration and kill after a timeout.</td></tr><tr><td valign="top"><a href="#start_accepting-1">start_accepting/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`


<a name="get_max_clients-1"></a>

### get_max_clients/1 ###

`get_max_clients(Ref) -> any()`

get max number of concurrent clients
<a name="get_nb_acceptors-1"></a>

### get_nb_acceptors/1 ###

`get_nb_acceptors(Ref) -> any()`

get the number of acceptors
<a name="get_port-1"></a>

### get_port/1 ###

`get_port(Ref) -> any()`

get current port
<a name="get_protocol_conf-1"></a>

### get_protocol_conf/1 ###

`get_protocol_conf(Ref) -> any()`


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

get all infos
<a name="info-2"></a>

### info/2 ###

`info(Ref, Keys) -> any()`

get info for some keys
<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`


<a name="set_max_clients-2"></a>

### set_max_clients/2 ###

`set_max_clients(Ref, Nb) -> any()`

set max number of concurrent clients
<a name="set_nb_acceptors-2"></a>

### set_nb_acceptors/2 ###

`set_nb_acceptors(Ref, Nb) -> any()`

set the number of acceptors
<a name="set_protocol_conf-4"></a>

### set_protocol_conf/4 ###

`set_protocol_conf(Ref, Handler, Opts, GracefulTimeout) -> any()`

update the protocol configuration and kill after a timeout
<a name="start_accepting-1"></a>

### start_accepting/1 ###

`start_accepting(Ref) -> any()`


<a name="start_link-1"></a>

### start_link/1 ###

`start_link(Options) -> any()`


<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


