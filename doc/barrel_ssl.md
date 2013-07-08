

# Module barrel_ssl #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accept-2">accept/2</a></td><td>Accept connections with the given listening socket.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close a TCP socket.</td></tr><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td></td></tr><tr><td valign="top"><a href="#connect-4">connect/4</a></td><td></td></tr><tr><td valign="top"><a href="#controlling_process-2">controlling_process/2</a></td><td>Assign a new controlling process <em>Pid</em> to <em>Socket</em>.</td></tr><tr><td valign="top"><a href="#listen-1">listen/1</a></td><td>Listen for connections on the given port number.</td></tr><tr><td valign="top"><a href="#listen-2">listen/2</a></td><td></td></tr><tr><td valign="top"><a href="#name-0">name/0</a></td><td>Name of this transport, <em>tcp</em>.</td></tr><tr><td valign="top"><a href="#peername-1">peername/1</a></td><td>Return the address and port for the other end of a connection.</td></tr><tr><td valign="top"><a href="#recv-2">recv/2</a></td><td></td></tr><tr><td valign="top"><a href="#recv-3">recv/3</a></td><td>Receive a packet from a socket in passive mode.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send a packet on a socket.</td></tr><tr><td valign="top"><a href="#setopts-2">setopts/2</a></td><td>Set one or more options for a socket.</td></tr><tr><td valign="top"><a href="#sockname-1">sockname/1</a></td><td>Get the local address and port of a socket.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accept-2"></a>

### accept/2 ###


<pre><code>
accept(LSocket::<a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>, Timeout::timeout()) -&gt; {ok, <a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>} | {error, closed | timeout | atom() | tuple()}
</code></pre>

<br></br>



Accept connections with the given listening socket.


Note that this function does both the transport accept and
the SSL handshake. The returned socket is thus fully connected.


__See also:__ [ssl:ssl_accept/2](ssl.md#ssl_accept-2), [ssl:transport_accept/2](ssl.md#transport_accept-2).
<a name="close-1"></a>

### close/1 ###


<pre><code>
close(Socket::<a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>) -&gt; ok
</code></pre>

<br></br>


Close a TCP socket.

__See also:__ [ssl:close/1](ssl.md#close-1).
<a name="connect-3"></a>

### connect/3 ###

`connect(Host, Port, Opts) -> any()`


<a name="connect-4"></a>

### connect/4 ###

`connect(Host, Port, Opts, Timeout) -> any()`


<a name="controlling_process-2"></a>

### controlling_process/2 ###


<pre><code>
controlling_process(Socket::<a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>, Pid::pid()) -&gt; ok | {error, closed | not_owner | atom()}
</code></pre>

<br></br>


Assign a new controlling process _Pid_ to _Socket_.

__See also:__ [ssl:controlling_process/2](ssl.md#controlling_process-2).
<a name="listen-1"></a>

### listen/1 ###

`listen(Opts) -> any()`

Listen for connections on the given port number.

__See also:__ [ssl:listen/2](ssl.md#listen-2).
<a name="listen-2"></a>

### listen/2 ###

`listen(Port, Opts) -> any()`


<a name="name-0"></a>

### name/0 ###

`name() -> any()`

Name of this transport, _tcp_.
<a name="peername-1"></a>

### peername/1 ###


<pre><code>
peername(Socket::<a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>) -&gt; {ok, {<a href="inet.md#type-ip_address">inet:ip_address()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}} | {error, atom()}
</code></pre>

<br></br>


Return the address and port for the other end of a connection.

__See also:__ [ssl:peername/1](ssl.md#peername-1).
<a name="recv-2"></a>

### recv/2 ###

`recv(Socket, Length) -> any()`


<a name="recv-3"></a>

### recv/3 ###


<pre><code>
recv(Socket::<a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>, Length::non_neg_integer(), Timeout::timeout()) -&gt; {ok, any()} | {error, closed | atom()}
</code></pre>

<br></br>


Receive a packet from a socket in passive mode.

__See also:__ [ssl:recv/3](ssl.md#recv-3).
<a name="send-2"></a>

### send/2 ###


<pre><code>
send(Socket::<a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>, Packet::iolist()) -&gt; ok | {error, atom()}
</code></pre>

<br></br>


Send a packet on a socket.

__See also:__ [ssl:send/2](ssl.md#send-2).
<a name="setopts-2"></a>

### setopts/2 ###


<pre><code>
setopts(Socket::<a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>, Opts::list()) -&gt; ok | {error, atom()}
</code></pre>

<br></br>


Set one or more options for a socket.

__See also:__ [ssl:setopts/2](ssl.md#setopts-2).
<a name="sockname-1"></a>

### sockname/1 ###


<pre><code>
sockname(Socket::<a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>) -&gt; {ok, {<a href="inet.md#type-ip_address">inet:ip_address()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}} | {error, atom()}
</code></pre>

<br></br>


Get the local address and port of a socket

__See also:__ [ssl:sockname/1](ssl.md#sockname-1).
