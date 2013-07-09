

# Module barrel_server #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_listener-1">get_listener/1</a></td><td>Return the listener associated to the ref.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_listener-2">set_listener/2</a></td><td>Set the listener associated to the ref.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Start the barell_sever.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_listener-1"></a>

### get_listener/1 ###


<pre><code>
get_listener(Ref::<a href="ranch.md#type-ref">ranch:ref()</a>) -&gt; pid()
</code></pre>

<br></br>


Return the listener associated to the ref.
<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`


<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`


<a name="set_listener-2"></a>

### set_listener/2 ###


<pre><code>
set_listener(Ref::<a href="barrel.md#type-ref">barrel:ref()</a>, Pid::pid()) -&gt; ok
</code></pre>

<br></br>


Set the listener associated to the ref.
<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, pid()}
</code></pre>

<br></br>


Start the barell_sever.
