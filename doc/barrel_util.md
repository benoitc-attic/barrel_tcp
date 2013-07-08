

# Module barrel_util #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#filter_props-2">filter_props/2</a></td><td>filter a list of properties and removed n.</td></tr><tr><td valign="top"><a href="#filter_props-3">filter_props/3</a></td><td></td></tr><tr><td valign="top"><a href="#fix_ip-1">fix_ip/1</a></td><td></td></tr><tr><td valign="top"><a href="#ipv6_supported-0">ipv6_supported/0</a></td><td></td></tr><tr><td valign="top"><a href="#propmerge-2">propmerge/2</a></td><td>Update a proplist with values of the second.</td></tr><tr><td valign="top"><a href="#require-1">require/1</a></td><td>Start the given applications if they were not already started.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="filter_props-2"></a>

### filter_props/2 ###

`filter_props(Props, Allowed) -> any()`

filter a list of properties and removed n
<a name="filter_props-3"></a>

### filter_props/3 ###

`filter_props(Rest, Allowed, Acc) -> any()`


<a name="fix_ip-1"></a>

### fix_ip/1 ###

`fix_ip(Opts) -> any()`


<a name="ipv6_supported-0"></a>

### ipv6_supported/0 ###

`ipv6_supported() -> any()`


<a name="propmerge-2"></a>

### propmerge/2 ###

`propmerge(L1, L2) -> any()`

Update a proplist with values of the second. In case the same
key is in 2 proplists, the value from the first are kept.
<a name="require-1"></a>

### require/1 ###


<pre><code>
require(Rest::[module()]) -&gt; ok
</code></pre>

<br></br>


Start the given applications if they were not already started.
