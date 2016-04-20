

# Module poolgirl #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-mfargs">mfargs()</a> ###


<pre><code>
mfargs() = {Module::atom(), Function::atom(), Args::list()}
</code></pre>




### <a name="type-pool_options">pool_options()</a> ###


<pre><code>
pool_options() = #{size =&gt; integer(), chunk_size =&gt; integer(), max_age =&gt; integer(), clean_interval =&gt; integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_pool-2">add_pool/2</a></td><td>Equivalent to <a href="#add_pool-3"><tt>add_pool(Name, MFArgs, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#add_pool-3">add_pool/3</a></td><td> 
Create a new pool.</td></tr><tr><td valign="top"><a href="#assigned-1">assigned/1</a></td><td> 
Return the list of assigned workers.</td></tr><tr><td valign="top"><a href="#checkin-1">checkin/1</a></td><td> 
Checkin a worker.</td></tr><tr><td valign="top"><a href="#checkout-1">checkout/1</a></td><td> 
Checkout a worker.</td></tr><tr><td valign="top"><a href="#remove_pool-1">remove_pool/1</a></td><td>
Remove an existing pool.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td> 
Get a worker size and number of unassigned workers.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_pool-2"></a>

### add_pool/2 ###

`add_pool(Name, MFArgs) -> any()`

Equivalent to [`add_pool(Name, MFArgs, #{})`](#add_pool-3).

<a name="add_pool-3"></a>

### add_pool/3 ###

<pre><code>
add_pool(Name::atom(), MFArgs::<a href="#type-mfargs">mfargs()</a>, Options::<a href="#type-pool_options">pool_options()</a>) -&gt; {ok, integer()} | {error, term()}
</code></pre>
<br />


Create a new pool.

Options :

* `size :: integer()` : Minimum pool size (Default : 5).

* `chunk_size :: integer()` : Chunk size (Default : 10).

* `max_age :: integer()` : Maximum age (in ms) of unused workers before destruction (Default : 120000).

* `clean_interval :: integer()` : Interval (in ms) between each cleanup (Default : 60000).


Example :

```

 poolgirl:add_pool(test, {my_server, start_link, [{127,0,0,1}, 9092]}, #{size => 2,
                                                                         chunk_size => 4}).
```

<a name="assigned-1"></a>

### assigned/1 ###

<pre><code>
assigned(Pool::atom()) -&gt; {ok, [pid()]} | {error, term()}
</code></pre>
<br />


Return the list of assigned workers

Example:

```

 {ok, Workers} = poolgirl:assigned(test).
```

<a name="checkin-1"></a>

### checkin/1 ###

<pre><code>
checkin(Pid::pid()) -&gt; ok | {error, term()}
</code></pre>
<br />


Checkin a worker

Example:

```

 poolgirl:checkin(W).
```

<a name="checkout-1"></a>

### checkout/1 ###

<pre><code>
checkout(Pool::atom()) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />


Checkout a worker

Example:

```

 W = poolgirl:checkout(test).
```

<a name="remove_pool-1"></a>

### remove_pool/1 ###

<pre><code>
remove_pool(Name::atom()) -&gt; ok | {error, term()}
</code></pre>
<br />

Remove an existing pool

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(Pool::atom()) -&gt; {ok, integer(), integer()} | {error, term()}
</code></pre>
<br />


Get a worker size and number of unassigned workers.

Example:

```

 poolgirl:size(test).
```

