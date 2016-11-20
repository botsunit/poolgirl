

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
pool_options() = #{size =&gt; integer(), chunk_size =&gt; integer(), max_age =&gt; integer(), max_size =&gt; integer(), clean_interval =&gt; integer(), retry_interval =&gt; integer(), allow_empty_pool =&gt; true | false, max_retry =&gt; integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_pool-1">add_pool/1</a></td><td>
Start a configured pool.</td></tr><tr><td valign="top"><a href="#add_pool-2">add_pool/2</a></td><td>Equivalent to <a href="#add_pool-3"><tt>add_pool(Name, MFArgs, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#add_pool-3">add_pool/3</a></td><td> 
Create a new pool.</td></tr><tr><td valign="top"><a href="#assigned-1">assigned/1</a></td><td> 
Return the list of assigned workers.</td></tr><tr><td valign="top"><a href="#checkin-1">checkin/1</a></td><td> 
Checkin a worker.</td></tr><tr><td valign="top"><a href="#checkout-1">checkout/1</a></td><td> 
Checkout a worker.</td></tr><tr><td valign="top"><a href="#pools-0">pools/0</a></td><td> 
Return the list of pools.</td></tr><tr><td valign="top"><a href="#remove_all_pools-0">remove_all_pools/0</a></td><td>
Remove all existing pools.</td></tr><tr><td valign="top"><a href="#remove_pool-1">remove_pool/1</a></td><td>
Remove an existing pool.</td></tr><tr><td valign="top"><a href="#remove_pools-1">remove_pools/1</a></td><td>
Remove a list of existing pools.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td> 
Get a worker size and number of unassigned workers.</td></tr><tr><td valign="top"><a href="#transaction-2">transaction/2</a></td><td> 
Checkout a worker from the given pool and execute a function with the worker as parameter.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_pool-1"></a>

### add_pool/1 ###

<pre><code>
add_pool(Name::atom()) -&gt; {ok, integer()} | {error, term()}
</code></pre>
<br />

Start a configured pool

<a name="add_pool-2"></a>

### add_pool/2 ###

<pre><code>
add_pool(Name::atom(), MFArgs::<a href="#type-mfargs">mfargs()</a>) -&gt; {ok, integer()} | {error, term()}
</code></pre>
<br />

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

* `max_size :: integer()` : Maximum number or worker in the pool (Default: infinity).

* `clean_interval :: integer()` : Interval (in ms) between each cleanup (Default : 60000).

* `max_retry :: integer()` : Number of new attempts to acquire workers if none is available (Default : 0).

* `retry_interval :: integer()` : Interval (in ms) between workers acquisition attempts (Default : 100).

* `allow_empty_pool :: true | false` : If this option is set to true and the pool is empty at start, it is removed (Default: false).


_Warning_ : If `max_size =< size + chunk_size` then `max_size` is set to `size + chunk_size`

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

<a name="pools-0"></a>

### pools/0 ###

<pre><code>
pools() -&gt; [atom()]
</code></pre>
<br />


Return the list of pools

Example:

```

 poolgirl:size(test).
```

<a name="remove_all_pools-0"></a>

### remove_all_pools/0 ###

<pre><code>
remove_all_pools() -&gt; ok | [{error, term(), term()}]
</code></pre>
<br />

Remove all existing pools

<a name="remove_pool-1"></a>

### remove_pool/1 ###

<pre><code>
remove_pool(Name::atom()) -&gt; ok | {error, term()}
</code></pre>
<br />

Remove an existing pool

<a name="remove_pools-1"></a>

### remove_pools/1 ###

<pre><code>
remove_pools(Pools::[atom()]) -&gt; ok | [{error, term()}]
</code></pre>
<br />

Remove a list of existing pools

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

<a name="transaction-2"></a>

### transaction/2 ###

<pre><code>
transaction(Pool::atom(), Fun::fun((Worker::pid()) -&gt; Result::term())) -&gt; Result::term() | {error, term()}
</code></pre>
<br />


Checkout a worker from the given pool and execute a function with the worker as parameter.

Example:

```

 poolgirl:transaction(test, fun(Worker) ->
   gen_server(Worker, {do, Something})
 end).
```

