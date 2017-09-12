# gen_pnet
###### A generic Petri net OTP behavior.
[![hex.pm](https://img.shields.io/hexpm/v/gen_pnet.svg?style=flat-square)](https://hex.pm/packages/gen_pnet) [![Build Status](https://travis-ci.org/joergen7/gen_pnet.svg?branch=master)](https://travis-ci.org/joergen7/gen_pnet)

The major advantage of modeling applications with Petri nets is that they provide a natural view on the concurrent behavior of an application. This is achieved by making explicit the preconditions for an operation to be carried out while leaving implicit how and when an operation is triggered and how independent operations are timed.

This OTP behavior allows programming with Petri nets. It implements a very general form of Petri nets using Erlang terms as tokens. This means that (i) tokens are not only markers but can be any data structure conceivable in Erlang, (ii) a place can hold any number of tokens not just one, (iii) transitions can perform any computation conceivable in Erlang.

The Petri net is specified by implementing a set of callback functions (much like the gen_fsm behavior) declaring the place names, the transition names, the preset for each transition, in what modes a transition is enabled, what happens, when a transition fires in a given mode, and the net's initial marking. To communicate with the outside world, callback functions handling calls, casts, and unformatted messages can be provided. Finally, the user can specify a trigger function that is called for each token that is about to emerge on a place. This trigger function can devise side effects and can either let the token be created normally or make it vanish. Both terminating and live nets can be defined using gen_pnet and even though a live net never finishes to make progress, the net instance is constantly responsive to outside requests. Conflicting transitions fire randomly and fairly.

The [documentation](https://cuneiform-lang.org/man/gen_pnet/index.html) of the gen_pnet module's API is available online.

## Usage

This section shows how the gen_pnet library can be added to your project, how Petri nets are defined, and how Petri net instances are started, queried, and manipulated. We demonstrate the API by constructing a cookie vending machine. The [source code](https://github.com/joergen7/gen_pnet_examples/blob/master/src/cvm.erl) of the cookie vending machine module is part of the [example collection](https://github.com/joergen7/gen_pnet_examples) for gen_pnet.

![Cookie vending machine Petri net](https://github.com/joergen7/gen_pnet/blob/dev/priv/cvm2.png)

*Cookie vending machine example net. Place and transition names are atoms while, in this example, tokens are also atoms.*

### Adding gen_pnet to a Project

#### rebar3

To integrate gen_pnet into a rebar3 managed project change the `deps` entry in your application's `rebar.config` file to include the tuple `{gen_pnet, "0.1.7"}`.

```erlang
{deps, [{gen_pnet, "0.1.7"}]}.
```

#### mix

```elixir
{:gen_pnet, "~> 0.1.7"}
```

### Defining a Petri net

Petri nets are defined by creating a callback module that implements the gen_pnet behavior by providing a number of callback functions.

### Callback Functions for the Net Structure

There are six callbacks that define the Petri net structure and its initial marking:

- `place_lst/0` returns the names of the places in the net
- `trsn_lst/0` returns the names of the transitions in the net
- `init_marking/2` returns the initial marking for a given place
- `preset/1` returns the preset places of a given transition
- `is_enabled/2` determines whether a given transition is enabled in a given mode
- `fire/3` returns which tokens are produced on what places if a given transition is fired in a given mode that enables this transition

We have a look at each of them in turn.

#### place_lst/0

```erlang
-callback place_lst() -> [atom()].
```

The `place_lst/0` function lets us define the names of all places in the net.

```erlang
place_lst() ->
  [coin_slot, cash_box, signal, storage, compartment].
```

Here, we define the net to have the five places in the cookie vending machine.

#### trsn_lst/0

```erlang
-callback trsn_lst() -> [atom()].
```

The `trsn_lst/0` function lets us define the names of all transitions in the net.

```erlang
trsn_lst() ->
  [a, b].
```

Here, we define the net to have the two places `a` and `b` in the cookie vending machine.

#### preset/1

```erlang
-callback preset( Trsn :: atom() ) -> [atom()].
```

The `preset/1` lets us define the preset places of a given transition.

```erlang
preset( a ) -> [coin_slot];
preset( b ) -> [signal, storage].
```

Here, we define the preset of the transition `a` to be just the place `coin_slot` while the transition `b` has the places `signal` and `storage` in its preset.

#### init_marking/2

```erlang
-callback init_marking( Place :: atom(), UsrInfo :: _ ) -> [_].
```

The `init_marking/2` function lets us define the initial marking for a given place in the form of a token list.

```erlang
init_marking( storage, _UsrInfo ) -> [cookie_box, cookie_box, cookie_box];
init_marking( _Place, _UsrInfo )  -> [].
```

Here, we initialize the storage place with three `cookie_box` tokens. All other places are left empty.

#### is_enabled/3

```erlang
-callback is_enabled( Trsn :: atom(), Mode :: #{ atom() => [_]}, UsrInfo :: _ ) ->
            boolean().
```

The `is_enabled/3` function is a predicate determining whether a given transition is enabled in a given mode.

```erlang
is_enabled( a, #{ coin_slot := [coin] }, _UsrInfo )                      -> true;
is_enabled( b, #{ signal := [sig], storage := [cookie_box] }, _UsrInfo ) -> true;
is_enabled( _Trsn, _Mode, _UsrInfo )                                     -> false.
```

Here, we state that the transition `a` is enabled if it can consume a single `coin` from the `coin_slot` place. Similarly, the transition `b` is enabled if it can consume a `sig` token from the `signal` place and a `cookie_box` token from the `storage` place. No other configuration can enable a transition. E.g., managing to get a `button` token on the `coin_slot` place will not enable any transition.

#### fire/3

```erlang
-callback fire( Trsn :: atom(), Mode :: #{ atom() => [_] }, UsrInfo :: _ ) ->
            abort | {produce, #{ atom() => [_] }}.
```

The `fire/3` function defines what tokens are produced when a transition fires in a given mode. As arguments it takes the name of the transition, a firing mode in the form of a hash map mapping place names to token lists, and a user info field that was generated by `init/1`. The `fire/3` function is called only on modes for which `is_enabled/2` returns `true`. The `fire/3` function is expected to return either a `{produce, ProduceMap}` tuple or the atom `abort`. If `abort` is returned, the firing is canceled. I.e., nothing is produced or consumed.

```erlang
fire( a, _Mode, _UsrInfo ) ->
  {produce, #{ cash_box => [coin], signal => [sig] }};

fire( b, _Mode, _UsrInfo ) ->
  {produce, #{ compartment => [cookie_box] }}.
```

Here, the firing of the transition `a` produces a `coin` token on the `cash_box` place and a `sig` token on the `signal` place. Similarly, the firing of the transition `b` produces a `cookie_box` token on the `compartment` place. We do not need to state the tokens to be consumed because the firing mode already uniquely identifies the tokens to be consumed.


### Callback Functions for the Actor Interface

In addition to the structure callback functions there are another seven callback functions that determine how the net instance appears as an Erlang actor to the outside world:

- `code_change/3` determines what happens when a hot code reload appears
- `handle_call/3` synchronous message exchange
- `handle_cast/2` asynchronous message reception
- `handle_info/2` asynchronous reception of an unformatted message
- `init/1` initializes the gen_pnet instance
- `terminate/2` determines what happens when the net instance is stopped
- `trigger/3` allows to add a side effects to the generation of a token

#### code_change/3

```erlang
-callback code_change( OldVsn :: _, NetState :: #net_state{}, Extra :: _ ) ->
            {ok, #net_state{}} | {error, _}.
```

The `code_change/3` function determines what happens when a hot code reload appears. This callback is identical to the `code_change/3` function in the gen_server behavior.

```erlang
code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.
```

#### handle_call/3

```erlang
-callback handle_call( Request :: _, From :: {pid(), _},
                       NetState :: #net_state{} ) ->
              {reply, _}
            | {reply, _, #{ atom() => [_] }, #{ atom() => [_] }}
            | noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _, _}.
```

The `handle_call/3` function performs a synchronous exchange of messages between the caller and the net instance. The first argument is the request message, the second argument is a tuple identifying the caller, and the third argument is a `#net_state{}` record instance describing the current state of the net. The `handle_call/3` function can either generate a reply without changing the net marking by returning a `{reply, Reply}` tuple or it can generate a reply, consuming or producing tokens by returning a `{reply, Reply, ConsumeMap, ProduceMap}` tuple.

```erlang
handle_call( insert_coin, _From, _NetState ) ->
  {reply, ok, #{}, #{ coin_slot => [coin] }};

handle_call( remove_cookie_box, _From, NetState ) ->

  case gen_pnet:get_ls( compartment, NetState ) of
    []    -> {reply, {error, empty_compartment}};
    [_|_] -> {reply, ok, #{ compartment => [cookie_box] }, #{}}
  end;

handle_call( _Request, _From, _NetState ) -> {reply, {error, bad_msg}}.
```

Here, we react to two kinds of messages: Inserting a coin in the coin slot and removing a cookie box from the compartment. Thus, we react to an `insert_coin` message by replying with `ok`, consuming nothing and producing a `coin` token on the `coin_slot` place. When receiving a `remove_cookie_box` message, we check whether the `compartment` place is empty, replying with an error message if it is, otherwise replying with `ok`, consuming one `cookie_box` token from the `compartment` place, and producing nothing. We can inspect the tokens on a given place by using the `get_ls/2` accessor function. Calls that are neither `insert_coin` nor `remove_cookie_box` are responded to with an error message.

#### handle_cast/2

```erlang
-callback handle_cast( Request :: _, NetState :: #net_state{} ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.
```

The `handle_cast/2` function reacts to an asynchronous message received by the net instance. The first argument is the request while the second argument is a `#net_state{}` record instance. The `handle_cast/2` function can either leave the net unchanged by returning `noreply` or it can consume or produce tokens by returning a `{noreply, ConsumeMap, ProduceMap}` tuple.

```erlang
handle_cast( _Request, _NetState ) -> noreply.
```

Here, we just ignore any cast.

#### handle_info/2

```erlang
-callback handle_info( Info :: _, NetState :: #net_state{} ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.
```

The `handle_info/2` function reacts to an asynchronous, unformatted message received by the net instance. The first argument is the message term while the second argument is a `#net_state{}` record instance. The `handle_info/2` function can either leave the net unchanged by returning `noreply` or it can consume or produce tokens by returning a `{noreply, ConsumeMap, ProduceMap}` tuple.

```erlang
handle_info( _Request, _NetState ) -> noreply.
```

Here, we just ignore any message.

#### init/1

```erlang
-callback init( Args :: _ ) -> UsrInfo :: _.
```

The `init/1` function initializes the net instance. It is given an initial argument which is provided with `gen_pnet:start_link/n`. The `init/1` function is expected to return a user info field which is later handed to other callback functions.

```erlang
init( _Args ) -> [].
```

Here, we return the empty list as a dummy user info field.

#### terminate/2

```erlang
-callback terminate( Reason :: _, NetState :: #net_state{} ) -> ok.
```

The `terminate/2` function determines what happens when the net instance is stopped. The first argument is the reason for termination while the second argument is a `#net_state{}` record instance. This callback is identical to the `terminate/2` function in the gen_server behavior.

```erlang
terminate( _Reason, _NetState ) -> ok.
```

#### trigger/3

```erlang
-callback trigger( Place :: atom(), Token :: _, NetState :: #net_state{} ) ->
            pass | drop.
```

The `trigger/3` function determines what happens when a token is produced on a given place. Its first argument is the place name, its second argument is the token about to be produced, and its third argument is the user info field generated by `init/1`. The `trigger/3` function is expected to return either `pass` in which case the token is produced normally, or `drop` in which case the token is forgotten.

```erlang
trigger( _Place, _Token, _UsrInfo ) -> pass.
```

Here, we simply let any token pass.

### Starting and Querying the Cookie Vending Machine Example

In the following we demonstrate how to start and play with the previously defined cookie vending machine example. You can either copy the above code in an Erlang callback module of your own or you can obtain the module from the [joergen7/gen_pnet_examples](https://github.com/joergen7/gen_pnet_examples) repository. Here, we clone it from GitHub and compile it. Then we start an interactive Erlang shell using [rebar3](https://github.com/erlang/rebar3).

    git clone https://github.com/joergen7/gen_pnet_examples.git
    cd gen_pnet_examples
    rebar3 shell

Compiling with rebar3 also fetches the gen_pnet library. We start the cookie vending machine which is stored in the callback module `src/cmv.erl` by using `gen_pnet:start_link/3`.

    {ok, Pid} = gen_pnet:start_link( cvm, [], [] ).
    {ok, <0.115.0>}

 The first argument is the callback module defining the cookie vending machine. It must implement all callback functions in the gen_pnet behavior. The second argument is an option list, identical to the one used in the `gen_server:start_link/n` functions. On success, `gen_pnet:start_link/3` returns the process id of the just created Petri net process. Now that the Petri net is running we can query the content of its places with `gen_pnet:ls/2`. This Petri net has five places: `coin_slot`, `cash_box`, `signal`, `compartment`, and `storage`. Initially, all places are empty except the `storage` place which holds three cookie packages.

    gen_pnet:ls( Pid, storage ).
    {ok,[cookie_box,cookie_box,cookie_box]}

    gen_pnet:ls( Pid, compartment ).
    {ok, []}

    gen_pnet:ls( Pid, some_place_that_does_not_exist ).
    {error,{bad_place,some_place_that_does_not_exist}}

To interact with the cookie vending machine we insert a coin by adding an according token to the `coin_slot` place. This can be done with the `gen_pnet:produce_token/3` function which takes a reference to a Petri net instance, a place name, and a token which is added to that place.

    gen_pnet:call( Pid, insert_coin ).
    ok

The effect of calling the net instance with `insert_coin` is that a coin is produced on the `coin_slot` place. Immediately, transition `a` fires making the coin wander to the `cash_box` place, leaving the `coin_slot` place empty again. Immediately afterwards, the transition `b` fires, making a `cookie_box` token appear in the formerly empty `compartment` place while the number of `cookie_box` tokens on the `storage` has reduced by one. We can check this by querying the places as previously.

    gen_pnet:ls( Pid, cash_box ). 
    {ok,[coin]}

    gen_pnet:ls( Pid, storage ).
    {ok,[cookie_box,cookie_box]}

    gen_pnet:ls( Pid, compartment ).
    {ok,[cookie_box]}

Now, we can remove the cookie box from the compartment by calling the net instance with `remove_cookie_box`.

    gen_pnet:call( Pid, remove_cookie_box ).
    ok

Calling with `remove_cookie_box` a second time will yield an error, since only one cookie box was bought.

    gen_pnet:call( Pid, remove_cookie_box ).
    {error,empty_compartment}

## System Requirements

- Erlang OTP 18.0 or higher
- Rebar3 3.0.0 or higher

## Resources

- [aabs/gen_pn](https://github.com/aabs/gen_pn). An alternative Erlang/OTP compatible Petri net library.
- [joergen7/gen_pnet_examples](https://github.com/joergen7/gen_pnet_examples). A collection of examples using gen_pnet.
- [joergen7/gruff](https://github.com/joergen7/gruff). A basic worker pool manager for Erlang to showcase gen_pnet.
- [joergen7/cre](https://github.com/joergen7/cre). A common runtime environment for distributed workflow languages.

## Authors

- Jörgen Brandt ([@joergen7](https://github.com/joergen7/)) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)
