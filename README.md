# gen_pnet [![hex.pm](https://img.shields.io/hexpm/v/gen_pnet.svg?style=flat-square)](https://hex.pm/packages/gen_pnet) [![Build Status](https://travis-ci.org/joergen7/gen_pnet.svg?branch=master)](https://travis-ci.org/joergen7/gen_pnet)

A generic Petri net OTP behavior.

The major advantage of modeling applications with Petri nets is that they provide a natural view on the concurrent behavior of an application. This is achieved by making explicit the preconditions for an operation to be carried out while leaving implicit how and when an operation is triggered and how independent operations are timed.

This OTP behavior allows programming with Petri nets. It implements a very general form of Petri nets using Erlang terms as tokens. This means that (i) tokens are not only markers but can be any data structure conceivable in Erlang, (ii) a place can hold any number of tokens not just one, (iii) transitions can perform any computation conceivable in Erlang.

The Petri net is specified by implementing a set of callback functions (much like the `gen_fsm` behavior) declaring the place names, the transition names, the preset for each transition, in what modes a transition is enabled, what happens, when a transition fires in a given mode, and the net's initial marking. To communicate with the outside world, callback functions handling calls, casts, and unformatted messages can be provided. Finally, the user can specify a trigger function that is called for each token that is about to emerge on a place. This trigger function can devise side effects and can either let the token be created normally or make it vanish. Both terminating and live nets can be defined using `gen_pnet` and even though a live net never finishes to make progress, the net instance is constantly responsive to outside requests.

# Adding gen_pnet to a Project

## rebar3

To integrate `gen_pnet` into a rebar3 managed project change the `deps` entry in your application's `rebar.config` file to include the tuple `{gen_pnet, "0.1.1"}`.

    {deps, [{gen_pnet, "0.1.1"}]}.

## mix

    {:lib_combin, "~> 0.1.1"}

# Usage

This section shows how Petri nets are started, queried, and manipulated with `gen_pnet`. We demonstrate the `gen_pnet` API by constructing a cookie vending machine. The [source code]() of the cookie vending machine module is part of the [example collection]().

![Cookie vending machine Petri net](https://github.com/joergen7/gen_pnet/blob/dev/priv/cvm2.png)

*Cookie vending machine example net. Place and transition names are atoms while, in this example, tokens are also atoms.*

## Implementing the Callback Functions

Let us have a look at the callback functions that need to be implemented to define a Petri net.

### Net Callback Functions

There are six callbacks that define the Petri net structure and its initial marking:

- `place_lst/0` returns the names of the places in the net
- `trsn_lst/0` returns the names of the transitions in the net
- `init_marking/1` returns the initial marking for a given place
- `preset/1` returns the preset places of a given transition
- `is_enabled/2` determines whether a given transition is enabled in a given mode
- `fire/2` returns which tokens are produced on what places if a given transition is fired in a given mode that enables this transition

We have a look at each of them in turn.

#### place_lst/0

The `place_lst/0` function lets us define the names of all places in the net.

    place_lst() ->
      [coin_slot, cash_box, signal, storage, compartment].

Here, we define the net to have the five places in the cookie vending machine.

#### trsn_lst/0

The `trsn_lst/0` function lets us define the names of all transitions in the net:

    trsn_lst() ->
      [a, b].

Here, we define the net to have the two places `a` and `b` in the cookie vending machine.

#### preset/1

The `preset/1` lets us define the preset places of a given transition.

    preset( a ) -> [coin_slot];
    preset( b ) -> [signal, storage].

Here, we define the preset of the transition `a` to be just the place `coin_slot` while the transition `b` has the places `signal` and `storage` in its preset.

#### init_marking/1

The `init_marking/1` function lets us define the initial marking for a given place in the form of a token list.

    init_marking( storage ) -> [cookie_box, cookie_box, cookie_box];
    init_marking( _ )       -> [].

Here, we initialize the storage place with three `cookie_box` tokens. All other places are left empty.

#### is_enabled/2

The `is_enabled/2` function is a predicate determining whether a given transition is enabled in a given mode.

    is_enabled( a, #{ coin_slot := [coin] } )                      -> true;
    is_enabled( b, #{ signal := [sig], storage := [cookie_box] } ) -> true;
    is_enabled( _, _ )                                             -> false.

Here, we state that the transition `a` is enabled if it can consume a single `coin` from the `coin_slot` place. Similarly, the transition `b` is enabled if it can consume a `sig` token from the `signal` place and a `cookie_box` token from the `storage` place. No other configuration can enable a transition. E.g., managing to get a `button` token on the `coin_slot` place will not enable any transition.

#### fire/2

The `fire/2` function defines what tokens are produced when a given transition fires in a given mode. As arguments it takes the name of the transition, and a firing mode in the form of a hash map mapping place names to token lists. The `fire/2` function is called only on modes for which `is_enabled/2` returns `true`. The `fire/2` function is expected to return either a `{produce, ProduceMap}` tuple or the term `abort`. If `abort` is returned, the firing is aborted. Nothing is produced or consumed.

    fire( a, _ ) -> {produce, #{ cash_box => [coin], signal => [sig] }};
    fire( b, _ ) -> {produce, #{ compartment => [cookie_box] }}.

Here, the firing of the transition `a` produces a `coin` token on the `cash_box` place and a `sig` token on the `signal` place. Similarly, the firing of the transition `b` produces a `cookie_box` token on the `compartment` place. We do not need to state the tokens to be consumed because the firing mode already uniquely identifies the tokens to be consumed.


### Interface Callback Functions

In addition to the net callback functions there are another six callback functions that determine how the net instance appears as an Erlang actor to the outside world:

- `code_change/3` determines what happens when a hot code reload happens
- `handle_call/3` synchronous message exchange
- `handle_cast/2` asynchronous message reception
- `handle_info/2` asynchronous reception of an unformatted message
- `terminate/2` determines what happens when the net instance is stopped
- `trigger/2` allows to add a side effects to the generation of a token

#### code_change/3

The `code_change/3` function determines what happens when a hot code reload happens. This callback is identical to the `code_change/3` function in the `gen_server` behavior.

    code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.

#### handle_call/3

The `handle_call/3` function performs a synchronous exchange of messages between the caller and the net instance. The first argument is the request message, the second argument is a tuple identifying the caller, and the third argument is a `#net_state{}` record instance describing the current state of the net. The `handle_call/3` function can either generate a reply without changing the net marking by returning a `{reply, Reply}` tuple or it can generate a reply, consuming or producing tokens by returning a `{reply, Reply, ConsumeMap, ProduceMap}` tuple.

    handle_call( insert_coin, _, _ ) ->
      {reply, ok, #{}, #{ coin_slot => [coin] }};

    handle_call( remove_cookie_box, _,
                 #net_state{ marking = #{ compartment := C } } ) ->

      case C of
        []    -> {reply, {error, empty_compartment}};
        [_|_] -> {reply, ok, #{ compartment => [cookie_box] }, #{}}
      end;

    handle_call( _, _, _ ) -> {reply, {error, bad_msg}}.

Here, we react to two kinds of messages: Inserting a coin in the coin slot and removing a cookie box from the compartment. Thus, we react to an `insert_coin` message by replying with `ok`, consuming nothing and producing a `coin` token on the `coin_slot` place. When receiving a `remove_cookie_box` message, we check whether the `compartment` place is empty, replying with an error message if it is, otherwise replying with `ok`, consuming one `cookie_box` token from the `compartment` place, and producing nothing. Calls that are neither `insert_coin` nor `remove_cookie_box` are responded to with an error message.

#### handle_cast/2

The `handle_cast/2` function reacts to an asynchronous message received by the net instance. The first argument is the request while the second argument is a `#net_state{}` record instance. The `handle_cast/2` function can either leave the net unchanged by returning `noreply` or it can consume or produce tokens by returning a `{noreply, ConsumeMap, ProduceMap}` tuple.

    handle_cast( _Request, _NetState ) -> noreply.

Here, we just ignore any cast.

#### handle_info/2

The `handle_info/2` function reacts to an asynchronous, unformatted message received by the net instance. The first argument is the message term while the second argument is a `#net_state{}` record instance. The `handle_info/2` function can either leave the net unchanged by returning `noreply` or it can consume or produce tokens by returning a `{noreply, ConsumeMap, ProduceMap}` tuple.

    handle_info( _Request, _NetState ) -> noreply.

Here, we just ignore any message.

#### terminate/2

The `terminate/2` function determines what happens when the net instance is stopped. The first argument is the reason for termination while the second argument is a `#net_state{}` record instance. This callback is identical to the `terminate/2` function in the `gen_server` behavior.

    terminate( _Reason, _NetState ) -> ok.

#### trigger/2

The `trigger/2` function determines what happens when a token is produced on a given place. Its first argument is the place name and its second argument is the token about to be produced. The `trigger/2` function is expected to return either `pass` in which case the token is produced normally, or `drop` in which case the token is forgotten.

    trigger( _, _ ) -> pass.

Here, we simply let any token pass.

## Using the Cookie Vending Machine Example

In the following we demonstrate how to start and play with the previously defined cookie vending machine example. You can either copy the above code in an Erlang callback module of your own or you can obtain the module from the [gen_pnet_examples](https://github.com/joergen7/gen_pnet_examples) repository. Here, we clone it from GitHub and compile it. Then we start an interactive Erlang shell using [rebar3](https://github.com/erlang/rebar3).

    git clone https://github.com/joergen7/gen_pnet_examples.git
    cd gen_pnet_examples
    rebar3 shell

Compiling with rebar3 also fetches the `gen_pnet` library. We start the cookie vending machine which is stored in the callback module `src/cmv.erl` by using `gen_pnet:start_link/2`.

    {ok, Pid} = gen_pnet:start_link( cvm, [] ).
    {ok, <0.115.0>}

 The first argument is the callback module defining the cookie vending machine. It must implement all callback functions in the `gen_pnet` behavior. The second argument is an option list, identical to the one used in the `gen_server:start_link/n` functions. On success, `gen_pnet:start_link/2` returns the process id of the just created Petri net process. Now that the Petri net is running we can query the content of its places with `gen_pnet:ls/2`. This Petri net has five places: `coin_slot`, `cash_box`, `signal`, `compartment`, and `storage`. Initially, all places are empty except the `storage` place which holds three cookie packages.

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


# Resources

- [joergen7/gen_pnet_examples](https://github.com/joergen7/gen_pnet_examples). A collection of examples using `gen_pnet`.
- [aabs/gen_pn](https://github.com/aabs/gen_pn). An alternative Erlang/OTP compatible Petri net library.


# License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)