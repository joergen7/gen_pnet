# gen_pnet [![hex.pm](https://img.shields.io/hexpm/v/gen_pnet.svg?style=flat-square)](https://hex.pm/packages/gen_pnet) [![Build Status](https://travis-ci.org/joergen7/gen_pnet.svg?branch=master)](https://travis-ci.org/joergen7/gen_pnet)

A generic Petri net OTP library.

The major advantage of modeling applications with Petri nets is that they provide a natural view on the concurrent behavior of an application. This is achieved by making explicit the preconditions for an operation to be carried out while leaving implicit how and when an operation is triggered and how independent operations are timed.

This OTP library is a framework for programming with Petri nets. It implements a very general form of Petri nets using Erlang terms as tokens. This means that (i) tokens are not only markers but can be any data structure conceivable in Erlang, (ii) a place can hold any number of tokens not just one, (iii) transitions can perform any computation conceivable in Erlang.

The Petri net is specified by implementing a set of callback functions (much like the `gen_fsm` behavior) declaring the place names, the transition names, the preset for each transition, in what modes a transition is enabled, what happens, when a transition fires in a given mode, and the net's initial marking. To communicate with the outside world, callback functions handling calls, casts, and unformatted messages can be provided. Finally, the user can specify a trigger function that is called for each token that is about to emerge on a place. This trigger function can devise side effects and can either let the token be created normally or make it vanish. Both terminating and live nets can be defined using `gen_pnet` and even though a live net never finishes to make progress, the net instance is constantly responsive to outside requests.

# Usage

This section shows how Petri nets are started, queried, and manipulated with `gen_pnet`. We demonstrate the `gen_pnet` API by constructing a cookie vending machine. The [source code]() of the cookie vending machine module is part of the [example collection]().

![Cookie vending machine Petri net](gen_pnet/blob/dev/priv/cvm2.png)
*Cookie vending machine example net*
 
## Using the Cookie Vending Machine Example

In the following we use the cookie vending machine example Petri net. The [source code]() for this net is available online. First we compile the library and start an interactive Erlang shell using [rebar3](https://github.com/erlang/rebar3).

    rebar3 shell

We start the cookie vending machine by using `gen_pnet:start_link/2`. Its first argument is the callback module defining the cookie vending machine. It must implement all callback functions in the `gen_pnet` behavior. The second argument is an option list, identical to the one used in the `gen_server:start_link/n` functions. `gen_pnet:start_link/2` returns the process id of the just created Petri net process.

    {ok, Pid} = gen_pnet:start_link( cvm2, [] ).
    {ok, <0.115.0>}

Now that the Petri net is running we can query the content of its places with `gen_pnet:ls/2`. This Petri net has five places: `coin_slot`, `cash_box`, `signal`, `compartment`, and `storage`. Initially, all places are empty except the `storage` place which holds three cookie packages.

    gen_pnet:ls( Pid, storage ).
    {ok,[cookie_box,cookie_box,cookie_box]}

    gen_pnet:ls( Pid, compartment ).
    {ok, []}

    gen_pnet:ls( Pid, some_place_that_does_not_exist ).
    {error,{bad_place, some_place_that_does_not_exist}}

To interact with the cookie vending machine we insert a coin by adding an according token to the `coin_slot` place. This can be done with the `gen_pnet:produce_token/3` function which takes a reference to a Petri net instance, a place name, and a token which is added to that place.

    gen_pnet:produce_token( Pid, coin_slot, coin ).
    ok

The effect of inserting a coin is that the coin has wandered to the `cash_box` place, leaving the `coin_slot` place empty again. Also, a cookie token should have appeared in the formerly empty `compartment` place while the number of cookie packages in the `storage' place should have been reduced by one. We can check this by querying the places as previously.

    gen_pnet:ls( Pid, cash_box ). 
    {ok,[coin]}

    gen_pnet:ls( Pid, compartment ).
    {ok,[cookie_box]}

    gen_pnet:ls( Pid, storage ).
    {ok,[cookie_box,cookie_box]}

## Implementing the Callback Functions

In the previous section we showed how the `gen_pnet' API can be used to create and use predefined Petri nets. Now, let us have a look at the callback functions that need to be implemented to define a Petri net. These are the six callbacks
<ul>
  <li>`init/1' which gives the initial marking of the net</li>
  <li>`place_lst/0', `trsn_lst/0', and `preset/1' which define the net structure</li>
  <li>`enum_consume_lst/3' which determines when a transition is enabled and what tokens it would consume if it were to fire</li>
  <li>`fire/3' which defines what happens when a transition actually fires and what tokens it produces</li>
</ul>
We have a look at each of them in turn.

<h4>init/1</h4>

The `init/1' function gives us the chance to define the initial marking of the Petri net in the form of a token list. Additionally, we can create a user info data structure which is handed also to the functions `enum_consume_lst/3' and `fire/3'.

```
init( _InitArg ) ->
  InitMarking = #{ storage => [cookie_box, cookie_box, cookie_box] },
  {ok, InitMarking, []}.
'''
<h4>place_lst/0</h4>
```
place_lst() ->
  [coin_slot, cash_box, signal, storage, compartment].
'''
<h4>trsn_lst/0</h4>
```
trsn_lst() -> [a, b].
'''
<h4>preset/1</h4>
```
preset( a ) -> [coin_slot];
preset( b ) -> [signal, storage].
'''
<h4>enum_consume_map/3</h4>
```
enum_consume_map( a, #{ coin_slot := CLst }, _UserInfo ) ->
  [#{ coin_slot => [C] } || C <- CLst];

enum_consume_map( b, #{ signal := SgLst, storage := StLst }, _UserInfo ) ->
  [#{ signal => [Sg], storage => [St] } || Sg <- SgLst, St <- StLst].
'''
<h4>fire/3</h4>
The `fire/3' function defines what happens when a given transition fires and what tokens are produced. As arguments it takes the name of the transition, a list of tokens to be consumed which is a randomly selected list from the lists returned by `enum_consume_lst/3', as well as the user info data structure as returned by the `init/1' function.

The cookie vending machine has two transitions `a' and `b'. The `fire/3' function has to defined for both transitions. Transition `a' always consumes single coin tokens.
```
fire( a, _ConsumeMap, _UserInfo ) ->
  #{ signal => [sig], cash_box => [coin] };

fire( b, _ConsumeMap, _UserInfo ) ->
  #{ compartment => [cookie_box]}.
'''


# Resources

- [joergen7/dinner](https://github.com/joergen7/dinner). A collection of examples using `gen_pnet`.
- [aabs/gen_pn](https://github.com/aabs/gen_pn). An alternative Erlang/OTP compatible Petri net library.

# License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)