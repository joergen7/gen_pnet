# gen_pnet [![hex.pm](https://img.shields.io/hexpm/v/gen_pnet.svg?style=flat-square)](https://hex.pm/packages/gen_pnet) [![Build Status](https://travis-ci.org/joergen7/gen_pnet.svg?branch=master)](https://travis-ci.org/joergen7/gen_pnet)

A generic Petri net OTP library.

The major advantage of modeling applications with Petri nets is that they provide a natural view on the concurrent behavior of an application. This is achieved by making explicit the preconditions for an operation to be carried out while leaving implicit how and when an operation is triggered and how independent operations are timed.

This OTP library is a framework for programming with Petri nets. It implements a very general form of Petri nets using Erlang terms as tokens. This means that (i) tokens are not only markers but can be any data structure conceivable in Erlang, (ii) a place can hold any number of tokens not just one, (iii) transitions can perform any computation conceivable in Erlang.

The Petri net is specified by implementing a set of callback functions (much like the `gen_fsm` behavior) declaring the place names, the transition names, the preset for each transition, in what modes a transition is enabled, what happens, when a transition fires in a given mode, and the net's initial marking. To communicate with the outside world, callback functions handling calls, casts, and unformatted messages can be provided. Finally, the user can specify a trigger function that is called for each token that is about to emerge on a place. This trigger function can devise side effects and can either let the token be created normally or make it vanish. Both terminating and live nets can be defined using `gen_pnet` and even though a live net never finishes to make progress, the net instance is constantly responsive to outside requests.

# Usage



# Resources

- [joergen7/dinner](https://github.com/joergen7/dinner). A collection of examples using `gen_pnet`.
- [aabs/gen_pn](https://github.com/aabs/gen_pn). An alternative Erlang/OTP compatible Petri net library.

# License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)