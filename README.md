# A generic Petri net OTP library

Some applications exhibit behavioral patterns that lend themselves to Petri nets. The major advantage of modeling applications with Petri nets is that they provide a natural view on the concurrent behavior of an application. This is achieved by making explicit the preconditions for an operation to be carried out while leaving implicit how and when an operation is triggered and what other operations might run in parallel.

This OTP library is a framework for programming with Petri nets. It implements a very general form of Petri nets: non-elementary, (in theory) unbounded Petri nets. I.e., tokens may not only be markers but carry additional information. Furthermore, a place can hold any number of tokens not just one.

While many simulation libraries only mimic the concurrent behavior of Petri nets, this library allows the definition of nets with an arbitrary number of transitions competing for a place's tokens neither imposing order in the form of an orverarching loop nor otherwise constraining parallelism.