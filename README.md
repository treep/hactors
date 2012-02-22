
About
=====

This library is about to implement the actor model on top of the GHC's
concurrency. Actors works as VM's lightweight threads and messages works with
STM's channels. There is also a swarms implementation, that is, a groups of
processes sharing a message box, and a mapReduce combinator implemented using
swarms.

Usage
=====

The *Control.Concurrent.Actor* module provides the API that mimics Erlang's
concurrency primitives. See the haddocks for more details.

Issues
======

* Timeouts? Process linking? Monitors? Kill siganl handling?

* Implement the *flush* funtion (to show already received messages)?

* Write more examples.
