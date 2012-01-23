
About
=====

This library is about to implement the actor model on top of GHC's concurrency.
Actors works as VM's lightweight processes and messages handled as asynchronous
exceptions.

XXX
===

Exceptions kill threads. What to do now? Signals?

Usage
=====

The `Control.Concurrent.Actor' module provides the API that mimics Erlang's
concurrency primitives.

Data types and type classes
---------------------------

Processes are just a VM's threads:

    type Process = ThreadId

And messages is an exceptions:

    class Exception m => Message m

    instance (Show a, Typeable a) => Exception a
    instance Exception m => Message m

Thus, any data type can be a message. Note that a process itself is also a
message, as in Erlang.

Spawning new processes
----------------------

For spawning a new process there is a function:

    spawn :: IO () -> IO Process

that can be used like this:

    spawn_1 = do
      hSetBuffering stdout LineBuffering
      child <- spawn $ do
        putStrLn $ "  Hi, I'am the child"
        me <- self
        putStrLn $ "  My PID = " ++ show me
      putStrLn $ "I was create a child with PID = " ++ show child

    > spawn_1
      Hi, I'am the child
      My PID = ThreadId 308
    I was create a child with PID = ThreadId 308

using the function:

    self :: IO Process

to determine our PID.

Also, there is a more generic function:

    actor :: forall t. t -> (t -> IO ()) -> IO Process

which can be used to pass any information from parrent to child (something like
an initial message). This is an example:

    data Child = Child String Process

    actor_1 = do
      hSetBuffering stdout LineBuffering
      me <- self
      putStrLn $ "Hi, I'am the parrent, my PID = " ++ show me
      child <- actor (Child "child" me) $
        \(Child name parent) -> do
          putStrLn $ "  Hi, I'am the one who was called " ++ name
          me <- self
          putStrLn $ "  My PID = " ++ show me
          putStrLn $ "  My parrent's PID = " ++ show parent
      putStrLn $ "I was create a child with PID = " ++ show child

    > actor_1
    Hi, I'am the parrent, my PID = ThreadId 335
      Hi, I'am the one who was called child
      My PID = ThreadId 336
      My parrent's PID = ThreadId 335
    I was create a child with PID = ThreadId 336

There are other functions to manipulate processes, including:

    kill :: Process -> IO ()

    exit :: IO ()

    sleep :: Int -> IO ()

    wait :: forall a. IO a

see the haddock for more details.

Sending messages
----------------

There are two identical funtions:

    send :: Message m => IO Process -> m -> m

    (!) :: Message m => IO Process -> m -> m

intended to send a messages to processes. For example:

    self ! "hello"
    *** Exception: "hello"

Also, there are variants of this functions which takes messages form inside the
IO:

    sendIO :: Message m => IO Process -> IO m -> IO m

    (<!) :: Message m => IO Process -> IO m -> IO m

thus, we can pipelining messages:

    self <! self ! "hello"
    *** Exception: "hello"

Receiving messages
------------------

There are two funtions:

    (?) :: Message m => forall a. IO a -> (m -> IO a) -> IO a

    receive :: Message m => forall a. (m -> IO a) -> IO a

intended to receiving messages. The second function is a special case of the
first one.

Lets take this Erlang code from the ["Learn You Some Erlang"](http://learnyousomeerlang.com) book:

    dolphin1() ->
      receive
        do_a_flip ->
          io:format("How about no?~n");
        fish ->
          io:format("So long and thanks for all the fish!~n");
        _ ->
          io:format("Heh, we're smarter than you humans.~n")
      end

it's turns to:

    new_dolphin = receive $
      \message -> case message of
        "do a flip" -> putStrLn "How about no?"
        "fish" -> putStrLn "So long and thanks for all the fish!"
        _ -> putStrLn "Heh, we're smarter than you humans."

    test_dolphin = do
      hSetBuffering stdout LineBuffering
      let dolphin = spawn new_dolphin
      dolphin ! "do a flip"
      dolphin ! "fish"
      dolphin ! "oh, hello dolphin!"
      return ()

    > test_dolphin
    How about no?
    So long and thanks for all the fish!
    Heh, we're smarter than you humans.

looks like it works.

XXX: No, it's don't. Every message run a new process in above example. But a
single exception kill a thread.

Issues
======

* Implement the *flush* funtion (to show already received messages)?
