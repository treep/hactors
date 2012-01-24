{-# LANGUAGE Rank2Types #-}

-- |
-- Implementation of the actor model on top of the GHC's concurrency.
-- 
-- The API mimics Erlang's concurrency primitives, with slight differences.
-- 
module Control.Concurrent.Actor where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan

-- -----------------------------------------------------------------------------
-- * Processes.

-- | The process is a VM's thread.
type Process = ThreadId

-- | Get the current process.
self :: IO Process
self = myThreadId

-- | Kill the process.
kill :: IO Process -> IO ()
kill = (>>= killThread)

-- | Finish the current process.
exit :: IO ()
exit = kill self

-- | Perform non busy waiting for a given number of microseconds in the current
-- process.
sleep :: Int -> IO ()
sleep = threadDelay

-- | Perform an infinite non busy waiting in the current process.
wait :: IO ()
wait = forever $ sleep slice
  where slice = maxBound :: Int

-- -----------------------------------------------------------------------------
-- * The message box.

-- | The message box is represented by the STM's channel.
type MBox m = TChan m

-- -----------------------------------------------------------------------------
-- * Actors.

-- | The actor is a process associated with the message box.
-- 
-- Note that the actor is parameterized by the type of message that it can
-- accept.
data Actor m = Actor
  { proc :: Process
  , mbox :: MBox m
  }

-- | Create a new actor from a function, send the initial message and the 
-- message box to this actor via function arguments.
-- 
-- This function calls @forkIO@.
-- 
actor :: forall t m a. t -> (t -> MBox m -> IO a) -> IO (Actor m)
actor i f = do
  m <- newTChanIO
  p <- forkIO $ f i m >> return ()
  return $ Actor p m

-- | Create a new actor from a function, send the message box to this actor via
-- function arguments.
-- 
-- This function calls @forkIO@.
-- 
spawn :: forall m a. (MBox m -> IO a) -> IO (Actor m)
spawn = actor () . const

-- -----------------------------------------------------------------------------
-- * Messages.

infixl 1 ?, <?
infixr 2 !, <!, !>, <!>

-- | Wait for an asynchronous message on the actor's channel.
receive :: Actor m -> (m -> IO a) -> IO b
receive a f = forever $ atomically (readTChan $ mbox a) >>= f

-- | Infix variant of @receive@.
(?) :: Actor m -> (m -> IO a) -> IO b
(?) = receive

-- | Variant of (?) with actor inside the IO.
(<?) :: IO (Actor m) -> (m -> IO a) -> IO b
a <? f = a >>= \a' -> a' ? f

-- | Send a message to the actor.
send :: Actor m -> m -> IO m
send a m = atomically $ mbox a `writeTChan` m >> return m

-- | Infix variant of @send@.
(!) :: Actor m -> m -> IO m
(!) = send

-- | Variant of (!) with actor inside the IO.
(<!) :: IO (Actor m) -> m -> IO m
a <! m = a >>= \a' -> a' ! m

-- | Variant of (!) with message inside the IO.
(!>) :: Actor m -> IO m -> IO m
a !> m = m >>= \m' -> a ! m'

-- | Variant of (!) with actor and message inside the IO.
(<!>) :: IO (Actor m) -> IO m -> IO m
a <!> m = a >>= \a' -> m >>= \m' -> a' ! m'
