{-# LANGUAGE Rank2Types, FlexibleInstances, UndecidableInstances #-}

-- |
-- Implementation of the actor model on top of GHC's concurrency.
-- 
-- The API mimics Erlang's concurrency primitives, with slight differences.
-- 
module Control.Concurrent.Actor where

import Prelude hiding ( catch )

import Data.Typeable

import Control.Monad
import Control.Concurrent
import Control.Exception

-- -----------------------------------------------------------------------------
-- * Processes.

-- | The definition -- process is a VM's thread.
type Process = ThreadId

-- | Get the current process.
self :: IO Process
self = myThreadId

-- | Kill the process.
kill :: Process -> IO ()
kill = killThread

-- | Finish the current process.
exit :: IO ()
exit = self >>= kill

-- | Create a new process from a function, send an initial message to process 
-- via function argument.
-- 
-- This function perform call to @forkIO@.
-- 
actor :: forall t. t -> (t -> IO ()) -> IO Process
actor message action = forkIO $ action message >>= return

-- | Create a new process from a function.
-- 
-- This function perform call to @forkIO@.
-- 
spawn :: IO () -> IO Process
spawn = actor () . const

-- | Perform non busy waiting for a given number of microseconds in the current
-- process.
sleep :: Int -> IO ()
sleep = threadDelay

-- | Perform an infinite non busy waiting in the current process. This waiting
-- may be interrupted by a message.
wait :: forall a. IO a
wait = forever $ sleep slice
  where slice = maxBound :: Int

-- -----------------------------------------------------------------------------
-- * Messages.

-- | The definition -- message is an exception.
class Exception m => Message m

-- | Everything is a message.
instance (Show a, Typeable a) => Exception a
instance Exception m => Message m

infixr 0 ?
infixr 1 ! , <!

-- | Perform some action and wait for an asynchronous message.
(?) :: Message m => forall a. IO a -> (m -> IO a) -> IO a
(?) = catch

-- | Just wait for an asynchronous message.
receive :: Message m => forall a. (m -> IO a) -> IO a
receive = (?) wait

-- | Send a message to the process.
send :: Message m => IO Process -> m -> IO m
send p m = do
  p' <- p
  throwTo p' m
  return m

-- | Infix alias for @send@.
(!) :: Message m => IO Process -> m -> IO m
(!) = send

-- | Send a message from inside the IO to the process.
sendIO :: Message m => IO Process -> IO m -> IO m
sendIO p m = m >>= (!) p

-- | Infix alias for @sendIO@.
(<!) :: Message m => IO Process -> IO m -> IO m
(<!) = sendIO
