{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables,
             FlexibleInstances, UndecidableInstances #-}

module Control.Concurrent.Actor where

import Prelude hiding ( catch )

import Data.Typeable

import Control.Monad
import Control.Concurrent
import Control.Exception

-- -----------------------------------------------------------------------------
-- * Processes.

type Name = String

-- | Process is just a VM's thread.
data Process = Process
  { pname :: Name                       -- ^ Process name.
  , pid   :: ThreadId                   -- ^ Process ID.
  } deriving ( Show, Eq, Typeable )

-- | Create a new named process from an action (call to @forkIO@).
actor :: Name -> IO () -> IO Process
actor name action = forkIO action >>= return . Process name

-- | Create a new anonymous process from an action (call to @forkIO@).
spawn :: IO () -> IO Process
spawn = actor "anonymous"

-- | Get the current process.
self :: IO Process
self = myThreadId >>= return . Process "me" 

-- | Kill the process.
kill :: Process -> IO ()
kill = killThread . pid

-- | Finish the current process.
exit :: IO ()
exit = self >>= kill

-- -----------------------------------------------------------------------------
-- * Messages.

infixr 0 ?
infixr 1 !

threadDelaySlice :: Int
threadDelaySlice = (10 :: Int) ^ (9 :: Int)

-- | The receive / send abstract interface.
class Message m where
  -- | Do some action and wait for asynchronous message.
  (?) :: IO a -> (m -> IO a) -> IO a
  -- | Just wait for asynchronous message.
  receive :: (m -> IO a) -> IO a
  receive f = forever (threadDelay threadDelaySlice) ? f
  -- | Send a message to process.
  send :: Process -> m -> IO ()
  -- | Infix alias for @send@.
  (!) :: Process -> m -> IO ()
  (!) = send

-- | Messages handled as exceptions.
instance Exception m => Message m where
  (?)  = catch
  send = throwTo . pid

-- | @String@s is a messages.
instance Exception String

{-
  Thus, any (non-polymorphic?) data type can be a message. Process itself can 
  be a message.
 -}
