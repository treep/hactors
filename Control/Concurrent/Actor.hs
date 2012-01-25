
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
import Control.Exception

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
-- 
data Actor m = Actor
  { proc :: Process
  , mbox :: MBox m
  }

-- | Create a new actor from a function, send the initial argument and the
-- message box to this actor via function arguments.
-- 
-- This function calls @forkIO@.
-- 
actor :: t -> (t -> MBox m -> IO a) -> IO (Actor m)
actor i f = do
  m <- newTChanIO
  p <- forkIO $ f i m >> return ()
  return $ Actor p m

-- | Create a new actor from a function, send the message box to this actor via
-- function argument.
-- 
-- This function calls @forkIO@.
-- 
spawn :: (MBox m -> IO a) -> IO (Actor m)
spawn = actor () . const

-- -----------------------------------------------------------------------------
-- * Messages.

infixl 1 ?, <?
infixr 2 !, <!, !>, <!>

-- | Wait for an asynchronous message in the message box.
receive :: MBox m -> (m -> IO a) -> IO b
receive mb f = forever $ atomically (readTChan mb) >>= f

-- | Infix variant of @receive@.
(?) :: MBox m -> (m -> IO a) -> IO b
(?) = receive

-- | Variant of (?) with the message box inside the IO.
(<?) :: IO (MBox m) -> (m -> IO a) -> IO b
mb <? f = mb >>= (? f)

-- | Send a message to the actor.
send :: Actor m -> m -> IO m
send a m = atomically $ mbox a `writeTChan` m >> return m

-- | Infix variant of @send@.
(!) :: Actor m -> m -> IO m
(!) = send

-- | Variant of (!) with the actor inside the IO.
(<!) :: IO (Actor m) -> m -> IO m
a <! m = a >>= (! m)

-- | Variant of (!) with the message inside the IO.
(!>) :: Actor m -> IO m -> IO m
a !> m = m >>= (a !)

-- | Variant of (!) with the actor and the message inside the IO.
(<!>) :: IO (Actor m) -> IO m -> IO m
a <!> m = a >>= \a' -> m >>= (a' !)

-- -----------------------------------------------------------------------------
-- * Combine actors with messages.

-- | Create a new receiving actor.
-- 
-- This function calls @forkIO@.
-- 
spawn_receive :: (m -> IO a) -> IO (Actor m)
spawn_receive f = spawn (? f)

-- -----------------------------------------------------------------------------
-- * Fault tolerance.
-- 
-- Where "fault" means having an exception in the thread. Still, AFAIK GHC's
-- runtime can't survive if some thread has segmentation fault (e.g. perform
-- (unsafeCoerce id)).
--

-- | Perform an action ignoring any exception in it.
tolerant :: IO () -> IO ()
tolerant = handle $ \e -> let _ = e :: SomeException in return undefined

-- | Perform an action, do @exit@ on exceptions.
-- 
-- XXX Bad name?
-- 
faultable :: IO () -> IO ()
faultable = handle $ \e -> let _ = e :: SomeException in exit
