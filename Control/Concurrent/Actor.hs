
-- |
-- Implementation of the actor model on top of the GHC's concurrency.
-- 
-- The API mimics Erlang's concurrency primitives, with slight differences.
-- 
module Control.Concurrent.Actor where

import Control.Applicative
import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception hiding ( onException )

-- -----------------------------------------------------------------------------
-- * Processes

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

-- | Print string to @stdout@ with the current process ID prefix.
say :: String -> IO ()
say s = do
  me <- self
  putStrLn $ show me ++ ": " ++ s

-- -----------------------------------------------------------------------------
-- * The message box

-- | The message box is represented by the STM's channel.
type MBox m = TChan m

-- -----------------------------------------------------------------------------
-- * Actors

-- | The actor is a process associated with the message box.
-- 
-- Note that the actor is parameterized by the type of message that it can
-- use.
-- 
data Actor m = Actor
  { actorProcess :: Process             -- ^ Actor's process.
  , actorMBox :: MBox m                 -- ^ Actor's message box.
  }

instance Eq (Actor m) where
  (Actor pid _) == (Actor pid' _) = pid == pid'

instance Ord (Actor m) where
  (Actor pid _) < (Actor pid' _) = pid < pid'

instance Show (Actor m) where
  show (Actor pid _) = show pid

-- | Create a new actor from a function, send the initial argument and the
-- message box to this actor via function arguments.
-- 
-- This function calls @forkIO@.
-- 
actor :: t -> (t -> MBox m -> IO a) -> IO (Actor m)
actor i f = do
  m <- newTChanIO
  p <- forkIO $ void $ f i m
  return $ Actor p m

-- | Create a new actor from a function, send the message box to this actor via
-- function argument.
-- 
-- This function calls @forkIO@.
-- 
spawn :: (MBox m -> IO a) -> IO (Actor m)
spawn = actor () . const

-- -----------------------------------------------------------------------------
-- * Messages

infixl 1 ?, <?
infixr 2 !, <!, !>, <!>

-- | Wait for an asynchronous message in the message box.
receive :: MBox m -> (m -> IO a) -> IO b
receive mb f = forever $ atomically (readTChan mb) >>= f

-- | Infix variant of @receive@.
(?) :: MBox m -> (m -> IO a) -> IO b
(?) = receive

-- | Variant of @?@ with the message box inside the IO.
(<?) :: IO (MBox m) -> (m -> IO a) -> IO b
mb <? f = mb >>= (? f)

-- | Send a message to the actor.
send :: Actor m -> m -> IO m
send a m = atomically $ actorMBox a `writeTChan` m >> return m

-- | Send a list of messages to the actor.
sendAll :: [m] -> Actor m -> IO ()
sendAll ms a = mapM_ (a !) ms

-- | Send a message to all the actors from the list.
sendToAll :: [Actor m] -> m -> IO m
sendToAll as m = mapM_ (! m) as >> return m

-- | Infix variant of @send@.
(!) :: Actor m -> m -> IO m
(!) = send

-- | Variant of @!@ with the actor inside the IO.
(<!) :: IO (Actor m) -> m -> IO m
a <! m = a >>= (! m)

-- | Variant of @!@ with the message inside the IO.
(!>) :: Actor m -> IO m -> IO m
a !> m = m >>= (a !)

-- | Variant of @!@ with the actor and the message inside the IO.
(<!>) :: IO (Actor m) -> IO m -> IO m
a <!> m = a >>= \a' -> m >>= (a' !)

-- -----------------------------------------------------------------------------
-- * Combine actors with messages

-- | Create a new receiving actor.
-- 
-- This function calls @forkIO@.
-- 
spawnReceive :: (m -> IO a) -> IO (Actor m)
spawnReceive f = spawn (? f)

-- -----------------------------------------------------------------------------
-- * Fault tolerance
-- 
-- Where "fault" means having an exception in the thread. Still, AFAIK GHC's
-- runtime can't survive if some thread has segmentation fault (e.g. perform
-- (unsafeCoerce id)).
--

-- | Perform an action, on exceptions perform a given action @f@.
onException :: IO a -> IO a -> IO a
onException f = handle $ \e -> let _ = e :: SomeException in f

-- | Perform an action ignoring any exceptions in it.
tolerant :: IO a -> IO a
tolerant = onException $ return undefined

-- | Perform an action, do @exit@ on exceptions.
-- 
-- XXX Bad name?
-- 
faultable :: IO () -> IO ()
faultable = onException exit

-- -----------------------------------------------------------------------------
-- * Swarms

-- | Swarm is a group of processes sharing a message box.
data Swarm m = Swarm
  { swarmProcesses :: [Process]         -- ^ A list of swarm's processes.
  , swarmMBox :: MBox m                 -- ^ Swarm's message box.
  }

instance Eq (Swarm m) where
  (Swarm pids _) == (Swarm pids' _) = pids == pids'

instance Ord (Swarm m) where
  (Swarm pids _) < (Swarm pids' _) = pids < pids'

instance Show (Swarm m) where
  show (Swarm pids _) = show pids

-- | Create a new swarm from a function, send the initial arguments and the
-- message boxes to each swarm's process via function arguments.
-- 
-- This function calls @forkIO@ several times.
-- 
swarm :: [t] -> (t -> MBox m -> IO a) -> IO (Swarm m)
swarm xs f = do
  m <- newTChanIO
  ps <- forM xs $ forkIO . void . flip f m
  return $ Swarm ps m

-- | Create a new swarm from a function, send the message boxes to each swarm's 
-- process via function arguments.
-- 
-- This function calls @forkIO@ several times.
-- 
swarm' :: (MBox m -> IO a) -> IO (Swarm m)
swarm' = swarm [] . const

-- -----------------------------------------------------------------------------
-- * MapReduce with swarms.

-- | Create a new swarm from a map function (@mf@), perform reducing on it
-- message box with reduce function (@rf@).
-- 
mapReduce :: [t] -> (t -> MBox m -> IO a) -> ([m] -> r) -> IO r
mapReduce xs mf rf = do
  sw <- swarm xs mf
  atomically $ rf <$> forM xs (const $ readTChan $ swarmMBox sw)
