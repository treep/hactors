
module Control.Concurrent.Actor.Examples where

import System.IO
import Control.Concurrent.Actor

-- -----------------------------------------------------------------------------
-- * Spawning.

spawn_1 :: IO ()
spawn_1 = do
  hSetBuffering stdout LineBuffering
  child <- spawn $ const $ say $ "  Hi, I'am the child."
  say $ "I was create a child with PID = " ++ show child

{-
> spawn_1
ThreadId 44:   Hi, I'am the child.
ThreadId 43: I was create a child with PID = ThreadId 44
-}

data Child = Child String Process

actor_1 :: IO ()
actor_1 = do
  hSetBuffering stdout LineBuffering
  me <- self
  say $ "Hi, I'am the parrent."
  child <- actor (Child "child" me) $
    \(Child name parent) _ -> do
      say $ "  Hi, I'am the one who was called " ++ name
      say $ "  My parrent's PID = " ++ show parent
  say $ "I was create a child with PID = " ++ show child

{-
> actor_1
ThreadId 46: Hi, I'am the parrent.
ThreadId 47:   Hi, I'am the one who was called child
ThreadId 46: I was create a child with PID = ThreadId 47
ThreadId 47:   My parrent's PID = ThreadId 46
-}

{-
XXX can't send messages to ourselves.

send_1 = self <! "hello"

send_2 = self <!> self <! "hello"
-}

-- -----------------------------------------------------------------------------
-- * Communicate.

dolphin :: String -> IO ()
dolphin "do a flip" = say "How about no?"
dolphin "fish"      = say "So long and thanks for all the fish!"
dolphin _           = say "Heh, we're smarter than you humans."

test_dolphin :: IO ()
test_dolphin = do
  hSetBuffering stdout LineBuffering
  dol <- spawn_receive dolphin
  dol ! "do a flip"
  dol ! "fish"
  dol ! "oh, hello dolphin!"
  return ()

{-
> test_dolphin
ThreadId 58: How about no?
ThreadId 58: So long and thanks for all the fish!
ThreadId 58: Heh, we're smarter than you humans.
-}
