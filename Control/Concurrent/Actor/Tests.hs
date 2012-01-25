
module Control.Concurrent.Actor.Tests where

import Control.Concurrent.Actor hiding ( receive, spawn_receive )
import Control.Concurrent.Actor.Debug

-- -----------------------------------------------------------------------------
-- * @receive@ is non busy.

test_receive_1 :: IO ()
test_receive_1 = do
  act <- spawn_receive $
    \msg -> case msg of
      "ok?" -> putStrLn "ok"
      _     -> putStrLn "nothing"
  act ! "ok?"
  act ! "ok?"
  act ! "what?"
  return ()

{-
> test_receive_1
ThreadId 39: receiving...
ok
ThreadId 39: receiving...
ok
ThreadId 39: receiving...
nothing
ThreadId 39: receiving...
-}

-- Thus, the @receive@ function don't perform busy waiting.

-- -----------------------------------------------------------------------------
-- * @tolerant@ handle exceptions.

test_tolerant_1 :: IO ()
test_tolerant_1 = do
  act <- spawn_receive $
    \msg -> tolerant $ case msg of
      True  -> putStrLn "ok"
      False -> putStrLn $ tail []
  act ! False
  act ! True
  act ! True
  return ()

{-
> test_tolerant_1
ThreadId 31: receiving...
ThreadId 31: receiving...
ok
ThreadId 31: receiving...
ok
ThreadId 31: receiving...
-}
