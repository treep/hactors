
module Control.Concurrent.Actor.Tests where

import Control.Concurrent.Actor hiding ( receive, spawnReceive )
import Control.Concurrent.Actor.Debug

-- -----------------------------------------------------------------------------
-- * @receive@ is non busy

testReceive1 :: IO ()
testReceive1 = do
  act <- spawnReceive $
    \msg -> case msg of
      "ok?" -> putStrLn "ok"
      _     -> putStrLn "nothing"
  act ! "ok?"
  act ! "ok?"
  act ! "what?"
  return ()

-- > testReceive1
-- ThreadId 39: receiving...
-- ok
-- ThreadId 39: receiving...
-- ok
-- ThreadId 39: receiving...
-- nothing
-- ThreadId 39: receiving...

-- Thus, the @receive@ function don't perform busy waiting.

-- -----------------------------------------------------------------------------
-- * @tolerant@ handle exceptions

testTolerant1 :: IO ()
testTolerant1 = do
  act <- spawnReceive $
    \msg -> tolerant $ if msg then putStrLn "ok" else putStrLn $ tail []
  act ! False
  act ! True
  act ! True
  return ()

-- > testTolerant1
-- ThreadId 31: receiving...
-- ThreadId 31: receiving...
-- ok
-- ThreadId 31: receiving...
-- ok
-- ThreadId 31: receiving...
