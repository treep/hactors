
module Control.Concurrent.Actor.Tests where

import Control.Concurrent.Actor

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TChan

-- -----------------------------------------------------------------------------
-- * Testing functions.

-- | Variant of @receive@ with the test printing.
receive_test :: MBox m -> (m -> IO a) -> IO b
receive_test a f = forever $ do
  putStrLn "receiving..." 
  atomically (readTChan a) >>= f

-- | Variant of @spawn_receive@ with the test printing.
spawn_receive_test :: (m -> IO a) -> IO (Actor m)
spawn_receive_test f = spawn $ \m -> receive_test m f

-- -----------------------------------------------------------------------------
-- * @receive@ is non busy.

test_receive_1 :: IO ()
test_receive_1 = do
  act <- spawn_receive_test $
    \msg -> case msg of
      "ok?" -> putStrLn "ok"
      _     -> putStrLn "nothing"
  act ! "ok?"
  act ! "ok?"
  act ! "what?"
  return ()

-- > test_receive_1
-- receiving...
-- ok
-- receiving...
-- ok
-- receiving...
-- nothing
-- receiving...

-- Thus, the @receive@ function don't perform busy waiting.

-- -----------------------------------------------------------------------------
-- * @tolerant@ handle exceptions.

test_tolerant_1 :: IO ()
test_tolerant_1 = do
  act <- spawn_receive_test $
    \msg -> tolerant $ case msg of
      True  -> putStrLn "ok"
      False -> putStrLn $ tail []
  act ! False
  act ! True
  act ! True
  return ()

-- > test_tolerant_1
-- receiving...
-- receiving...
-- ok
-- receiving...
-- ok
-- receiving...
