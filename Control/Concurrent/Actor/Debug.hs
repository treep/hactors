
-- |
-- This module reimplement some functions from the @Control.Concurrent.Actor@
-- module with debug features.
-- 
module Control.Concurrent.Actor.Debug where

import Control.Concurrent.Actor hiding ( receive, spawnReceive )

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TChan

-- | Variant of @receive@ with the test printing.
receive :: MBox m -> (m -> IO a) -> IO b
receive a f = forever $ do
  say "receiving..." 
  atomically (readTChan a) >>= f

-- | Variant of @spawn_receive@ with the test printing.
spawnReceive :: (m -> IO a) -> IO (Actor m)
spawnReceive f = spawn $ \m -> receive m f
