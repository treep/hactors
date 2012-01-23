
module Control.Concurrent.Actor.Examples where

import System.IO

import Control.Concurrent.Actor

spawn_1 = do
  hSetBuffering stdout LineBuffering
  child <- spawn $ do
    putStrLn $ "  Hi, I'am the child."
    me <- self
    putStrLn $ "  My PID = " ++ show me
  putStrLn $ "I was create a child with PID = " ++ show child

data Child = Child String Process

actor_1 = do
  hSetBuffering stdout LineBuffering
  me <- self
  putStrLn $ "Hi, I'am the parrent, my PID = " ++ show me
  child <- actor (Child "child" me) $
    \(Child name parent) -> do
      putStrLn $ "  Hi, I'am the one who was called " ++ name
      me <- self
      putStrLn $ "  My PID = " ++ show me
      putStrLn $ "  My parrent's PID = " ++ show parent
  putStrLn $ "I was create a child with PID = " ++ show child

send_1 = self ! "hello"

send_2 = self <! self ! "hello"

new_dolphin = receive $
  \message -> case message of
    "do a flip" -> putStrLn "How about no?"
    "fish" -> putStrLn "So long and thanks for all the fish!"
    _ -> putStrLn "Heh, we're smarter than you humans."

test_dolphin = do
  hSetBuffering stdout LineBuffering
  let dolphin = spawn new_dolphin
  dolphin ! "do a flip"
  dolphin ! "fish"
  dolphin ! "oh, hello dolphin!"
  return ()
