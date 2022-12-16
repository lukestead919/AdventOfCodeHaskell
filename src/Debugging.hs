module Debugging
  ( traceLns,
    traceShowIf,
  )
where

import Debug.Trace (traceShow)
import GHC.IO.Unsafe (unsafePerformIO)

{-# NOINLINE traceLns #-}
traceLns :: String -> a -> a
traceLns string expr =
  unsafePerformIO $ do
    putStrLn string
    return expr

traceShowIf :: (Show a) => Bool -> a -> b -> b
traceShowIf predicate showable rest =
  if predicate
    then traceShow showable rest
    else rest
