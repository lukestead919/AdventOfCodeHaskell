module Debugging
  ( traceLns,
    traceShowIf,
  )
where

import Debug.Trace (traceShow)
import GHC.IO.Unsafe (unsafePerformIO)

{-# NOINLINE traceLns #-}
traceLns :: (Show a) => a -> b -> b
traceLns ln expr =
  unsafePerformIO $ do
    putStrLn $ show ln
    return expr

traceShowIf :: (Show a) => Bool -> a -> b -> b
traceShowIf predicate showable rest =
  if predicate
    then traceShow showable rest
    else rest
