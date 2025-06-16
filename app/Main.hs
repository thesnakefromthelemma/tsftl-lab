{-# LANGUAGE Haskell2010
  , PatternSynonyms
  , TypeApplications
  #-}

module Main
  ( main
  ) where

import System.Environment
  ( getArgs )

import System.Random
  ( mkStdGen
--, initStdGen
  , uniforms
  )


-- INTERNAL IMPORTS --

import Calculus
  ( extrapolate )


-- MAIN --

main :: IO ()
main = do
    [g, n0, n1] <- fmap (read @Int) <$> getArgs
    let sval = take n0 . uniforms @Int $ mkStdGen g
    print $ extrapolate sval n1
