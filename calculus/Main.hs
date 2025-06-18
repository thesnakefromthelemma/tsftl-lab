{-# LANGUAGE Haskell2010
  , CPP
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

#if LIST
import List
  ( extrapolate )
#elif VECTOR
import Vector
  ( extrapolate )
#elif MVECTOR
import MVector
  ( extrapolate )
#elif LINEAR
import Linear
  ( extrapolate )
#endif


-- MAIN --

main :: IO ()
main = do
    [gStr, n0Str, n1Str] <- getArgs
    let g = read @Int gStr
        n0 = read @Int n0Str
        n1 = read @Int n1Str
        sval = take n0 . uniforms @Int $ mkStdGen g
    print $ extrapolate sval n1
