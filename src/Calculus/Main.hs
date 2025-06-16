{-# LANGUAGE Haskell2010
  , PatternSynonyms
  , TypeApplications
  #-}

module Calculus.Main
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

import qualified Calculus.List as List
  ( extrapolate )

import qualified Calculus.Vector as Vector
  ( extrapolate )

import qualified Calculus.MVector as MVector
  ( extrapolate )

import qualified Calculus.Linear as Linear
  ( extrapolate )


-- MAIN --

main :: IO ()
main = do
    [cStr, gStr, n0Str, n1Str] <- getArgs
    let g = read @Int gStr
        n0 = read @Int n0Str
        n1 = read @Int n1Str
        sval = take n0 . uniforms @Int $ mkStdGen g
    case cStr of
        "List" -> print $ List.extrapolate sval n1
        "Vector" -> print $ Vector.extrapolate sval n1
        "MVector" -> print $ MVector.extrapolate sval n1
        "Linear" -> print $ Linear.extrapolate sval n1
        _        -> fail "Bad container flag!"
