{-# LANGUAGE Haskell2010
  , CPP
  , TypeApplications
#-}

module Main
  ( main
  ) where

#if LIST || LINEAR
#elif BOXED && EAGER
import Data.Vector.Strict
  ( Vector )
#elif BOXED
import Data.Vector
  ( Vector )
#elif EAGER
import Data.Vector.Unboxed
  ( Vector )
#endif

import System.Random
  ( mkStdGen
--, initStdGen
  , uniforms
  )

import System.Environment
  ( getArgs )


-- /Internal imports/

#if LIST
import Calculus.List
  ( extrapolate )
#elif VECTOR
import Calculus.Vector
  ( extrapolate )
#elif MVECTOR
import Calculus.MVector
  ( extrapolate )
#elif LINEAR
import Calculus.Linear
  ( extrapolate )
#endif


-- /Main/

main :: IO ()
main = do
    [gStr, n0Str, n1Str] <- getArgs
    let g = read @Int gStr
        n0 = read @Int n0Str
        n1 = read @Integer n1Str
        sval = take n0 . uniforms @Int $ mkStdGen g
#if LIST || LINEAR
    print $ extrapolate sval n1
#elif VECTOR || MVECTOR
    print $ (extrapolate Vector) sval n1
#endif