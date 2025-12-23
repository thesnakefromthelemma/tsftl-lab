{-# LANGUAGE Haskell2010
  , GADTSyntax
  , KindSignatures
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , TypeApplications
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

module Main
  ( main
  ) where

import GHC.Exts
  ( Int#
  , pattern I#
  , (*#)
  , remInt#
  , (<#)
  , (==#)
  )

import System.Environment
  ( getArgs )

import Match
  ( Count
      ( Zero
      , One
      , Many
      )
  , CountMat
      ( CountMat )
  , match
  )

import Data.Stalk
  ( harvest )

{-# INLINE myCountMat# #-}
myCountMat# :: Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Count
myCountMat# = \ p0 p1 a0 a1 x0 x1 ->
        let y0 = (a0 *# x0) `remInt#` p0
            y1 = (a1 *# x1) `remInt#` p1
        in  case (# y0 <# y1, y0 ==# y1 #) of
                (# 0#, 0# #) -> Many
                (# 0#, _  #) -> One
                _            -> Zero


{-# INLINE myCountMat #-}
myCountMat :: Int -> Int -> Int -> Int -> CountMat
myCountMat = \ (I# p0) (I# p1) (I# a0) (I# a1) -> CountMat (I# p0) (I# p1) $
    \ (I# x0) (I# x1) -> myCountMat# p0 p1 a0 a1 x0 x1

main :: IO ()
main = do
    [p0, p1, a0, a1] <- fmap (read @Int) <$> getArgs 
    print . harvest . match $ myCountMat p0 p1 a0 a1