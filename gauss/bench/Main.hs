{-# LANGUAGE Haskell2010
  , GADTSyntax
  , KindSignatures
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC
    -Wall
    -fspecialize-aggressively
    -ddump-to-file
    -ddump-simpl
    -dno-typeable-binds
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-module-prefixes
    #-}


module Main
  ( main
  ) where

import GHC.Exts
  ( pattern I# )

import Match
  ( pattern Zero
  , pattern One
  , pattern Many
  , pattern CountMat
  , match
  )

import Data.Stalk
  ( harvest )

main :: IO ()
main =
    let {-# NOINLINE p# #-}
        p# = 30001#
        p = I# p#
        a = 2
        b = 3
        {-# NOINLINE f #-}
        f = \ x y -> case compare (a * x `rem` p) (b * y `rem` p) of
            LT -> Zero
            EQ -> One
            GT -> Many
    in  print . harvest . match $ CountMat p f

{-
newtype Ref :: Type -> Type where
    Ref :: forall s. STRef s Int -> Ref s

{-# INLINE myList #-}
myList = filter ((0 ==) . (`rem` 10000000)) $ unFoldList
  ( \ rn@(Ref un) -> readSTRef un >>= \case
          -1 -> pure Nothing
          n  -> do
              writeSTRef un $ n - 1
              pure $ Just (n, rn) )
  ( Ref <$> newSTRef 50000000 )

main :: IO ()
main = print myList

{-
main :: IO ()
main =
    let mainR = \ sn ->
            getLine >>= \case
                "" -> case sn of
                    n : sn' -> do
                        print n
                        mainR sn'
                    []      -> putStrLn "All out!"
                _  -> print "Bye!"
    in  do putStrLn "Hi!"
           mainR myList
-}

main :: IO ()
main = putStrLn . yield $ unFoldStalk
  ( \ rn@(Ref un) -> readSTRef un >>= \case
          0 -> pure $ Left "Arrived!"
          n  -> do
              let n' =  n - 1
              writeSTRef un $ n'
              pure $ Right (n', rn) )
  ( Ref <$> newSTRef 1000000000 )
-}