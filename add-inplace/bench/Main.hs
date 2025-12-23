{-# LANGUAGE Haskell2010
  , LambdaCase
  , MagicHash
  , ScopedTypeVariables
  , TypeApplications
#-}

{-# OPTIONS_GHC
    -Wall
    -O2
    -fexpose-all-unfoldings
    -fspecialize-aggressively
    -fllvm
#-}


-- + Imports

import Data.Foldable
  ( foldlM )

import GHC.Num.BigNat
  ( BigNat
      ( BN# )
  , bigNatFromWord
  , bigNatAdd
  )

import Prototype
  ( fromWord
  , add
  , run
  )

import GHC.Num.Natural
  ( Natural
  , naturalFromBigNat#
  )

import System.Environment
  ( getArgs )


-- + Benchmark

{-# INLINE naturalFromBigNat #-}
naturalFromBigNat :: BigNat -> Natural
naturalFromBigNat = \ (BN# x) ->
     naturalFromBigNat# x

main :: IO ()
main =
    let -- | Summing BigNaturals using 'GHC.Num.BigNat.BigNat' (i.e., not in place, essentially FFIed from GMP)
        main0 :: Int -> Int -> BigNat
        main0 = \ n a ->
            foldl' (\ (BN# x) _ -> BN# $ bigNatAdd x x) (BN# $ bigNatFromWord $ fromIntegral a) [0..n]

        -- | Summing big naturals in place using a dynamically resized 'Data.Primitive.PrimArray.MutablePrimArray'
        main1 :: Int -> Int -> BigNat
        main1 = \ n a -> run $ do
            x0 <- fromWord $ fromIntegral a
            foldlM (\ x _ -> add x x) x0 [0..n]

    in  -- | Benchmark
        fmap (read @Int) <$> getArgs >>= \case
            [n, a, 0] -> print . naturalFromBigNat $ main0 n a
            [n, a, 1] -> print . naturalFromBigNat $ main1 n a
            _      -> error "Bad command line flags!"