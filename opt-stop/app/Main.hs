{-# LANGUAGE Haskell2010
  , LambdaCase
  , ScopedTypeVariables
  , TypeApplications
#-}

{-# OPTIONS_GHC -Wall #-}

module Main
  ( main
  ) where

import OptStop

import System.Environment
  ( getArgs )

import Data.Foldable

main :: IO ()
main =
    mapM_ print . flip map [10000000..10000004] $ optStratMuAsHash @Double
    -- mapM_ print . take 5 . drop 10000 . toList $ optStratMuAsStream @Double