{-# LANGUAGE Haskell2010
  , ScopedTypeVariables
  , TypeApplications
#-}

{-# OPTIONS_GHC -Wall #-}

module Main
  ( main
  ) where

import OptStop

main :: IO ()
main =
    -- mapM_ print . flip map [1000000..1000004] $ optStratMuAsHash @Double
    -- mapM_ print . take 5 . drop 10000 . toList $ optStratMuAsStream @Double
    print . (*) (exp 1) . optStratXiAsHash @Double $ SecState 100000 99999