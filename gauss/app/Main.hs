{-# LANGUAGE Haskell2010
  , TypeApplications
#-}

{-# OPTIONS_GHC -Wall #-}

module Main
  ( main
  ) where

import Marriage
   ( marry )

import System.Environment
  ( getArgs )


-- * Main

main :: IO ()
main = do
    [n, s, d] <- fmap (read @Word) <$> getArgs
    mapM_ (\ (x, y) -> print $ show x ++ " ~ " ++ show y) $ marry n s d