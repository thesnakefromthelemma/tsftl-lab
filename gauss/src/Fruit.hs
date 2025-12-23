{-# LANGUAGE Haskell2010 #-}

{-# OPTIONS_GHC -Wall #-}

module Fruit
  ( testConjecture
  ) where


-- * Imports

-- ** (internal)

import Polynomy
  ( InTerm
  , OutTerm
  )


-- * Fruit

testConjecture :: Int -> Int -> Int -> [(InTerm, OutTerm)]
testConjecture = undefined