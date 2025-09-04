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

testConjecture :: Word -> Word -> Word -> [(InTerm, OutTerm)]
testConjecture = undefined