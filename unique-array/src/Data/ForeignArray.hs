{-# LANGUAGE Haskell2010, TemplateHaskell, KindSignatures, MagicHash, RankNTypes, UnboxedTuples #-}

{-# OPTIONS_GHC -Wall #-}

{- | Mutable arrays outside the GC's purview\;
    if you're not very careful you'll quickly leak space
    (or worse)
-}
module Data.ForeignArray
  ( act2#
  , act10#
  , observe2#
  , observe10#
  ) where

import Unique

$(actN# 2)

$(actN# 10)

$(observeN# 2)

$(observeN# 10)