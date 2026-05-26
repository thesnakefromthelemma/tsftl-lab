{-# LANGUAGE Haskell2010
  , MagicHash
  , PatternSynonyms
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Exposes constructor of 'Data.Addr.Addr#' -}
module Data.Addr.Unsafe
  ( -- * Constructor of 'Data.Addr.Addr#'
    pattern Addr#
  ) where

import Data.Addr.TH
  ( pattern Addr# )