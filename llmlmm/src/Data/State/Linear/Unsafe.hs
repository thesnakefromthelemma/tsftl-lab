{-# LANGUAGE Haskell2010
  , MagicHash
  , PatternSynonyms
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Exposes instance constructors of 'Data.State.Linear.State#' -}
module Data.State.Linear.Unsafe
  ( -- * Instance constructors of 'Data.State.Linear.State#'
    pattern StateGC#
  , pattern StateL#
  ) where

import Data.State.Linear.TH
  ( pattern StateGC#
  , pattern StateL#
  )