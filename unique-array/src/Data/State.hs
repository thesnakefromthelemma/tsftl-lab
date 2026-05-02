{-# LANGUAGE Haskell2010
  , DataKinds
  , MagicHash
  , PatternSynonyms
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall -Wno-overlapping-patterns -Wno-inaccessible-code #-}

{- | Low-level 'Control.Monad.ST.runST' -}
module Data.State
  ( -- * 'runST#'
    runST#
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( TYPE
  , RuntimeRep
  , State#
  , runRW#
  )


-- * Low-level 'Control.Monad.ST.runST'

{- | Low-level 'Control.Monad.ST.runST' -}
{-# INLINE runST# #-}
runST# :: forall (r :: RuntimeRep) (a :: TYPE r). (forall s. State# s -> a) -> a
runST# = runRW#