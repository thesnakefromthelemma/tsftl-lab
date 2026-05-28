{-# LANGUAGE Haskell2010
  , DataKinds
  , MagicHash
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Low-level 'Control.Monad.ST.runST' -}
module Data.State
  ( -- * 'runST#'
    runST#
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( RuntimeRep
  , TYPE
  , State#
  , runRW#
  )


-- * Low-level 'Control.Monad.ST.runST'

{- | Low-level 'Control.Monad.ST.runST' -}
{-# INLINE runST# #-}
runST# ::
    forall (r :: RuntimeRep) (a :: TYPE r).
    (forall s. State# s -> a) -> a
runST# = runRW#