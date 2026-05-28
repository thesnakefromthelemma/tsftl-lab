{-# LANGUAGE Haskell2010
  , DataKinds
  , GHCForeignImportPrim
  , MagicHash
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , UnliftedFFITypes
#-}

{-# OPTIONS_GHC -Wall #-}

{- 
Note [Future work]
~~~~~~~~~~~~~~~~~~

  * Verify that 'noPrimOp' is safe (opaque to core) and free (codegens to a no-op)
-}

{- | Low-level 'Control.Monad.ST.runST' -}
module Data.State
  ( -- * 'runST#'
    runST#
    -- * 'State#' token refreshing
  , refresh#
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


-- * 'State#' token refreshing

{- | 'State#' token refreshing -}
foreign import prim "noPrimOp"
    refresh# :: forall s. State# s -> State# s