{-# LANGUAGE Haskell2010
  , DataKinds
  , LinearTypes
  , MagicHash
  , PatternSynonyms
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
#-}

{-| @-Woverlapping-patterns@ and @Winaccessible-code@ are disabled
    as they only fire due to the match on 'UnsafeRefl'
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-overlapping-patterns
    -Wno-inaccessible-code
#-}

{- | Linear low-level 'Control.Monad.ST.runST' -}
module Data.State.Linear
  ( -- * Linear low-level 'Control.Monad.ST.runST'
    runST#
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( RuntimeRep
  , TYPE
  , State#
  , runRW#
  , pattern One
  )


-- * Linear low-level 'Control.Monad.ST.runST'

{- | Linear low-level 'Control.Monad.ST.runST'\;
    note that in this paradigm the 'State#' values
    do not represent the \"state of the real world\",
    but instead tokens affording resource allocation
    (after consumption as which they cease to be passed around).
-}
{-# INLINE runST# #-}
runST# ::
    forall (r :: RuntimeRep) (a :: TYPE r).
    (forall s. State# s %One-> a) -> a
runST# = \ x -> runRW# (\ s -> x s)