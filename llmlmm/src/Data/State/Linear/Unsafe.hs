{-# LANGUAGE Haskell2010
  , DataKinds
  , GADTSyntax
  , LinearTypes
  , MagicHash
  , PatternSynonyms
  , PolyKinds
  , RoleAnnotations
  , ScopedTypeVariables
  , UnliftedNewtypes
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Exposes constructor of 'Data.State.Linear.LAlloc#' -}
module Data.State.Linear.Unsafe
  ( -- * 'State#'-parametrized linear allocation tokens
    LAlloc#
      ( LAlloc# )
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( pattern Lifted
  , RuntimeRep
      ( TupleRep
      , BoxedRep
      )
  , TYPE
  , State#
  , pattern One
  )


-- * 'State#'-parametrized linear allocation tokens

-- ** 'State#'-parametrized linear allocation tokens

{- | 'State#'-parametrized linear allocation tokens -}
type role LAlloc# nominal
newtype LAlloc# :: TYPE (BoxedRep Lifted) -> TYPE (TupleRep '[]) where
    LAlloc# ::
        forall (s :: TYPE (BoxedRep Lifted)).
        State# s %One-> LAlloc# s