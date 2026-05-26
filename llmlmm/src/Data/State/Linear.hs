{-# LANGUAGE Haskell2010
  , DataKinds
  , FlexibleInstances
  , GADTSyntax
  , InstanceSigs
  , LinearTypes
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , PolyKinds
  , RankNTypes
  , RoleAnnotations
  , ScopedTypeVariables
  , TemplateHaskell
  , TupleSections
  , TypeApplications
  , UnboxedTuples
  , UndecidableInstances -- Only because the class quanitfies over a two-term-type!
  , UnliftedNewtypes
#-}

{-| @-Woverlapping-patterns@ and @Winaccessible-code@ are disabled
    as they only fire due to the match on 'UnsafeRefl'.
    @-Worphans@ is disabled so that we can
    generate 'Urlike' instances
    in this module ("Data.State.Linear")
    for (@forall t.@) @LAlloc# t@
    (defined in "Data.State.Linear.Unsafe")\;
    this is safe because 'LAlloc#' is exported
    outside this package solely by this module.
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-inaccessible-code
    -Wno-overlapping-patterns
    -Wno-orphans
#-}

{- | Linear low-level 'Control.Monad.ST.runST' -}
module Data.State.Linear
  ( -- * 'State#'-parametrized linear allocation tokens
    -- ** 'State#'-parametrized linear allocation tokens
    LAlloc#
    -- ** Running 'LAlloc#' and 'State#'
  , runLA#
  , RunST#
      ( runST# )
  , runLAST#
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( RuntimeRep
      ( TupleRep )
  , TYPE
  , State#
  , runRW#
  , Multiplicity
      ( One
      , Many
      )
  )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( mkName
  , pattern ConT
  , pattern AppT
  , pattern VarT
  )

-- ++ (internal)

import Prelude.Linear.TH
  ( deriveUrlike )

import Prelude.Linear
  ( Urlike (..) )

import Data.State.Linear.Unsafe
  ( LAlloc#
      ( LAlloc# )
  )


-- * 'State#'-parametrized linear allocation tokens

-- ** Instantiates 'Urlike' for (@forall t.@) @LAlloc# t@

{- | Instantiates 'Urlike' for (@forall t.@) @LAlloc# t@ -}
$(pure
    [ deriveUrlike
        ( TupleRep [ ] )
        ( AppT
            ( ConT ''LAlloc# )
            ( VarT (mkName "t") ) ) ] -- There's no point in being explicit about this quantification thanks to GHC-71492
  )

-- ** Running 'LAlloc#' and 'State#'

{- | Running 'LAlloc#' -}
{-# INLINE runLA# #-}
runLA# ::
    forall (r :: RuntimeRep) (a :: TYPE r).
    (forall t. LAlloc# t %One-> a) %One-> a
runLA# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ x -> runRW# (\ s -> x (LAlloc# s))

{- | Running 'State#' -}
class RunST# (p :: Multiplicity) where
    {- | Multiplicity polymorphic 'Control.Monad.ST.runST' -}
    runST# :: forall (r :: RuntimeRep) (a :: TYPE r).
        (forall t. State# t %p-> a) %One-> a
instance RunST# One where
    {-# INLINE runST# #-}
    runST# = case unsafeEqualityProof @Many @One of
        UnsafeRefl -> \ x -> runRW# (\ s -> x s)
instance RunST# Many where
    {-# INLINE runST# #-}
    runST# = case unsafeEqualityProof @Many @One of
        UnsafeRefl -> \ x -> runRW# (\ s -> x s)

{- | Running 'LAlloc#' and 'State#' -}
{-# INLINE runLAST# #-}
runLAST# ::
    forall (p :: Multiplicity). RunST# p =>
    forall (r :: RuntimeRep) (a :: TYPE r).
    (forall t s. LAlloc# t %One-> State# s %p-> a) %One-> a
runLAST# = \ y -> runST# (runLA# y)