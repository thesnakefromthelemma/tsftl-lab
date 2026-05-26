{-# LANGUAGE Haskell2010
  , CPP
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
    LAlloc#
    -- * Representation-polymorphic linear allocation token suppression
  , Supp
      ( supp )
    -- * Running 'LAlloc#' and 'State#'
  , runLA#
  , RunST#
      ( runST# )
  , runLAST#
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

#if SIMD
import Prelude hiding
  ( elem )

import qualified Prelude
  ( elem )
#endif

import GHC.Exts
  ( 
#if SIMD
    pattern Vec2
  , pattern Vec4
  , pattern Vec8
  , pattern Vec16
  , pattern Vec32
  , pattern Vec64
  , pattern Int8ElemRep
  , pattern Int16ElemRep
  , pattern Int32ElemRep
  , pattern Int64ElemRep
  , pattern Word8ElemRep
  , pattern Word16ElemRep
  , pattern Word32ElemRep
  , pattern Word64ElemRep
  , pattern FloatElemRep
  , pattern DoubleElemRep
  ,
#endif
    pattern Unlifted
  , pattern Lifted
  , RuntimeRep
      ( Int8Rep
      , Int16Rep
      , Int32Rep
      , Int64Rep
      , IntRep
      , Word8Rep
      , Word16Rep
      , Word32Rep
      , Word64Rep
      , WordRep
      , AddrRep
      , FloatRep
      , DoubleRep
      , TupleRep
      , SumRep
#if SIMD
      , VecRep
#endif
      , BoxedRep
      )
  , TYPE
  , State#
  , runRW#
  , Multiplicity
      ( One
      , Many
      )
#if SIMD
  , pattern I#
#endif
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

import Data.RuntimeRep
  ( pattern Prim
  , pattern Lim
  , pattern Vec
  , pattern Box
#if SIMD
  , repBytes
  , supportedSIMDBytes
#endif
  )

import Prelude.Linear.TH
  ( deriveUrlike )

import Prelude.Linear
  ( Urlike (..) )

import Data.State.Linear.TH
  ( LAlloc#
      ( LAlloc# )
  , Supp
      ( supp )
  , deriveSupp
  )


-- * 'State#'-parametrized linear allocation tokens

-- ** 'Urlike' instance for (@forall t.@) @LAlloc# t@

{- | Instantiates 'Urlike' for (@forall t.@) @LAlloc# t@ -}
$(pure
    [ deriveUrlike
        ( TupleRep [ ] )
        ( AppT
            ( ConT ''LAlloc# )
            ( VarT (mkName "t") ) ) ] -- There's no point in being explicit about this quantification thanks to GHC-71492
  )


-- * Representation-polymorphic linear allocation token suppression

-- ** TemplateHaskell generation of linear allocation token suppression instances

{- | Instantiates 'Supp' for various 'RuntimeRep's -}
$(pure $ do
    g <-
      [ Prim
      , Lim
      , Vec
      , Box ]
    case g of
        Prim -> do
            r <-
              [ Int8Rep
              , Int16Rep
              , Int32Rep
              , Int64Rep
              , IntRep
              , Word8Rep
              , Word16Rep
              , Word32Rep
              , Word64Rep
              , WordRep
              , AddrRep
              , FloatRep
              , DoubleRep ]
            [ deriveSupp r ]
        Lim  -> do
            v <-
              [ TupleRep
              , SumRep ]
            let r = v []
            [ deriveSupp r ]
#if SIMD
        Vec  -> do
            e <-
              [ Int8ElemRep
              , Int16ElemRep
              , Int32ElemRep
              , Int64ElemRep
              , Word8ElemRep
              , Word16ElemRep
              , Word32ElemRep
              , Word64ElemRep
              , FloatElemRep
              , DoubleElemRep ]         
            c <-
              [ Vec2
              , Vec4
              , Vec8
              , Vec16
              , Vec32
              , Vec64 ]
            let r = VecRep c e
            case Prelude.elem (I# (repBytes r)) supportedSIMDBytes of
                True  -> [ deriveSupp r ]
                False -> [ ]
#else
        Vec  -> [ ]
#endif
        Box  -> do
            l <-
              [ Unlifted
              , Lifted ]
            let r = BoxedRep l
            [ deriveSupp r ]
  )


-- * Running 'LAlloc#' and 'State#'

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