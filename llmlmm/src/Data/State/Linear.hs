{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , FlexibleInstances
  , GADTSyntax
  , GHCForeignImportPrim
  , InstanceSigs
  , LinearTypes
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  , UnboxedTuples
  , UnliftedFFITypes
#-}

{-| @-Woverlapping-patterns@ and @Winaccessible-code@ are disabled
    as they only fire due to the match on 'UnsafeRefl'.
    Worphans@ is disabled so that we can
    generate 'Supp' instances
    (defined in "Prelude.Linear.TH")
    in this module ("Data.State.Linear")
    for 'Alloc#'
    (defined in "Data.State.Linear.TH")\;
    this is safe because 'Alloc#'
    is exported outside this package solely by this module.
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-inaccessible-code
    -Wno-overlapping-patterns
    -Wno-orphans
#-}

{- 
Note [Future work]
~~~~~~~~~~~~~~~~~~

  * GHC: More SIMD support (cf. Issue #25030)

  * @#if SIMD ... #endif@ removed
-}

{- | Low-level linear 'Control.Monad.ST.runST' -}
module Data.State.Linear
  ( -- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear allocation tokens
    Liberty
      ( Free
      , Bound
      )
  , Alloc#
    -- * Running 'Alloc#' and 'State#'
  , runLA#
  , RunST#
      ( runST# )
    -- * 'Alloc#' token forking
    -- ???
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

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
      , VecRep
      , BoxedRep )
  , TYPE
  , State#
  , runRW#
  , Multiplicity
      ( One
      , Many
      )
  )

import Data.Coerce
  ( coerce )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

import Control.Monad
  ( join )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( newName
  , pattern PromotedT
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
  , supportedSIMDType
#endif
  )

import Prelude.Linear
  ( Supp
      ( supp )
  , deriveSupp
  )

import Data.State.Linear.TH
  ( Liberty
      ( Free
      , Bound )
  , Alloc#
      ( Alloc# )
  )


-- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear allocation tokens

{- | Declares 'Supp' via 'Urlike' -}
$(sequence $ do
    let sr = do
            g <-
              [ Prim
              , Lim
              , Vec
              , Box ]
            r <- case g of
                Prim -> do
                    a <-
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
                    [ a ]
                Lim  -> do
                    x <-
                      [ TupleRep
                      , SumRep ]
                    [ x [ ] ]
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
                    case supportedSIMDType e c of
                        True  -> [ VecRep c e ]
                        False -> [ ]
#else
                Vec  -> [ ]
#endif
                Box  -> do
                    l <-
                      [ Unlifted
                      , Lifted ]
                    [ BoxedRep l ]
            [ r ]
    s <- sr
    pure $ do
        t_nm <- newName "t"
        deriveSupp
          ( AppT ( AppT ( ConT ''Alloc# ) ( PromotedT 'Free ) ) ( VarT t_nm ) )
          ( s )
  )


-- * Running 'Alloc#' and 'State#'

{- | Running 'Alloc#' -}
{-# INLINE runLA# #-}
runLA# ::
    forall (r :: RuntimeRep) (a :: TYPE r).
    (forall t. Alloc# Free t %One-> a) %One-> a
runLA# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce runRW#

{- | Representation-polymorphic, multiplicity-polymorphic 'Control.Monad.ST.runST' -}
class RunST# (p :: Multiplicity) where
    {- | Multiplicity-polymorphic, representation-polymorphic 'Control.Monad.ST.runST' -}
    runST# :: forall (r :: RuntimeRep) (a :: TYPE r).
        (forall t. State# t %p-> a) %One-> a
instance RunST# One where
    {-# INLINE runST# #-}
    runST# = case unsafeEqualityProof @Many @One of
        UnsafeRefl -> coerce runRW#
instance RunST# Many where
    {-# INLINE runST# #-}
    runST# = case unsafeEqualityProof @Many @One of
        UnsafeRefl -> coerce runRW#


-- * 'Alloc#' token forking

{- | TemplateHaskell generation of 'Alloc#' token forking -}

--- ???