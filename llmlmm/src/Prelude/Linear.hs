{-# LANGUAGE Haskell2010
  , DataKinds
  , FlexibleInstances
  , GADTSyntax
  , InstanceSigs
  , LambdaCase
  , LinearTypes
  , MagicHash
  , PatternSynonyms
  , PolyKinds
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
#-}

{-| @-Worphans@ is disabled so that we can
    generate 'Ur' instances (defined in "Prelude.Linear.Internal")
    in this module ("Prelude.Linear") for types defined in "GHC.Exts"\;
    this is safe because 'Ur' is exported outside this package solely
    by this module.
-}
{-# OPTIONS_GHC -Wall -Wno-partial-type-signatures -Wno-orphans #-}

{- | Miscellaneous linear utilities -}
module Prelude.Linear
  ( -- * Representation-polymorphic interface to strict unrestricted modality
    Urable
      ( Ur
      , ur
      , evUr
      )
    -- * Kind and multiplicity-polymorphic 'Prelude.($)' operator
  , ($)
  , pattern UrInt8#
  , pattern UrInt16#
  , pattern UrInt32#
  , pattern UrInt64#
  , pattern UrInt#
  , pattern UrWord8#
  , pattern UrWord16#
  , pattern UrWord32#
  , pattern UrWord64#
  , pattern UrWord#
  , pattern UrAddr#
  , pattern UrFloat#
  , pattern UrDouble#
  , pattern Ur1#
  , pattern Ur0#
{-, pattern UrInt8X16#
  , pattern UrInt8X32#
  , pattern UrInt8X64#
  , pattern UrInt16X8#
  , pattern UrInt16X16#
  , pattern UrInt16X32#
  , pattern UrInt32X4#
  , pattern UrInt32X8#
  , pattern UrInt32X16#
  , pattern UrInt64X2#
  , pattern UrInt64X4#
  , pattern UrInt64X8#
  , pattern UrWord8X16#
  , pattern UrWord8X32#
  , pattern UrWord8X64#
  , pattern UrWord16X8#
  , pattern UrWord16X16#
  , pattern UrWord16X32#
  , pattern UrWord32X4#
  , pattern UrWord32X8#
  , pattern UrWord32X16#
  , pattern UrWord64X2#
  , pattern UrWord64X4#
  , pattern UrWord64X8#
  , pattern UrFloatX4#
  , pattern UrFloatX8#
  , pattern UrFloatX16#
  , pattern UrDoubleX2#
  , pattern UrDoubleX4#
  , pattern UrDoubleX8#-}
  , pattern Ur#
  , pattern Ur
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import Prelude hiding
  ( ($)
{-, elem-}
  )

import qualified Prelude as NL
  ( ($)
{-, elem-}
  )

import GHC.Exts
  ( TYPE
{-, pattern Vec2
  , pattern Vec4
  , pattern Vec8
  , pattern Vec16
  , pattern Vec32
  , pattern Vec64-}
{-, pattern Int8ElemRep
  , pattern Int16ElemRep
  , pattern Int32ElemRep
  , pattern Int64ElemRep
  , pattern Word8ElemRep
  , pattern Word16ElemRep
  , pattern Word32ElemRep
  , pattern Word64ElemRep
  , pattern FloatElemRep
  , pattern DoubleElemRep-}
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
{-    , VecRep-}
      , BoxedRep )
  , pattern Unlifted
  , pattern Lifted
  , Multiplicity
  )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( mkName
  , pattern PromotedT
  , pattern PromotedNilT
  , pattern AppT
  )

-- ++ (internal)

import Prelude.Linear.Internal
  ( Urable
      ( Ur
      , ur
      , evUr
      )
  , deriveUrable
  )

import Data.RuntimeRep.Extra
  ({-repBytes
  , supportedSIMDBytes
  ,-}repStem
  )


-- * Representation-polymorphic interface to strict unrestricted modality

{- | Instantiates 'Ur' for primitive types -}
$(pure NL.$ do
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
    let r_nm = mkName NL.$ show r -- is there a nicer way to do this?
        cn_nm = mkName NL.$ "Ur" <> repStem r
    pure NL.$
        deriveUrable
          ( PromotedT r_nm )
          ( cn_nm )
  )

{- | Instantiates 'Ur' for SIMD vector types\;
    unfortunately their current GHC support is buggy
    and hence this is commented out for now
    (although it otherwise works)
-} {-
$(pure NL.$ do
    elem <-
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
    count <-
      [ Vec2
      , Vec4
      , Vec8
      , Vec16
      , Vec32
      , Vec64 ]
    let size = repBytes (VecRep count elem)
        elem_nm = mkName NL.$ show elem -- is there a nicer way to do this?
        count_nm = mkName NL.$ show count -- is there a nicer way to do this?
        cn_nm = mkName NL.$ "Ur" <> repStem (VecRep count elem)
    case NL.elem size supportedSIMDBytes of
        True  -> pure NL.$
            deriveUrable
              ( AppT ( AppT
                  ( PromotedT 'VecRep )
                  ( PromotedT count_nm ) )
                  ( PromotedT elem_nm ) )
              ( cn_nm )
        False -> []
  )-}

{- | Instantiates 'Ur' for \(0\)-tuples -}
$(pure
  [ deriveUrable
      ( AppT
          ( PromotedT 'TupleRep )
          ( PromotedNilT ) )
      ( mkName NL.$ "Ur" <> repStem (TupleRep []) ) ]
  )

{- | Instantiates 'Ur' for \(0\)-sums -}
$(pure
  [ deriveUrable
      ( AppT
          ( PromotedT 'SumRep )
          ( PromotedNilT ) )
      ( mkName NL.$ "Ur" <> repStem (SumRep []) ) ]
  )

{- | Instantiates 'Ur' for unlifted boxed values -}
$(pure
  [ deriveUrable
      ( AppT
          ( PromotedT 'BoxedRep )
          ( PromotedT 'Unlifted ) )
      ( mkName NL.$ "Ur" <> repStem (BoxedRep Unlifted) ) ]
  )

{- | Instantiates 'Ur' for lifted boxed values -}
$(pure
  [ deriveUrable
      ( AppT
          ( PromotedT 'BoxedRep )
          ( PromotedT 'Lifted ) )
      ( mkName NL.$ "Ur" <> repStem (BoxedRep Lifted) ) ]
  )


-- * Kind and multiplicity-polymorphic 'Prelude.($)' operator

{- | Kind and multiplicity-polymorphic 'Prelude.($)' operator -}
infixr 0 $
{-# INLINE ($) #-}
($) :: forall
    (p :: Multiplicity) (q :: Multiplicity)
    (ra :: RuntimeRep) (rb :: RuntimeRep)
    (a :: TYPE ra) (b :: TYPE rb).
    (a %q -> b) %p -> a %q -> b
($) = \ f -> f