{-# LANGUAGE Haskell2010
  , DataKinds
  , GADTSyntax
  , LinearTypes
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , PolyKinds
  , ScopedTypeVariables
  , StandaloneKindSignatures
  , TemplateHaskellQuotes
  , UnboxedTuples
  , UnliftedNewtypes
#-}

{-# OPTIONS_GHC -Wall #-}

{- | '@'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses,
    representation-polymorphic interface to writing/reading off them,
    and TemplateHaskell generation of instances thereof
-}
module Data.Addr.Linear.TH
  ( -- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses
    Addr#
      ( Addr# )
    -- * Representation-polymorphic interface to writing/reading off 'Addr#'s
  , Addrable#
      ( writeAddr#
      , readAddr#
      )
  , declareAddrableEg
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( pattern Lifted
  , RuntimeRep
      ( AddrRep
      , TupleRep
      , BoxedRep
      )
  , TYPE
  , Int#
  , Constraint
  , pattern One
  , pattern Many
  )

import qualified GHC.Exts as GHC
  ( Addr# )

import Data.Coerce
  ( coerce )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( newName
  , pattern UnboxedTupE
  , pattern AppE
  , pattern Match
  , pattern CaseE
  , pattern VarE
  , pattern LamE
  , pattern AppTypeE
  , pattern PromotedT
  , pattern UnboxedTupleT
  , pattern MulArrowT
  , pattern ConT
  , pattern AppT
  , pattern VarT
  , pattern PlainTV
  , pattern SpecifiedSpec
  , pattern ForallT
  , pattern UnboxedTupP
  , pattern ConP
  , pattern VarP
  , Dec
  , pattern NormalB
  , pattern ValD
  , pattern SigD
  , pattern InstanceD
  , pattern Inline
  , pattern ConLike
  , pattern AllPhases
  , pattern InlineP
  , pattern PragmaD
  , Q
  , pattern InstanceSigs
  , pattern LinearTypes
  , pattern MagicHash
  , pattern ScopedTypeVariables
  , pattern TypeApplications
  )

-- ++ (internal)

import Misc.TH
  ( guardExts
  , guardValue
  )

import Data.RuntimeRep
  ( repStem
  , repEg
  )

import Prelude.Linear
  ( Ur
  , ur
  )

import Data.State.Linear
  ( Alloc# )


-- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses

{- | @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses -}
newtype
    Addr# :: TYPE (BoxedRep Lifted) -> TYPE (TupleRep [TupleRep '[], AddrRep])
  where
    Addr# ::
        forall (t :: TYPE (BoxedRep Lifted)).
        (# Alloc# t, GHC.Addr# #) %One-> Addr# t


-- * Representation-polymorphic interface to writing/reading off 'Addr#'s

-- ** Representation-polymorphic interface to writing/reading off 'Addr#'s

{- | Representation-polymorphic interface to writing/reading off 'Addr#'s -}
type Addrable# ::
    forall {r :: RuntimeRep}. TYPE r -> Constraint
class Addrable# a where
    {- | Given arguments @p@, @i@, @x@,
        writes @x@ to @p + repBytes r * i bytes@
        and returns @p@
    -}
    writeAddr# :: forall t. Addr# t %One-> Int# %One-> a %One-> Addr# t
    {- | Given arguments @p@, @i@,
        reads @x@ from @p + repBytes r * i bytes@
        and returns @p@, @x@
    -}
    readAddr# :: forall t. Addr# t %One-> Int# %One-> (# Addr# t, Ur a #)

-- ** TemplateHaskell generation of standard 'Addrable#' instances

{- | Given argument @r@, representing a promoted term of type 'RuntimeRep',
    generates an 'Addrable#' instance
    for the standard representation instance @EG_TY@ of @r@\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_Addrable#(r)                                           \
        instance Addrable# (EG_TY) where                                      \
            {-# INLINE CONLIKE writeAddr# #-}                                \
          ; writeAddr# ::                                                    \
                forall t. Addr# t %One-> Int# %One-> a %One-> Addr# t        \
          ; writeAddr# = case unsafeEqualityProof @Many @One of              \
                UnsafeRefl -> coerce $ \ (# s, a #) n x ->                   \
                    case GHC.Exts,writeEG_TYOffAddr# a n x s of              \
                        s' -> (# s', a #)                                    \
          ; {-# INLINE CONLIKE readAddr# #-}                                 \
          ; readAddr# ::                                                     \
                forall t. Addr# t %One-> Int# %One-> (# Addr# t, Ur EG_TY #) \
          ; readAddr# = case unsafeEqualityProof @Many @One of               \
                UnsafeRefl -> coerce $ \ (# s, a #) n ->                     \
                    case GHC.Exts.readEG_TYOffAddr# a n s of                 \
                        (# s', x #) -> (# s', ur x #)
    @
    Requires @-XInstanceSigs -XLinearTypes -XScopedTypeVariables -XTypeApplications@.
    Requires that "GHC.Exts.writeEG_TYOffAddr#" and "GHC.Exts.readEG_TYOffAddr# " be in scope.
    Requires that the constructors 'Data.State.Linear.TH.Alloc#' and 'Data.Addr.Linear.Addr#' be in scope,
    but this is not checked.
-}
declareAddrableEg :: RuntimeRep -> Q Dec
declareAddrableEg = \ r -> do
    guardExts
      ( "\'Data.Addr.Linear.deriveAddrable#\'")
      [ InstanceSigs
      , LinearTypes
      , MagicHash
      , ScopedTypeVariables
      , TypeApplications ]
    let eg_ty = repEg r
    wr_nm <- guardValue
      ( "\'Data.Addr.Linear.deriveAddrable#\'" )
      ( "GHC.Exts.write" <> repStem r <> "OffAddr#" )
    rd_nm <- guardValue
      ( "\'Data.Addr.Linear.deriveAddrable#\'" )
      ( "GHC.Exts.read" <> repStem r <> "OffAddr#" )
    t_nm <- newName "t" -- not great to be recycling these...
    a_nm <- newName "a"
    n_nm <- newName "n"
    x_nm <- newName "x"
    s_nm <- newName "s"
    s'_nm <- newName "s'"
    pure
      ( InstanceD
          ( Nothing )
          [ ]
          ( AppT
              ( ConT ''Addrable# )
              ( eg_ty ) )
          [ ValD
              ( VarP 'writeAddr# )
              ( NormalB ( CaseE
                  ( AppTypeE ( AppTypeE
                      ( VarE 'unsafeEqualityProof )
                      ( PromotedT 'Many ) )
                      ( PromotedT 'One ) )
                  [ Match
                      ( ConP
                          ( 'UnsafeRefl )
                          [ ]
                          [ ] )
                      ( NormalB ( AppE
                          ( VarE 'coerce )
                          ( LamE
                              [ UnboxedTupP
                                  [ VarP s_nm
                                  , VarP a_nm ]
                              , VarP n_nm
                              , VarP x_nm ]
                              ( CaseE
                                  ( AppE ( AppE ( AppE ( AppE
                                      ( VarE wr_nm )
                                      ( VarE a_nm ) )
                                      ( VarE n_nm ) )
                                      ( VarE x_nm ) )
                                      ( VarE s_nm ) )
                                  [ Match
                                      ( VarP s'_nm )
                                      ( NormalB ( UnboxedTupE
                                          [ Just ( VarE s'_nm )
                                          , Just ( VarE a_nm ) ] ) )
                                      [ ] ] ) ) ) )
                      [ ] ] ) )
              [ ]
          , SigD
              ( 'writeAddr# )
              ( ForallT
                  [ PlainTV
                      ( t_nm )
                      ( SpecifiedSpec ) ]
                  [ ]
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'One ) )
                      ( AppT
                          ( ConT ''Addr# )
                          ( VarT t_nm ) ) )
                      ( AppT ( AppT ( AppT
                          ( MulArrowT )
                          ( PromotedT 'One ) )
                          ( ConT ''Int# ) )
                          ( AppT ( AppT ( AppT
                              ( MulArrowT )
                              ( PromotedT 'One ) )
                              ( eg_ty ) )
                              ( AppT
                                  ( ConT ''Addr# )
                                  ( VarT t_nm ) ) ) ) ) )
          , PragmaD ( InlineP
              ( 'writeAddr# )
              ( Inline )
              ( ConLike )
              ( AllPhases ) )
          , ValD
              ( VarP 'readAddr# )
              ( NormalB ( CaseE
                  ( AppTypeE ( AppTypeE
                      ( VarE 'unsafeEqualityProof )
                      ( PromotedT 'Many ) )
                      ( PromotedT 'One ) )
                  [ Match
                      ( ConP
                          ( 'UnsafeRefl )
                          [ ]
                          [ ] )
                      ( NormalB ( AppE
                          ( VarE 'coerce )
                          ( LamE
                              [ UnboxedTupP
                                  [ VarP s_nm
                                  , VarP a_nm ]
                              , VarP n_nm ]
                              ( CaseE
                                  ( AppE ( AppE ( AppE
                                      ( VarE rd_nm )
                                      ( VarE a_nm ) )
                                      ( VarE n_nm ) )
                                      ( VarE s_nm ) )
                                  [ Match
                                      ( UnboxedTupP
                                          [ VarP s'_nm
                                          , VarP x_nm ] )
                                      ( NormalB ( UnboxedTupE
                                          [ Just ( UnboxedTupE
                                              [ Just ( VarE s'_nm )
                                              , Just ( VarE a_nm ) ] )
                                          , Just ( AppE
                                              ( VarE 'ur )
                                              ( VarE x_nm ) ) ] ) )
                                      [ ] ] ) ) ) )
                      [ ] ] ) )
              [ ]
          , SigD
              ( 'readAddr# )
              ( ForallT
                  [ PlainTV
                      ( t_nm )
                      ( SpecifiedSpec ) ]
                  [ ]
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'One ) )
                      ( AppT
                          ( ConT ''Addr# )
                          ( VarT t_nm ) ) )
                      ( AppT ( AppT ( AppT
                          ( MulArrowT )
                          ( PromotedT 'One ) )
                          ( ConT ''Int# ) )
                          ( AppT ( AppT
                              ( UnboxedTupleT 2 )
                              ( AppT
                                  ( ConT ''Addr# )
                                  ( VarT t_nm ) ) )
                              ( AppT
                                  ( ConT ''Ur )
                                  ( eg_ty ) ) ) ) ) )
          , PragmaD ( InlineP
              ( 'readAddr# )
              ( Inline )
              ( ConLike )
              ( AllPhases ) ) ] )