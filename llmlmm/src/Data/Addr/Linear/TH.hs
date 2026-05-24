{-# LANGUAGE Haskell2010
  , DataKinds
  , GADTSyntax
  , LinearTypes
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , PolyKinds
  , RoleAnnotations
  , ScopedTypeVariables
  , TemplateHaskellQuotes
  , UnboxedTuples
  , UnliftedNewtypes
#-}

{-# OPTIONS_GHC -Wall #-}

{- | 'State#'-parametrized machine addresses,
    representation-polymorphic interface to writing/reading off them,
    and TemplateHaskell generation of instances thereof
-}
module Data.Addr.Linear.TH
  ( -- * 'State#'-parametrized machine addresses
    Addr#
      ( Addr# )
    -- * Writing/reading off 'Addr#'s
    -- ** Representation-polymorphic interface to writing/reading off 'Addr#'s
  , Addrable
      ( writeAddr#
      , readAddr#
      )
    -- ** TemplateHaskell generation of standard 'Addrable' instances
  , deriveAddrable
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( TYPE
  , RuntimeRep
      ( AddrRep
      , BoxedRep
      )
  , pattern Lifted
  , pattern One
  , pattern Many
  , Int#
  , realWorld# -- BIG gamble
  )

import qualified GHC.Exts as GHC
  ( Addr# )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( mkName
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
  , pattern KindedTV
  , pattern SpecifiedSpec
  , pattern ForallT
  , pattern WildP
  , pattern UnboxedTupP
  , pattern ConP
  , pattern VarP
  , pattern AsP
  , pattern NormalB
  , Dec
  , pattern ValD
  , pattern SigD
  , pattern InstanceD
  , pattern Inline
  , pattern ConLike
  , pattern AllPhases
  , pattern InlineP
  , pattern PragmaD
  )

-- ++ (internal)

import Data.RuntimeRep
  ( repType
  , repStem
  , repEg
  )

import Prelude.Linear
  ( Ur
  , ur
  )


-- * 'State#'-parametrized machine addresses

{- | 'State#'-parametrized machine addresses -}
type role Addr# nominal
newtype Addr# :: TYPE (BoxedRep Lifted) -> TYPE AddrRep where
    Addr# ::
        forall (s :: TYPE (BoxedRep Lifted)).
        GHC.Addr# %One-> Addr# s


-- * Writing/reading off 'Addr#'s

-- ** Representation-polymorphic interface to writing/reading off 'Addr#'s

{- | Representation-polymorphic interface to writing/reading off 'Addr#'s -}
class Addrable (r :: RuntimeRep) (a :: TYPE r) where
    {- | Given arguments @p@, @n@, @x@,
        writes @x@ to @p@ at an offset of @n * repBytes(a)@ bytes,
        returning @x@
    -}
    writeAddr# :: forall s. Addr# s %One-> Int# %One-> a %One-> Addr# s
    {- | Given arguments @p@, @n@,
        returns the 'State#' action
        reading from @p@ at an offset of  @n * repBytes(a)@ bytes
    -}
    readAddr# :: forall s. Addr# s %One-> Int# %One-> (# Addr# s, Ur a #)

-- ** TemplateHaskell generation of standard 'Addrable' instances

{- | Given argument @r@, representing a promoted term of type 'RuntimeRep',
    generates a 'Addrable' instance
    for the standard representation instance of @r@\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_Addrable(r_ty, EG_TY)                                                \
        instance Addrable (r_ty) (EG_TY) where                                              \
            {-# INLINE CONLIKE writeAddr# #-}                                               \
          ; writeAddr# :: forall s. Addr# s %One-> Int# %One-> a %One-> Addr# s             \
          ; writeAddr# = case unsafeEqualityProof @Many @One of                             \
                UnsafeRefl -> \ p@(Addr# a) n x ->                                          \
                    case GHC.writeEG_TYOffAddr# a n x realWorld# of                         \
                        _ -> p                                                              \
          ; {-# INLINE CONLIKE readAddr# #-}                                                \
          ; readAddr# :: forall s. forall s. Addr# s %One-> Int# %One-> (# Addr# s, Ur a #) \
          ; readAddr# = case unsafeEqualityProof @Many @One of                              \
                UnsafeRefl -> \ p@(Addr# a) n ->                                            \
                    case GHC.readEG_TYOffAddr# a n realWorld# of                            \
                        (# _, x #) -> (# p, ur x #)
    @
    Requires at least @-XDataKinds -XInstanceSigs -XKindSignatures -XLinearTypes -XMultiParamTypeClasses -XScopedTypeVariables -XTemplateHaskell -XTypeApplications@,
    but this is not checked.

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs write/read effects by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@,
    hence that the case expression scrutinizing their result
    must be forced when consuming its result\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
deriveAddrable :: RuntimeRep -> Dec
deriveAddrable = \ r ->
    let r_ty = repType r
        eg_ty = repEg r
        wr_nm = mkName $ "GHC.write" <> repStem r <> "OffAddr#"
        rd_nm = mkName $ "GHC.read" <> repStem r <> "OffAddr#"
        s_nm = mkName "s"
        p_nm = mkName "p"
        a_nm = mkName "a"
        n_nm = mkName "n"
        x_nm = mkName "x"
    in  InstanceD
          ( Nothing )
          [ ]
          ( AppT ( AppT
              ( ConT ''Addrable )
              ( r_ty ) )
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
                      ( NormalB ( LamE
                          [ AsP
                              ( p_nm )
                              ( ConP
                                  ( 'Addr# )
                                  [ ]
                                  [ VarP a_nm ] )
                          , VarP n_nm
                          , VarP x_nm ]
                          ( CaseE
                              ( AppE ( AppE ( AppE ( AppE
                                  ( VarE wr_nm )
                                  ( VarE a_nm ) )
                                  ( VarE n_nm ) )
                                  ( VarE x_nm ) )
                                  ( VarE 'realWorld# ) )
                              [ Match
                                  ( WildP )
                                  ( NormalB ( VarE p_nm ) )
                                  [ ] ] ) ) )
                      [ ] ] ) )
              [ ]
          , SigD
              ( 'writeAddr# )
              ( ForallT
                  [ KindedTV
                      ( s_nm )
                      ( SpecifiedSpec )
                      ( AppT
                          ( ConT ''TYPE )
                          ( AppT
                              ( PromotedT 'BoxedRep )
                              ( PromotedT 'Lifted ) ) ) ]
                  [ ]
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'One ) )
                      ( AppT
                          ( ConT ''Addr# )
                          ( VarT s_nm ) ) )
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
                                  ( VarT s_nm ) ) ) ) ) )
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
                      ( NormalB ( LamE
                          [ AsP
                              ( p_nm )
                              ( ConP
                                  ( 'Addr# )
                                  [ ]
                                  [ VarP a_nm ] )
                          , VarP n_nm ]
                          ( CaseE
                              ( AppE ( AppE ( AppE
                                  ( VarE rd_nm )
                                  ( VarE a_nm ) )
                                  ( VarE n_nm ) )
                                  ( VarE 'realWorld# ) )
                              [ Match
                                  ( UnboxedTupP
                                      [ WildP
                                      , VarP x_nm ] )
                                  ( NormalB ( UnboxedTupE
                                      [ Just ( VarE p_nm )
                                      , Just ( AppE
                                          ( VarE 'ur )
                                          ( VarE x_nm ) ) ] ) )
                                  [ ] ] ) ) )
                      [ ] ] ) )
              [ ]
          , SigD
              ( 'readAddr# )
              ( ForallT
                  [ KindedTV
                      ( s_nm )
                      ( SpecifiedSpec )
                      ( AppT
                          ( ConT ''TYPE )
                          ( AppT
                              ( PromotedT 'BoxedRep )
                              ( PromotedT 'Lifted ) ) ) ]
                  [ ]
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'One ) )
                      ( AppT
                          ( ConT ''Addr# )
                          ( VarT s_nm ) ) )
                      ( AppT ( AppT ( AppT
                          ( MulArrowT )
                          ( PromotedT 'One ) )
                          ( ConT ''Int# ) )
                          ( AppT ( AppT
                              ( UnboxedTupleT 2 )
                              ( AppT
                                  ( ConT ''Addr# )
                                  ( VarT s_nm ) ) )
                              ( AppT
                                  ( ConT ''Ur )
                                  ( eg_ty ) ) ) ) ) )
          , PragmaD ( InlineP
              ( 'readAddr# )
              ( Inline )
              ( ConLike )
              ( AllPhases ) ) ]