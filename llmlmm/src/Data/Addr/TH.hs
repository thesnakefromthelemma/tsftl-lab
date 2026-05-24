{-# LANGUAGE Haskell2010
  , DataKinds
  , GADTSyntax
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
module Data.Addr.TH
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
  , State#
  , Int#
  )

import qualified GHC.Exts as GHC
  ( Addr# )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( mkName
  , pattern AppE
  , pattern VarE
  , pattern LamE
  , pattern PromotedT
  , pattern UnboxedTupleT
  , pattern ArrowT
  , pattern ConT
  , pattern AppT
  , pattern VarT
  , pattern KindedTV
  , pattern SpecifiedSpec
  , pattern ForallT
  , pattern ConP
  , pattern VarP
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



-- * 'State#'-parametrized machine addresses

{- | 'State#'-parametrized machine addresses -}
type role Addr# nominal
newtype Addr# :: TYPE (BoxedRep Lifted) -> TYPE AddrRep where
    Addr# ::
        forall (s :: TYPE (BoxedRep Lifted)).
        GHC.Addr# -> Addr# s


-- * Writing/reading off 'Addr#'s

-- ** Representation-polymorphic interface to writing/reading off 'Addr#'s

{- | Representation-polymorphic interface to writing/reading off 'Addr#'s -}
class Addrable (r :: RuntimeRep) (a :: TYPE r) where
    {- | Given arguments @p@, @n@, @x@,
        returns the 'State#' action
        writing @x@ to @p@ at an offset of @n * repBytes(a)@ bytes
    -}
    writeAddr# :: forall s. Addr# s -> Int# -> a -> State# s -> State# s
    {- | Given arguments @p@, @n@,
        returns the 'State#' action
        reading from @p@ at an offset of  @n * repBytes(a)@ bytes
    -}
    readAddr# :: forall s. Addr# s -> Int# -> State# s -> (# State# s, a #)

-- ** TemplateHaskell generation of standard 'Addrable' instances

{- | Given argument @r@, representing a promoted term of type 'RuntimeRep',
    generates a 'Addrable' instance
    for the standard representation instance of @r@\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_Addrable(r_ty, EG_TY)                                     \
        instance Addrable (r_ty) (EG_TY) where                                   \
            {-# INLINE CONLIKE write #-}                                         \
          ; write :: forall s. Addr# s -> Int# -> EG_TY# -> State# s -> State# s \
          ; write = \ (Addr# a) -> GHC.writeEG_TYOffAddr# a                      \
          ; {-# INLINE CONLIKE read #-}                                          \
          ; read :: forall s. Addr# s -> Int# -> State# s -> (# State# s, a #)   \
          ; read = \ (Addr# a) -> GHC.writeEG_TYOffAddr# a
    @
    Requires at least @-XInstanceSigs -XKindSignatures -XMultiParamTypeClasses -XScopedTypeVariables -XTemplateHaskell@,
    but this is not checked.
-}
deriveAddrable :: RuntimeRep -> Dec
deriveAddrable = \ r ->
    let r_ty = repType r
        eg_ty = repEg r
        wr_nm = mkName $ "GHC.write" <> repStem r <> "OffAddr#"
        rd_nm = mkName $ "GHC.read" <> repStem r <> "OffAddr#"
        s_nm = mkName "s"
        a_nm = mkName "a"
    in  InstanceD
          ( Nothing )
          [ ]
          ( AppT ( AppT
              ( ConT ''Addrable )
              ( r_ty ) )
              ( eg_ty ) )
          [ ValD
              ( VarP 'writeAddr# )
              ( NormalB ( LamE
                  [ ConP
                      ( 'Addr# )
                      [ ]
                      [ VarP a_nm ] ]
                  ( AppE
                      ( VarE wr_nm )
                      ( VarE a_nm ) ) ) )
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
                  ( AppT ( AppT
                      ( ArrowT )
                      ( AppT
                          ( ConT ''Addr# )
                          ( VarT s_nm ) ) )
                      ( AppT ( AppT
                          ( ArrowT )
                          ( ConT ''Int# ) )
                          ( AppT ( AppT
                              ( ArrowT )
                              ( eg_ty ) )
                              ( AppT ( AppT
                                  ( ArrowT )
                                  ( AppT
                                      ( ConT ''State# )
                                      ( VarT s_nm ) ) )
                                  ( AppT
                                      ( ConT ''State# )
                                      ( VarT s_nm ) ) ) ) ) ) )
          , PragmaD ( InlineP
              ( 'writeAddr# )
              ( Inline )
              ( ConLike )
              ( AllPhases ) )
          , ValD
              ( VarP 'readAddr# )
              ( NormalB ( LamE
                  [ ConP
                      ( 'Addr# )
                      [ ]
                      [ VarP a_nm ] ]
                  ( AppE
                      ( VarE rd_nm )
                      ( VarE a_nm ) ) ) )
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
                  ( AppT ( AppT
                      ( ArrowT )
                      ( AppT
                          ( ConT ''Addr# )
                          ( VarT s_nm ) ) )
                      ( AppT ( AppT
                          ( ArrowT )
                          ( ConT ''Int# ) )
                          ( AppT ( AppT
                              ( ArrowT )
                              ( AppT
                                  ( ConT ''State# )
                                  ( VarT s_nm ) ) )
                              ( AppT ( AppT
                                  ( UnboxedTupleT 2 )
                                  ( AppT
                                      ( ConT ''State# )
                                      ( VarT s_nm ) ) )
                                  ( eg_ty ) ) ) ) ) )
          , PragmaD ( InlineP
              ( 'readAddr# )
              ( Inline )
              ( ConLike )
              ( AllPhases ) ) ]