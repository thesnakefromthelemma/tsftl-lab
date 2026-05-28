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

{- | @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses,
    representation-polymorphic interface to writing/reading off them,
    and TemplateHaskell generation of instances thereof
-}
module Data.Addr.TH
  ( -- *  @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses
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
  ( pattern Lifted
  , RuntimeRep
      ( AddrRep
      , BoxedRep
      )
  , TYPE
  , Int#
  , State#
  )

import qualified GHC.Exts as GHC
  ( Addr# )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( mkName
  , newName
  , pattern AppE
  , pattern VarE
  , pattern PromotedT
  , pattern UnboxedTupleT
  , pattern ArrowT
  , pattern ConT
  , pattern AppT
  , pattern VarT
  , pattern KindedTV
  , pattern SpecifiedSpec
  , pattern ForallT
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
  , Quote
  )

import Data.Coerce
  ( coerce )

-- ++ (internal)

import Data.RuntimeRep
  ( repType
  , repStem
  , repEg
  )


-- *  @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses

{- |  @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses -}
type role Addr# nominal
newtype Addr# :: TYPE (BoxedRep Lifted) -> TYPE AddrRep where
    Addr# ::
        forall (s :: TYPE (BoxedRep Lifted)).
        GHC.Addr# -> Addr# s


-- * Writing/reading off 'Addr#'s

-- ** Representation-polymorphic interface to writing/reading off 'Addr#'s

{- | Representation-polymorphic interface to writing/reading off 'Addr#'s -}
class Addrable (r :: RuntimeRep) (a :: TYPE r) where
    {- | Given arguments @p@, @i@, @x@,
        returns the 'State#' action
        writing @x@ to @p + repBytes r * i bytes@
    -}
    writeAddr# :: forall s. Addr# s -> Int# -> a -> State# s -> State# s
    {- | Given arguments @p@, @i@,
        returns the 'State#' action
        reading @x@ from @p + repBytes r * i bytes@
        returning @x@
    -}
    readAddr# :: forall s. Addr# s -> Int# -> State# s -> (# State# s, a #)

-- ** TemplateHaskell generation of standard 'Addrable' instances

{- | Given argument @r@, representing a promoted term of type 'RuntimeRep',
    generates an 'Addrable' instance
    for the standard representation instance of @r@\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_Addrable(r_ty, EG_TY)                                    \
        instance Addrable (r_ty) (EG_TY) where                                  \
            {-# INLINE writeAddr# #-}                                           \
          ; writeAddr# ::                                                       \
                forall s. Addr# s -> Int# -> EG_TY# -> State# s -> State# s     \
          ; writeAddr# = coerce GHC.writeEG_TYOffAddr#                          \
          ; {-# INLINE readAddr# #-}                                            \
          ; readAddr# ::                                                        \
                forall s. Addr# s -> Int# -> State# s -> (# State# s, EG_TY# #) \
          ; readAddr# = coerce GHC.readEG_TYOffAddr#
    @
    Requires at least @-XFlexibleInstances -XInstanceSigs -XKindSignatures -XMultiParamTypeClasses -XScopedTypeVariables -XTemplateHaskell@,
    but this is not checked.
-}
deriveAddrable :: forall q. Quote q => RuntimeRep -> q Dec
deriveAddrable = \ r -> do
    let r_ty = repType r
    let eg_ty = repEg r
    let wr_nm = mkName $ "GHC.write" <> repStem r <> "OffAddr#"
    let rd_nm = mkName $ "GHC.read" <> repStem r <> "OffAddr#"
    s_nm <- newName "s"
    pure
      ( InstanceD
          ( Nothing )
          [ ]
          ( AppT ( AppT
              ( ConT ''Addrable )
              ( r_ty ) )
              ( eg_ty ) )
          [ ValD
              ( VarP 'writeAddr# )
              ( NormalB ( AppE
                  ( VarE 'coerce )
                  ( VarE wr_nm ) ) )
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
              ( NormalB ( AppE
                  ( VarE 'coerce )
                  ( VarE rd_nm ) ) )
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
              ( AllPhases ) ) ] )