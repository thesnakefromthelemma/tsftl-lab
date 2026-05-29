{-# LANGUAGE Haskell2010
  , DataKinds
  , GADTSyntax
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , PolyKinds
  , RoleAnnotations
  , ScopedTypeVariables
  , StandaloneKindSignatures
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
      , BoxedRep
      )
  , TYPE
  , Constraint
  , Int#
  , State#
  )

import qualified GHC.Exts as GHC
  ( Addr# )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( newName
  , pattern AppE
  , pattern VarE
  , pattern LamE
  , pattern UnboxedTupleT
  , pattern ArrowT
  , pattern ConT
  , pattern AppT
  , pattern VarT
  , pattern PlainTV
  , pattern SpecifiedSpec
  , pattern ForallT
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
  , pattern MagicHash
  , pattern ScopedTypeVariables
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


-- *  @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses

{- |  @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses -}
type role Addr# nominal
newtype Addr# :: TYPE (BoxedRep Lifted) -> TYPE AddrRep where
    Addr# ::
        forall (s :: TYPE (BoxedRep Lifted)).
        GHC.Addr# -> Addr# s


-- * Representation-polymorphic interface to writing/reading off 'Addr#'s

-- ** Representation-polymorphic interface to writing/reading off 'Addr#'s

{- | Representation-polymorphic interface to writing/reading off 'Addr#'s -}
type Addrable# ::
    forall {r :: RuntimeRep}. TYPE r -> Constraint
class Addrable# a where
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

-- ** TemplateHaskell generation of standard 'Addrable#' instances

{- | Given argument @r@, representing a promoted term of type 'RuntimeRep',
    generates an 'Addrable#' instance
    for the standard representation instance @EG_TY@ of @r@\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_Addrable#(EG_TY)                                          \
        instance Addrable# (EG_TY) where                                         \
            {-# INLINE CONLIKE writeAddr# #-}                                    \
          ; writeAddr# ::                                                        \
                forall s. Addr# s -> Int# -> EG_TY# -> State# s -> State# s      \
          ; writeAddr# = \ (Addr# a) -> GHC.Exts.writeEG_TYOffAddr# a            \
          ; {-# INLINE CONLIKE readAddr# #-}                                     \
          ; readAddr# ::                                                         \
                forall s. Addr# s -> Int# -> State# s -> (# State# s, EG_TY# #)  \
          ; readAddr# = \ (Addr# a) -> GHC.Exts.readEG_TYOffAddr# a
    @
    Requires @-XInstanceSigs -XMagicHash -XScopedTypeVariables@.
    Requires that "GHC.Exts.writeEG_TYOffAddr#" and "GHC.Exts.readEG_TYOffAddr# " be in scope.
-}
declareAddrableEg :: RuntimeRep -> Q Dec
declareAddrableEg = \ r -> do
    guardExts
      ( "\'Data.Addr.declareAddrableEg\'")
      [ InstanceSigs
      , MagicHash
      , ScopedTypeVariables ]
    let eg_ty = repEg r
    wr_nm <- guardValue
      ( "\'Data.Addr.declareAddrableEg\'" )
      ( "GHC.Exts.write" <> repStem r <> "OffAddr#" )
    rd_nm <- guardValue
      ( "\'Data.Addr.declareAddrableEg\'" )
      ( "GHC.Exts.read" <> repStem r <> "OffAddr#" )
    s_ty_nm <- newName "s"
    a_nm <- newName "a"
    pure
      ( InstanceD
          ( Nothing )
          [ ]
          ( AppT
              ( ConT ''Addrable# )
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
                  [ PlainTV
                      ( s_ty_nm )
                      ( SpecifiedSpec ) ]
                  [ ]
                  ( AppT ( AppT
                      ( ArrowT )
                      ( AppT
                          ( ConT ''Addr# )
                          ( VarT s_ty_nm ) ) )
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
                                      ( VarT s_ty_nm ) ) )
                                  ( AppT
                                      ( ConT ''State# )
                                      ( VarT s_ty_nm ) ) ) ) ) ) )
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
                  [ PlainTV
                      ( s_ty_nm )
                      ( SpecifiedSpec ) ]
                  [ ]
                  ( AppT ( AppT
                      ( ArrowT )
                      ( AppT
                          ( ConT ''Addr# )
                          ( VarT s_ty_nm ) ) )
                      ( AppT ( AppT
                          ( ArrowT )
                          ( ConT ''Int# ) )
                          ( AppT ( AppT
                              ( ArrowT )
                              ( AppT
                                  ( ConT ''State# )
                                  ( VarT s_ty_nm ) ) )
                              ( AppT ( AppT
                                  ( UnboxedTupleT 2 )
                                  ( AppT
                                      ( ConT ''State# )
                                      ( VarT s_ty_nm ) ) )
                                  ( eg_ty ) ) ) ) ) )
          , PragmaD ( InlineP
              ( 'readAddr# )
              ( Inline )
              ( ConLike )
              ( AllPhases ) ) ] )