{-# LANGUAGE Haskell2010
  , DataKinds
  , GADTs
  , KindSignatures
  , LinearTypes
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , TemplateHaskell
  , UnliftedNewtypes
#-}

{-# OPTIONS_GHC -Wall -Wno-overlapping-patterns -Wno-inaccessible-code #-}

{- | 'State#'-parametrized machine addresses -}
module Data.Addr.Linear.Internal
  ( -- * 'State#'-parametrized machine addresses
    Addr#
      ( Addr# )
    -- * TemplateHaskell generation of 'Addr#' w/r ops
  , deriveAddrOps
  , docAddrOps
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( TYPE
  , pattern Lifted
  , pattern BoxedRep
  , pattern AddrRep
  , Int#
  , pattern Many
  , pattern One
  , State#
  )

import qualified GHC.Exts as RealWorld
  ( Addr# )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( Name
  , mkName
  , nameBase
  , pattern ConE
  , pattern VarE
  , pattern LamE
  , pattern AppE
  , pattern NormalB
  , Type
  , pattern PromotedT
  , pattern ConT
  , pattern AppT
  , pattern UnboxedTupleT
  , pattern ArrowT
  , pattern MulArrowT
  , pattern VarT
  , pattern KindedTV
  , pattern SpecifiedSpec
  , pattern ForallT
  , pattern ConP
  , pattern VarP
  , Dec
  , pattern ValD
  , pattern SigD
  , pattern NoSourceUnpackedness
  , pattern SourceStrict
  , pattern Bang
  , pattern GadtC
  , pattern ForallC
  , pattern DataInstD
  , pattern InstanceD
  , pattern Inline
  , pattern FunLike
  , pattern AllPhases
  , pattern InlineP
  , pattern PragmaD
  , Q
  , pattern DeclDoc
  , putDoc
  )

import Prelude.Linear
  ( Ur
  , ur
  )


-- * 'State#'-parametrized machine addresses

{- | 'State#'-parametrized machine addresses -}
newtype Addr# :: TYPE (BoxedRep Lifted) -> TYPE AddrRep where
    Addr# :: forall s. RealWorld.Addr# %1 -> Addr# s


-- * TemplateHaskell generation of 'Addr#' w/r ops

deriveAddrOps :: Type -> Name -> Name -> [Dec]
deriveAddrOps = \ r_ty wr_nm rd_nm ->
    let s_nm = mkName "s"
    in
      [ ValD
          ( VarP wr_nm )
          ( NormalB (undefined) )
          [ ]
      , SigD
          ( wr_nm )
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
                          ( r_ty ) )
                          ( AppT
                              ( ConT ''Addr# )
                              ( VarT s_nm ) ) ) ) ) )
      , PragmaD ( InlineP
          ( wr_nm )
          ( Inline )
          ( FunLike )
          ( AllPhases ) )
      , ValD
          ( VarP rd_nm )
          ( NormalB (undefined) )
          [ ]
      , SigD
          ( rd_nm )
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
                              ( r_ty ) ) ) ) ) )
      , PragmaD ( InlineP
          ( rd_nm )
          ( Inline )
          ( FunLike )
          ( AllPhases ) )
      ]

docAddrOps :: Name -> Name -> Name -> Q ()
docAddrOps = \ x_ty_nm wr_nm rd_nm -> do
    putDoc (DeclDoc wr_nm) $ "Given arguments @p@, @n@, @x@,"
        <> "linearly consumes @p@, writing @x@ thereto at an offset of @n@ terms of " <> nameBase x_ty_nm <> "#,"
        <> "the result a fresh instance of @p@"
    putDoc (DeclDoc rd_nm) $ "Given arguments @p@, @n@,"
        <> "linearly consumes @p@, reading therefrom at an offset of @n@ terms of " <> nameBase x_ty_nm <> "#,"
        <> "the results a fresh instance of @p@ and the read value in that order"

{-
#define deriveAddrOps(TYP)                                                    \
    {- | Given arguments @p@, @n@, @x@,                                       \
        linearly consumes @p@, writing @x@ thereto at an offset of @n@ bytes, \
        the result a fresh instance of @p@                                    \
    -}                                                                        \
  ; {-# INLINE writeTYPOffAddr #-}                                            \
  ; writeTYPOffAddr# ::                                                       \
      forall (s :: TYPE (BoxedRep Lifted)).                                   \
      Addr# s %1 -> Int# %1 -> TYP# %1 -> Addr# s                             \
  ; writeTYPOffAddr# = case unsafeEqualityProof @Many @One of                 \
        UnsafeRefl -> \ p@(Addr# q) n x ->                                    \
            case RealWorld.writeTYPOffAddr# q n x realWorld# of               \
                _ -> p                                                        \
    {- | Given arguments @p@, @n@, @x@,                                       \
        linearly consumes @p@, reading therefrom at an offset of @n@ bytes,   \
        the results a fresh instance of @p@ and the read value in that order  \
    -}                                                                        \
  ; {-# INLINE readTYPOffAddr #-}                                             \
  ; readTYPOffAddr# ::                                                        \
      forall (s :: TYPE (BoxedRep Lifted)).                                   \
      Addr# s %1 -> Int# %1 -> (# Addr# s, Ur TYP# #)                         \
  ; readTYPOffAddr# = case unsafeEqualityProof @Many @one of                  \
        UnsafeRefl -> \ p@(Addr# q) n ->                                      \
            case RealWorld.readTYPOffAddr# q n realWorld# of                  \
                (# _, x #) -> (# p, ur x #)
-}