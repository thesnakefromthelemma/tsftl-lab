{-# LANGUAGE Haskell2010
  , DataKinds
  , GADTs
  , KindSignatures
  , LinearTypes
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , TemplateHaskell
  , UnboxedTuples
  , UnliftedNewtypes
#-}

{-# OPTIONS_GHC -Wall -Wno-overlapping-patterns -Wno-inaccessible-code #-}

{- | 'State#'-parametrized machine addresses -}
module Data.Addr.Linear.Internal
  ( -- * 'State#'-parametrized machine addresses
    Addr#
      ( Addr# )
    -- * TH-driven derivation of 'Addr#' w/r ops
  , deriveAddrOps
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( TYPE
  , pattern Lifted
  , pattern BoxedRep
  , pattern AddrRep
  , pattern TupleRep
  , State#
  )

import qualified GHC.Exts as RealWorld
  ( Addr# )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

import Language.Haskell.TH
  ( Name
  , mkName
  , pattern LamE
  , pattern AppE
  , pattern Match
  , pattern CaseE
  , pattern AppTypeE
  , pattern UnboxedTupE
  , pattern VarE
  , pattern NormalB
  , pattern AppT
  , pattern UnboxedTupleT
  , pattern MulArrowT
  , pattern PromotedT
  , pattern ConT
  , pattern VarT
  , pattern UnboxedTupP
  , pattern ConP
  , pattern VarP
  , Dec
  , pattern ValD
  , pattern SigD
  , pattern Inline
  , pattern FunLike
  , pattern AllPhases
  , pattern InlineP
  , pattern PragmaD
  , pattern DeclDoc
  , Q
  , putDoc
  )


-- * 'State#'-parametrized machine addresses

newtype Addr# :: TYPE (BoxedRep Lifted) -> TYPE (TupleRep [TupleRep '[], AddrRep]) where
    Addr# :: forall s. (# State# s, RealWorld.Addr# #) %1 -> Addr# s


-- * TH-driven derivation of 'Addr#' w/r ops

deriveAddrOps :: String -> Q [Dec]
deriveAddrOps = \ ty -> do
    let ty_Nam = mkName $ ty <> "#"
    let wr_Nam = mkName $ "write" <> ty <> "OffAddr#"
        wr_Exp = undefined
        wr_Typ = undefined
    putDoc (DeclDoc wr_Nam) $ "Given arguments @p@, @n@, @x@,"
        <> "linearly consumes @p@, writing @x@ thereto at an offset of @n@ terms of " <> ty <> "#,"
        <> "the result a fresh instance of @p@"
    let rd_Nam = mkName $ "read" <> ty <> "OffAddr#"
        rd_Exp = undefined
        rd_Typ = undefined
    putDoc (DeclDoc wr_Nam) $ "Given arguments @p@, @n@,"
        <> "linearly consumes @p@, reading therefrom at an offset of @n@ terms of " <> ty <> "#,"
        <> "the results a fresh instance of @p@ and the read value in that order"    
    pure []

{-
#define deriveAddrOps(TYP)                                                      \
    {- | Given arguments @p@, @n@, @x@,                                         \
        linearly consumes @p@, writing @x@ thereto at an offset of @n@ bytes,   \
        the result a fresh instance of @p@                                      \
    -}                                                                          \
  ; {-# INLINE writeTYPOffAddr #-}                                              \
  ; writeTYPOffAddr# :: forall s. Addr# s %1 -> Int# -> TYP# -> Addr# s         \
  ; writeTYPOffAddr# = case unsafeEqualityProof @(Addr# s -> Int# -> TYP# -> Addr# s) @(Addr# s %1 -> Int# -> TYP# -> Addr# s) of \
        UnsafeRefl -> \ (Addr# (# s0, q #)) n x ->                              \
            Addr# (# RealWorld.writeTYPOffAddr# q n x s0, q #)                  \
    {- | Given arguments @p@, @n@, @x@,                                         \
        linearly consumes @p@, reading therefrom at an offset of @n@ bytes,     \
        the results a fresh instance of @p@ and the read value in that order    \
    -}                                                                          \
  ; {-# INLINE readTYPOffAddr #-}                                               \
  ; readTYPOffAddr# :: forall s. Addr# s %1 -> Int# -> (# Addr# s, TYP# #)      \
  ; readTYPOffAddr# = case unsafeEqualityProof @(Addr# s -> Int# -> (# Addr# s, TYP# #)) @(Addr# s %1 -> Int# -> (# Addr# s, TYP# #)) of \
        UnsafeRefl -> \ (Addr# (# s0, q #)) n ->                                \
            case RealWorld.readTYPOffAddr# q n s0 of                            \
                (# s1, x #) -> (# Addr# (# s1, q #), x #)
-}