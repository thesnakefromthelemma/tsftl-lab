{-# LANGUAGE Haskell2010
  , GADTSyntax
  , KindSignatures
  , MagicHash
  , PatternSynonyms
  , RankNTypes
  , ScopedTypeVariables
  , TemplateHaskellQuotes
  , TupleSections
  , UnboxedTuples
  , UnliftedNewtypes
#-}

{-# OPTIONS_GHC -Wall #-}

{- HLINT ignore "Redundant bracket" -}

{- | Unique references (typically to memory)
    via the 'Control.Monad.ST' quantification trick
-}
module Unique
  ( -- * 'Unique'
    -- ** 'Unique'
    Unique
      ( Unique
      , unUnique
      )
  -- ** 'Unique' operations
  , act#
  , observe#
  -- ** 'Unique' operations in TH
  , actN#
  , observeN#
  -- * 'Frozen#'
  -- ** 'Frozen#'
  , Frozen#
      ( Frozen#
      , unFrozen#
      )
  -- ** Thawing and freezing
  , thawUnsafe#
  , freeze#
  ) where


-- + Imports

-- ++ From base >= 4.21 && << 4.22

import Data.Kind
  ( Type )

import GHC.Exts
  ( UnliftedType
  , unsafeCoerce#
  , State#
  , RealWorld
  , runRW#
  )

-- ++ From template-haskell >= 2.23 && < 2.24

import Language.Haskell.TH
  ( Quote
  , mkName
  , newName
  , pattern VarT
  , pattern AppT
  , pattern ArrowT
  , pattern ForallT
  , pattern PlainTV
  , pattern KindedTV
  , pattern SpecifiedSpec
  , pattern ConT
  , pattern UnboxedTupleT
  , pattern WildP
  , pattern VarP
  , pattern ConP
  , pattern UnboxedTupP
  , pattern VarE
  , pattern AppE
  , pattern LamE
  , pattern ConE
  , pattern CaseE
  , pattern Match
  , Dec
  , pattern SigD
  , pattern ValD
  , pattern NormalB
  , pattern PragmaD
  , pattern InlineP
  , pattern Inline
  , pattern FunLike
  , pattern AllPhases
  )


-- * 'Unique'

-- ** 'Unique'

newtype Unique :: (Type -> UnliftedType) -> Type where
    Unique :: forall (r :: Type -> UnliftedType). {
        unUnique :: forall s. State# s -> (# State# s, r s #) } ->
        Unique r


-- ** 'Unique' operations

{-# INLINE act# #-}
act# :: forall (r :: Type -> UnliftedType).
    (forall s. r s -> State# s -> (# State# s, r s #)) -> Unique r -> Unique r
act# = \ yrs (Unique xrs) -> Unique $
    \ s -> case xrs s of
        (# s', rs #) -> yrs rs s'

{-# INLINE observe# #-}
observe# :: forall (r :: Type -> UnliftedType) a.
    (forall s. r s -> State# s -> (# State# s, a #)) -> Unique r -> a
observe# = \ yrs (Unique xrs) -> runRW# $
    \ s -> case xrs s of
        (# s', rs #) -> case yrs rs s' of
            (# _, a #) -> a


-- ** 'Unique' operations in TH

{- | Requires -XKindSignatures, -XMagicHash, -XRankNTypes, -XUnboxedTuples -}
actN# :: forall (q :: Type -> Type).
    Quote q =>
    Int -> q [Dec]
actN# = \ n -> do
    -- | Name
    let sn = [0..n-1]
        actNam = mkName $ "act" ++ show n ++ "#"
    -- | Type
    rTypNam <- newName "r"
    sTypNam <- newName "s"
    let actTyp =
            let actTypF = \ ~(t0, t1) ->
                  ( AppT ( AppT
                      ( ArrowT )
                      ( AppT
                          ( VarT rTypNam )
                          ( VarT sTypNam ) ) )
                      ( t0 )
                  , AppT ( AppT
                      ( ArrowT )
                      ( AppT
                          ( ConT ''Unique )
                          ( VarT rTypNam ) ) )
                      ( t1 ) )
                actTypK =
                  ( AppT ( AppT
                      ( ArrowT )
                      ( AppT
                          ( ConT ''State# )
                          ( VarT sTypNam ) ) )
                      ( AppT ( AppT
                          ( UnboxedTupleT 2 )
                          ( AppT
                              ( ConT ''State# )
                              ( VarT sTypNam ) ) )
                          ( AppT
                              ( VarT rTypNam )
                              ( VarT sTypNam ) ) )
                  , AppT
                      ( ConT ''Unique )
                      ( VarT rTypNam ) )
                (actTypA0', actTypA1) = foldr (const actTypF) actTypK sn
            in  ForallT
                  [ KindedTV
                      ( rTypNam )
                      ( SpecifiedSpec )
                      ( AppT ( AppT
                          ( ArrowT )
                          ( ConT ''Type ) )
                          ( ConT ''UnliftedType ) ) ]
                  [ ]
                  ( AppT ( AppT 
                      ( ArrowT )
                      ( ForallT
                          [ PlainTV
                              ( sTypNam )
                              ( SpecifiedSpec ) ]
                          [ ] 
                          ( actTypA0' ) ) )
                      ( actTypA1 ) )
    -- | Expression
    yrsExpNam <- newName "yrs"
    s0ExpNam <- newName "s"
    sXrsExpNam <- traverse (newName . ("xrs" ++) . show) sn
    sSExpNam <- traverse (newName . ("s" ++) . show) sn
    sRsExpNam <- traverse (newName . ("rs" ++) . show) sn
    let actExp =
            let actExpF = \ (xrsExpNam, sExpNam', rsExpNam) k (sExp, e) ->
                    CaseE
                      ( AppE
                          ( VarE xrsExpNam )
                          ( sExp ) )
                      [ Match
                          ( UnboxedTupP
                              [ VarP sExpNam'
                              , VarP rsExpNam ] )
                          ( NormalB ( k
                              ( VarE sExpNam'
                              , AppE
                                  ( e )
                                  ( VarE rsExpNam ) ) ) )
                          [ ] ]
                actExpK = \ (sExp, e) ->
                    AppE
                      ( e )
                      ( sExp )
                actExpI =
                  ( VarE s0ExpNam
                  , VarE yrsExpNam
                  )
            in  LamE
                  ( VarP yrsExpNam : fmap (ConP 'Unique [] . pure . VarP) sXrsExpNam )
                  ( AppE
                      ( ConE 'Unique )
                      ( LamE
                          [ VarP s0ExpNam ]
                          ( foldr actExpF actExpK (zip3 sXrsExpNam sSExpNam sRsExpNam) actExpI ) ) )
    -- | Declaration(s)
    pure
      [ SigD
          ( actNam )
          ( actTyp )
      , ValD
          ( VarP actNam )
          ( NormalB actExp )
          [ ]
      , PragmaD ( InlineP
          ( actNam )
          ( Inline )
          ( FunLike )
          ( AllPhases ) ) ]

{- | Requires -XKindSignatures, -XMagicHash, -XRankNTypes, -XUnboxedTuples -}
observeN# :: forall (q :: Type -> Type).
    Quote q =>
    Int -> q [Dec]
observeN# = \ n -> do
    -- | Name
    let sn = [0..n-1]
        observeNam = mkName $ "observe" ++ show n ++ "#"
    -- | Type
    rTypNam <- newName "r"
    aTypNam <- newName "a"
    sTypNam <- newName "s"
    let observeTyp =
            let observeTypF = \ ~(t0, t1) ->
                  ( AppT ( AppT
                      ( ArrowT )
                      ( AppT
                          ( VarT rTypNam )
                          ( VarT sTypNam ) ) )
                      ( t0 )
                  , AppT ( AppT
                      ( ArrowT )
                      ( AppT
                          ( ConT ''Unique )
                          ( VarT rTypNam ) ) )
                      ( t1 ) )
                observeTypK =
                  ( AppT ( AppT
                      ( ArrowT )
                      ( AppT
                          ( ConT ''State# )
                          ( VarT sTypNam ) ) )
                      ( AppT ( AppT
                          ( UnboxedTupleT 2 )
                          ( AppT
                              ( ConT ''State# )
                              ( VarT sTypNam ) ) )
                          ( VarT aTypNam ) )
                  , VarT aTypNam )
                (observeTypA0', observeTypA1) = foldr (const observeTypF) observeTypK sn
            in  ForallT
                  [ KindedTV
                      ( rTypNam )
                      ( SpecifiedSpec )
                      ( AppT ( AppT
                          ( ArrowT )
                          ( ConT ''Type ) )
                          ( ConT ''UnliftedType ) )
                  , PlainTV
                      ( aTypNam )
                      ( SpecifiedSpec ) ]
                  [ ]
                  ( AppT ( AppT 
                      ( ArrowT )
                      ( ForallT
                          [ PlainTV
                              ( sTypNam )
                              ( SpecifiedSpec ) ]
                          [ ] 
                          ( observeTypA0' ) ) )
                      ( observeTypA1 ) )
    -- | Expression
    yrsExpNam <- newName "yrs"
    s0ExpNam <- newName "s"
    sXrsExpNam <- traverse (newName . ("xrs" ++) . show) sn
    sSExpNam <- traverse (newName . ("s" ++) . show) sn
    sRsExpNam <- traverse (newName . ("rs" ++) . show) sn
    aExpNam <- newName "a"
    let observeExp =
            let observeExpF = \ (xrsExpNam, sExpNam', rsExpNam) k (sExp, e) ->
                    CaseE
                      ( AppE
                          ( VarE xrsExpNam )
                          ( sExp ) )
                      [ Match
                          ( UnboxedTupP
                              [ VarP sExpNam'
                              , VarP rsExpNam ] )
                          ( NormalB ( k
                              ( VarE sExpNam'
                              , AppE
                                  ( e )
                                  ( VarE rsExpNam ) ) ) )
                          [ ] ]
                observeExpK = \ (sExp, e) ->
                    CaseE
                      ( AppE
                          ( e )
                          ( sExp ) )
                      [ Match
                          ( UnboxedTupP
                              [ WildP
                              , VarP aExpNam ] )
                          ( NormalB ( VarE aExpNam ) )
                          [ ] ]
                observeExpI =
                  ( VarE s0ExpNam
                  , VarE yrsExpNam )
            in  LamE
                  ( VarP yrsExpNam : fmap (ConP 'Unique [] . pure . VarP) sXrsExpNam )
                  ( AppE
                      ( VarE 'runRW# )
                      ( LamE
                          [ VarP s0ExpNam ]
                          ( foldr observeExpF observeExpK (zip3 sXrsExpNam sSExpNam sRsExpNam) observeExpI ) ) )
    -- | Declaration(s)
    pure
      [ SigD
          ( observeNam )
          ( observeTyp )
      , ValD
          ( VarP observeNam )
          ( NormalB observeExp )
          [ ]
      , PragmaD ( InlineP
          ( observeNam )
          ( Inline )
          ( FunLike )
          ( AllPhases ) ) ]


-- * 'Frozen#'

-- ** 'Frozen#'

newtype Frozen# :: (Type -> UnliftedType) -> UnliftedType where
    Frozen# :: forall (r :: Type -> UnliftedType). {
        unFrozen# :: r RealWorld } ->
        Frozen# r


-- ** Thawing and freezing

{-# INLINE thawUnsafe# #-}
thawUnsafe# :: forall (r :: Type -> UnliftedType).
    Frozen# r -> Unique r
thawUnsafe# = \ (Frozen# rs) -> Unique $
    \ s -> (# s, #) $ (unsafeCoerce# :: r RealWorld -> r s) rs

{-# INLINE freeze# #-}
freeze# :: forall (r :: Type -> UnliftedType).
    Unique r -> Frozen# r
freeze# = \ (Unique xrs) -> runRW# $
    \ s -> case xrs s of
        (# _, rs #) -> Frozen# $ (unsafeCoerce# :: r s -> r RealWorld) rs