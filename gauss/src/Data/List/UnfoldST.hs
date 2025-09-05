{-# LANGUAGE Haskell2010
  , KindSignatures
  , MagicHash
  , PatternSynonyms
  , RankNTypes
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Safely, lazily, and fusibly unfolding 'Data.List.List's
    within 'Control.Monad.ST.ST'
-}
module Data.List.UnfoldST
  ( -- * 'unfoldST'
    unfoldST
  ) where


-- * Imports

-- ** base

import Data.Kind
  ( Type )

import GHC.Exts
  ( runRW#
  , build
  )

import Control.Monad.ST
  ( ST )

import GHC.ST
  ( pattern ST )


-- * 'unfoldST'

{- | A safe, lazy, and fusible unfolding
    (\'a la 'Data.List.unfoldr') of 'Data.List.List's
    within 'Control.Monad.ST.ST'\; seems like it should be standard?
    Well, implementing it correctly is hard!
    My process looked something like this:

    __Attempt 1:__ Safe but neither lazy nor fusible

    I.e., the entire 'Control.Monad.ST.ST' action building the result
    is executed the moment the latter is demanded (to WHNF).

@
{-# INLINE unfoldST1 #-}
unfoldST1 :: forall rs s a.
    (rs -> ST s (Maybe (a, rs))) ->
    (ST s rs) ->
    ST s [a]
unfoldST1 = \ t yrs0 ->
    let unfoldSTR = \ rs ->
            t rs >>= \case
                Just (a, rs') -> (a :) <$> unfoldSTR rs'
                Nothing       -> pure []
    in  unfoldSTR =<< yrs0
@

    __Attempt 2:__ Lazy but neither safe nor fusible

    E.g., if 'unfoldST2' is applied twice to the same @rs@
    within a single 'Control.Monad.ST.ST' thread, then
    the two 'Data.List.List's produced are contingent
    on the order in which their elements are demanded
    (which GHC is generally free to modify unless
    it has some explicit contraindication).

@
{-# INLINE unfoldST2 #-}
unfoldST2 :: forall rs s a.
    (rs -> ST s (Maybe (a, rs))) ->
    (ST s rs) ->
    ST s [a]
unfoldST2 = \ t yrs0 ->
    let unfoldSTR = \ rs -> unsafeDupableInterleaveST $
            t rs >>= \case
                Just (a, rs') -> (a :) <$> unfoldSTR rs'
                Nothing       -> pure []
    in  unfoldSTR =<< yrs0
@

    __Attempt 3:__ Safe and lazy but not fusible

@
{-# INLINE unfoldST3 #-}
unfoldST3 :: forall (r :: Type -> Type) a.
    (forall s. r s -> ST s (Maybe (a, r s))) ->
    (forall s. ST s (r s)) ->
    [a]
unfoldST3 = \ t yr0 ->
    let unfoldSTR = \ r -> unsafeDupableInterleaveST $
            t r >>= \case
                Just (a, r') -> (a :) <$> unfoldSTR r'
                Nothing      -> pure []
    in  runST $ unfoldSTR =<< yr0
@

    __Attempt 4:__ Safe and fusible but not (necessarily) lazy

    E.g. if the output is filtered and fusion fires
    then the resulting core is not tail recursive,
    releasing only when the next element satisfying the predicate
    is reached\; if, in particular, this happens infrequently
    then stack usage will blow up in the intervening time(s)

@
{-# INLINE unfoldST4 #-}
unfoldST4 :: forall (r :: Type -> Type) a.
    (forall s. r s -> ST s (Maybe (a, r s))) ->
    (forall s. ST s (r s)) ->
    [a]
unfoldST4 = \ t yr0 ->
    build $ \ g b ->
        let -- | requires -XNoMonoLocalBinds to typecheck
            unfoldSTR = \ r -> unsafeDupableInterleaveST $
                t r >>= \case
                    Just (a, r') -> g a <$> unfoldSTR r'
                    Nothing      -> pure b
        in  runST $ unfoldSTR =<< yr0
@

    __Final attempt:__ Safe, lazy, and fusible

    Note that the explicit passing of the state tokens @s@\*
    in this implementation merely serves as proof to the reader
    that the relevant 'Control.Monad.ST.ST' actions
    execute in the correct order; the pattern in which
    the results of the latter are demanded ensures
    that this will happen even if we discard the former
    
@
{-# INLINE unfoldST #-}
unfoldST :: forall (r :: Type -> Type) a.
    (forall s. r s -> ST s (Maybe (a, r s))) ->
    (forall s. ST s (r s)) ->
    [a]
unfoldST = \ t (ST xr0) ->
    build $ \ g b ->
        let unfoldSTR = \ r s -> let ST xmar' = t r in case xmar' s of
                (# s', Just (a, r') #) -> g a $ unfoldSTR r' s'
                (# _ , Nothing      #) -> b
        in  runRW# $ \ s0 -> case xr0 s0 of
                (# s0', r0 #) -> unfoldSTR r0 s0'
@
-}
{-# INLINE unfoldST #-}
unfoldST :: forall (ref :: Type -> Type) a.
    (forall s. ref s -> ST s (Maybe (a, ref s))) ->
    (forall s. ST s (ref s)) ->
    [a]
unfoldST = \ t (ST xr0) ->
    build $ \ g b ->
        let unfoldSTR = \ r s -> let ST xmar' = t r in case xmar' s of
                (# s', Just (a, r') #) -> g a $ unfoldSTR r' s'
                (# _ , Nothing      #) -> b
        in  runRW# $ \ s0 -> case xr0 s0 of
                (# s0', r0 #) -> unfoldSTR r0 s0'