{-# LANGUAGE Haskell2010
  , GADTSyntax
  , ImpredicativeTypes
  , KindSignatures
  , MagicHash
  , PatternSynonyms
{-, RankNTypes (redundant) -}
  , RequiredTypeArguments
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Least-degree polynomial interpolation
    of a function defined on an evenly-spaced set of inputs
    via its discrete taylor series
-}
module Calculus.MVector
  ( -- * (Discrete) calculus
    diff
  , taylor
  , pascal
  , extrapolate
  ) where


-- + Imports

-- ++ From base:

import Data.Kind
  ( Type )

import GHC.Exts
  ( runRW# )

import GHC.ST
  ( pattern ST )

import Control.Monad.ST
  ( ST )

import Control.Monad
  ( foldM_ )

import GHC.List
  ( build )

import Data.List
  ( scanl' )


-- ++ From vector:

import qualified Data.Vector.Generic as V
  ( Vector
  , Mutable
  , fromList
  , thaw
  )

import qualified Data.Vector.Generic.Mutable as W
  ( unsafeWrite
  , unsafeInit
  , length
  , unsafeRead
  )


-- * Lazy 'Data.List.unfoldr' in 'Control.Monad.ST'

{- | A safe, lazy, and "fold/build" fusible unfolding
    (\'a la 'Data.List.unfoldr') of 'Data.List.List's
    within 'Control.Monad.ST.ST'\; seems like it should be standard?
    Well, implementing it correctly is surprisingly hard!
    Here's a record of all the ways I messed up:

    __Attempt 1:__ Safe but neither lazy nor fusible

    I.e., the entire 'Control.Monad.ST.ST' action building the result
    is executed the moment the latter is demanded (to WHNF).

    > {-# INLINE unfoldST1 #-}
    > unfoldST1 :: forall rs s a.
    >     (rs -> ST s (Maybe (a, rs))) ->
    >     (ST s rs) ->
    >     ST s [a]
    > unfoldST1 = \ t yrs0 ->
    >     let unfoldSTR = \ rs ->
    >             t rs >>= \case
    >                 Just (a, rs') -> (a :) <$> unfoldSTR rs'
    >                 Nothing       -> pure []
    >     in  unfoldSTR =<< yrs0

    __Attempt 2:__ Lazy but neither safe nor fusible

    E.g., if 'unfoldST2' is applied twice to the same @rs@
    within a single 'Control.Monad.ST.ST' thread, then
    the two 'Data.List.List's produced are contingent
    on the order in which their elements are demanded
    (which GHC is generally free to modify unless
    it has some explicit contraindication).

    > {-# INLINE unfoldST2 #-}
    > unfoldST2 :: forall rs s a.
    >     (rs -> ST s (Maybe (a, rs))) ->
    >     (ST s rs) ->
    >     ST s [a]
    > unfoldST2 = \ t yrs0 ->
    >     let unfoldSTR = \ rs -> Control.Monad.ST.Unsafe.unsafeDupableInterleaveST $
    >             t rs >>= \case
    >                 Just (a, rs') -> (a :) <$> unfoldSTR rs'
    >                 Nothing       -> pure []
    >     in  unfoldSTR =<< yrs0

    __Attempt 3:__ Safe and lazy but not fusible

    > {-# INLINE unfoldST3 #-}
    > unfoldST3 :: forall (r :: Type -> Type) a.
    >     (forall s. r s -> ST s (Maybe (a, r s))) ->
    >     (forall s. ST s (r s)) ->
    >     [a]
    > unfoldST3 = \ t yr0 ->
    >     let -- | requires -XNoMonoLocalBinds to typecheck
    >         unfoldSTR = \ r -> Control.Monad.ST.Unsafe.unsafeDupableInterleaveST $
    >             t r >>= \case
    >                 Just (a, r') -> (a :) <$> unfoldSTR r'
    >                 Nothing      -> pure []
    >     in  runST $ unfoldSTR =<< yr0

    __Attempt 4:__ Safe and fusible but not (necessarily) lazy

    E.g. if the output is filtered and fusion fires
    then the resulting GHC-core is not tail recursive,
    releasing only when the next element satisfying the predicate
    is reached\; if, in particular, this happens infrequently
    then stack usage will blow up in the intervening time(s)

    > {-# INLINE unfoldST4 #-}
    > unfoldST4 :: forall (r :: Type -> Type) a.
    >     (forall s. r s -> ST s (Maybe (a, r s))) ->
    >     (forall s. ST s (r s)) ->
    >     [a]
    > unfoldST4 = \ t yr0 ->
    >     build $ \ g b ->
    >         let -- | requires -XNoMonoLocalBinds to typecheck
    >             unfoldSTR = \ r -> Control.Monad.ST.Unsafe.unsafeDupableInterleaveST $
    >                 t r >>= \case
    >                     Just (a, r') -> g a <$> unfoldSTR r'
    >                     Nothing      -> pure b
    >         in  runST $ unfoldSTR =<< yr0

    __Final attempt:__ Safe, lazy, and fusible

    Note that the explicit passing of the state tokens @s@\*
    in this implementation merely serves as proof to the reader
    that the relevant 'Control.Monad.ST.ST' actions
    execute in the correct order; the pattern in which
    the results of the latter are demanded ensures
    that this will happen even if we discard the former

    > {-# INLINE unfoldST #-}
    > unfoldST :: forall (r :: Type -> Type) a.
    >    (forall s. r s -> ST s (Maybe (a, r s))) ->
    >    (forall s. ST s (r s)) ->
    >    [a]
    > unfoldST =  \ t (ST xr0) ->
    >    build $ \ g b ->
    >        let unfoldSTR = \ r s -> let ST xmtars' = t r in case xmtars' s of
    >                (# s', Just (a, r') #) -> g a $ unfoldSTR r' s'
    >                (# _ , Nothing      #) -> b
    >        in  GHC.Exts.runRW# $ \ s0 -> case xr0 s0 of
    >                (# s0', r0 #) -> unfoldSTR r0 s0'
-}
{-# INLINE unfoldST #-}
unfoldST :: forall (r :: Type -> Type) a.
    (forall s. r s -> ST s (Maybe (a, r s))) ->
    (forall s. ST s (r s)) ->
    [a]
unfoldST =  \ t (ST xr0) ->
    build $ \ g b ->
        let unfoldSTR = \ r s -> let ST xmtars' = t r in case xmtars' s of
                (# s', Just (a, r') #) -> g a $ unfoldSTR r' s'
                (# _ , Nothing      #) -> b
        in  runRW# $ \ s0 -> case xr0 s0 of
                (# s0', r0 #) -> unfoldSTR r0 s0'


-- * (Discrete) calculus

{- | Passes on (the knowable initial segment of)
    the discrete derivative of a given vector of 'Num' values
    assumed to be sequential with increment 1,
    as well as the evaluation of the former at 0,
    until we know 'Nothing'

    Computed in place using a generic mutable 'Data.Vector.Generic.Vector'
    captured by the 'Control.Monad.ST.ST' monad
-}
{-# INLINE diff #-}
diff :: forall (v :: Type -> Type) a s.
    (V.Vector v a, Num a) =>
    V.Mutable v s a -> ST s (Maybe (a, V.Mutable v s a))
diff = \ wa -> do
    let len' = W.length wa - 1
    case compare 0 len' of
        GT -> pure Nothing
        _  -> do
            a0 <- W.unsafeRead wa 0
            foldM_ ( \ a i -> do
                a' <- W.unsafeRead wa $ i + 1
                W.unsafeWrite wa i $ a' - a
                pure a'
              ) a0 [0 .. len'-1]
            pure . Just . (a0,) $ W.unsafeInit wa

{- | Newtype that fiddles with the order of type arguments
    in @forall (v :: Type -> Type) s a. V.Mutable v s a@
    so that we can use 'unfoldST'
-}
newtype WSyn :: (Type -> Type) -> Type -> Type -> Type where
    WSyn :: forall (v :: Type -> Type) a s.
        { unWSyn :: V.Mutable v s a } ->
        WSyn v a s

{- | (The knowable initial segment of)
    the discrete Taylor coefficients of a given vector of 'Num' values
    assumed to be sequential with increment 1

    The implementation caches each intermediate derivative
    as a generic 'Data.Vector.Generic.Vector.Mutable' instance
    captured by the 'Control.Monad.ST.ST' monad,
    its parametrizing immutable vector type represented
    by the required type argument @v@.
-}
{-# INLINE taylor #-}
taylor :: forall (v :: Type -> Type) ->
    forall a.
    (V.Vector v a, Num a) =>
    [a] -> [a]
taylor = \ v ->
    unfoldST (fmap (fmap (fmap WSyn)) . diff @v . unWSyn) . fmap WSyn . V.thaw . V.fromList -- /requires -XImpredicativeTypes to typecheck/

{- | The \(n\)th row of Pascal's triangle
    as a list of \(n+1\) 'Integral' values,
    where \(n\) is the argument

    For all but the \(n\) closest to zero,
    an arbitrary precision 'Integral' type
    should be used!
-}
{-# INLINE pascal #-}
pascal :: forall n. Integral n => n -> [n]
pascal = \ n -> (if n >= 0 then take $ fromIntegral n + 1 else id) $
    scanl' (\ b a -> b * (n - a + 1) `quot` a) 1 [1..]

{- | Extrapolates a given finite list of 'Num' values
    assumed to be sequential from 0 with increment 1
    to a polynomial function of minimal degree
    with arguments 'Integral' values

    The ambiguous type variable @v@, representing
    a generic 'Data.Vector.Generic.Vector' instance,
    is __necessarily specialized by type application at the call site__.

    ==== __Demo__
>>> :set -XHaskell2010 -XTypeApplications -Wall
>>> import qualified Data.Vector.Unboxed ( Vector )
>>> import qualified Calculus.MVector ( extrapolate )
>>> Calculus.MVector.extrapolate Data.Vector.Unboxed.Vector @Int @Integer [0,1,3,6] 100
5050
>>> Calculus.MVector.extrapolate Data.Vector.Unboxed.Vector @Int @Integer [0,1,3,6] 10000
50005000
>>> Calculus.MVector.extrapolate Data.Vector.Unboxed.Vector @Int @Integer [0,1,3,6] (-100)
4950
-}
{-# INLINE extrapolate #-}
extrapolate :: forall (v :: Type -> Type) ->
    forall a n.
    (V.Vector v a, Num a, Integral n) =>
    [a] -> n -> a
extrapolate = \ v sa ->
    sum . zipWith (*) (taylor v sa) . fmap fromIntegral . pascal