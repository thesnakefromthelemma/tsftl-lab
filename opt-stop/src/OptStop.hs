{-# LANGUAGE Haskell2010
  , BangPatterns
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTSyntax
  , GeneralizedNewtypeDeriving
  , InstanceSigs
{-, KindSignatures (redundant) -}
  , LambdaCase
{-, MultiParamTypeClasses (redundant) -}
  , ScopedTypeVariables
  , StandaloneDeriving
  , TupleSections
  , TypeApplications
  , TypeFamilyDependencies
#-}

{-# OPTIONS_GHC -Wall -fno-cse #-}

{- | Motivating baby's first optimal stopping solution via 'Data.Function.fix' -}
module OptStop
  ( -- * Context
    -- ** Dependent random variables
    DRandVar
      ( plt
      , pgt
      , mean
      , mlt
      , mgt
      )
    -- ** Transitionable state spaces
    , TransState
        ( transition )
    , TransNat
    -- ** Strategy representations
  , Choice
      ( Accept
      , Reject
      )
  , Strategy
  , StratRep
      ( apply
      , eval
      , thresh
      )
    -- * Generic solution
    -- ** Generic optimal stopping
  , bootstrap
  , optStrat
    -- * Implementations of 'RandVar'
    -- ** Uniform distribution
  , Mu
      ( Mu )
    -- ** Exponential distribution with log utility
  , Nu
      ( Nu )
    -- * Implementations of 'StratRep'
    -- ** Strategies as threshold functions
  , FunRep
      ( FunRep
      , unFunRep
      )
    -- ** Strategies as memoized threshold functions
  , HashRep
      ( HashRep
      , unHashRep
      )
    -- * Demonstrations
  , optStratMuAsFun
  , optStratMuAsHash
  , optStratNuAsFun
  , optStratNuAsHash
  ) where


-- From base ^>= 4.18:

import Data.Kind
  ( Type )

import Data.Function
  ( fix )

import Data.Foldable
  ( foldlM )

import Data.Proxy
  ( Proxy )

import Data.Maybe
  ( fromJust )

import Data.IORef
  ( IORef
  , newIORef
  , modifyIORef'
  , readIORef
  )

import System.IO.Unsafe
  ( unsafeDupablePerformIO )


-- From hashable ^>= 1.5 and unordered-containers ^>= 0.2

import Data.Hashable
  ( Hashable )

import Data.HashMap.Strict
  ( HashMap )

import qualified Data.HashMap.Strict as HashMap
  ( empty
  , insert
  , lookup
  , member
  )

import Data.HashSet
  ( HashSet )

import qualified Data.HashSet as HashSet
  ( empty
  , insert
  , member
  )


-- Local:

import qualified Math.Calculus.Simpson
  ( integrate )


-- Misc.

{- | Pair type strict in both type arguments -}
data Pair :: Type -> Type -> Type where
    Pair :: forall a0 a1. !a0 -> !a1 -> Pair a0 a1


-- * Context

-- ** Dependent random variables

{- | Interface to a dependent (on some parameter \(s\))
random variable realized as
a dependent probability measure \(\mu_s\) on \(\mathbb{R}\)
with dependent payoff function \(f_s\) of type \(\mathbb{R}\to\mathbb{R}\)\;
should satisfy

[nonnegativity of 'plt'] @forall (s :: s) (a :: a). plt s a >= 0@

[nonnegativity of 'pgt'] @forall (s :: s) (a :: a). pgt s a >= 0@

[normalization] @forall (s :: s) (a :: a). plt s a + pgt s a = 1@

[coherence] @forall (s :: s) (a :: a). mlt s a + mgt s a = mean s@

(the latter two laws being automatic when
precisely a minimal subset of methods is implemented)
-}
class (Ord a, Fractional a) => DRandVar s a where
    {-# MINIMAL (plt | pgt), ((mean, mlt) | (mean, mgt) | (mlt, mgt)) #-}

    {- | Dependent probability that a sampled value
    is less than the argument\; if \(x\) is said argument,
    then \(\int_{t\ \colon\ \left(-\infty,x\right)}\text{d}\mu_s\)
    -}
    plt :: s -> a -> a
    plt = \ s a ->
        1 - pgt s a

    {- | Dependent probability that a sampled value
    is greater than __or equal to__ the argument\; if \(x\) is said argument,
    then \(\int_{t\ \colon\ \left[x,\infty\right)}\ \text{d}\mu_s\)
    -}
    pgt :: s -> a -> a
    pgt = \ s a ->
        1 - plt s a

    {- | Dependent expectation of the distribution, i.e.
    \(\int_{t\ \colon\ \left(-\infty,\infty\right)}f_s(t)\ \text{d}\mu_s\)
    -}
    mean :: s -> a
    mean = \ s ->
        mlt s 0 + mgt s 0

    {- | Dependent contribition to the expectation
    by values less than the argument\;
    if \(x\) is said argument, then
    \(\int_{t\ \colon\ \left(-\infty,x\right)}f_s(t)\ \text{d}\mu_s\)
    -}
    mlt :: s -> a -> a
    mlt = \ s a ->
        mean s - mgt s a

    {- | Dependent contribition to the expectation
    by values greater than __or equal to__ the argument\;
    if \(x\) is said argument, then
    \(\int_{t\ \colon\ \left[x,\infty\right)}f_s(t)\ \text{d}\mu_s\)
    -}
    mgt :: s -> a -> a
    mgt = \ s a ->
        mean s - mlt s a


-- ** State spaces with nondeterministic transition

class TransState s a where
    transition :: s -> [(a,s)]

newtype TransNat where
    TransNat :: { unTransNat :: Word } -> TransNat

deriving instance Eq TransNat
deriving instance Num TransNat
deriving instance Hashable TransNat

{- | (Just for convenience in the REPL) -}
deriving instance Enum TransNat
deriving instance Show TransNat

instance forall a. Num a => TransState TransNat a where
    transition :: TransNat -> [(a, TransNat)]
    transition = \case
        0 -> []
        n -> pure . (1,) $ n - 1


-- ** Strategy representations

{- | Type representing that
at a given phase of the game we either 'Accept' or 'Reject'
our current draw, at least while we have the choice.
-}
data Choice where
    Accept, Reject :: Choice

{- | (Just for convenience in the REPL) -}
deriving instance Show Choice

{- | Type encoding the fully abstract notion of a \"strategy\"\;
in practice one can only work with it through its representations
via the 'StratRep' class below
-}
type Strategy s a =
    s -> a -> Choice

{- | Minimalistic collection of properties of \"strategies\"
necessary to make sense of their representations as such
in the context of the generic solution to
the simple optimal stopping problem\;
should satisfy

[Coherence] @forall (t :: s -> a) (s :: s) (a :: a). apply (thresh t) s a = case compare (t s) a of GT -> Reject; _ -> Accept@
-}
class DRandVar s a => StratRep x s a | x -> s, x -> a where
    {- | Unpacking of the argument
    into the strategy it represents\;
    not actually used below,
    but pedagogically useful to have/implement anyway
    -}
    apply :: x -> Strategy s a

    {- | Representation of the argument,
    understood as a \"payoff threshold strategy\"
    -}
    thresh :: (s -> a) -> x

    {- | Expectation of the result of implementing
    the first argument at the second argument state
    of the dependent distribution of payoffs
    -}
    eval :: x -> s -> a

    {- | Hack to make the 'HashMap' implementation efficient\;
    treat as a wart (at least on first read)
    -}
    type MemoType x = mx | mx -> x
    type MemoType x = Proxy x

    {- | Hack to make the 'HashMap' implementation efficient\;
    treat as a wart (at least on first read)
    -}
    {-# NOINLINE memo #-}
    memo :: MemoType x
    memo = undefined



-- * Generic solution

-- ** Generic optimal stopping

{- |
> bootstrap = thresh . eval

Key \"bootstrapping\" function that,
from the strategy represented by the argument,
constructs a strategy with at least as great expectation
-}
bootstrap :: forall x s a.
    (DRandVar s a, StratRep x s a) => x -> x
bootstrap = thresh . eval

{- |
> optStrat = fix bootstrap

Generic optimal strategy (representation)—at least
when the methods of @a@\'s 'RandVar' instance are continuous—as
the unique 'Data.Function.fix'ed point of 'bootstrap'
-}
optStrat :: forall x s a.
    (DRandVar s a, StratRep x s a) => x
optStrat = fix bootstrap


-- * Implementations of 'RandVar'

-- ** Independent uniform distributions

{- | Independent uniform distributions on the unit interval with utility(a) = a -}
newtype Mu :: Type -> Type -> Type where
    Mu :: forall s a. a -> Mu s a

deriving instance forall s a. Eq a => Eq (Mu s a)
deriving instance forall s a. Ord a => Ord (Mu s a)
deriving instance forall s a. Num a => Num (Mu s a)
deriving instance forall s a. Fractional a => Fractional (Mu s a)

{- | (Just for convenience in the REPL) -}
deriving instance forall s a. Show a => Show (Mu s a)

instance forall s a. (Ord a, Fractional a) => DRandVar s (Mu s a) where
    plt :: s -> Mu s a -> Mu s a
    plt = \ _ ma -> case (compare 0 ma, compare 1 ma) of
        (GT, _ ) -> 0
        (_ , LT) -> 1
        _        -> ma

    mean :: s -> Mu s a
    mean = \ _ ->
        1 / 2

    mlt :: s -> Mu s a -> Mu s a
    mlt = \ _ ma -> case (compare 0 ma, compare 1 ma) of
        (GT, _ ) -> 0
        (_ , LT) -> 1 / 2
        _        -> ma ^ (2 :: Int) / 2


-- ** Independent exponential distributions with log utility

{- | Independent exponential distributions with utility(a) = log(1+a) -}
newtype Nu :: Type -> Type -> Type where
    Nu :: forall s a. a -> Nu s a

deriving instance forall s a. Eq a => Eq (Nu s a)
deriving instance forall s a. Ord a => Ord (Nu s a)
deriving instance forall s a. Num a => Num (Nu s a)
deriving instance forall s a. Fractional a => Fractional (Nu s a)
deriving instance forall s a. Floating a => Floating (Nu s a)

{- | (Just for convenience in the REPL) -}
deriving instance forall s a. Show a => Show (Nu s a)

{- | Measure of the exponential distribution
on the nonnegative reals
 -}
nuMeasure :: forall s a. Floating a =>
    Nu s a -> -- ^ Start of the interval being measured
    Nu s a -> -- ^ End of the interval being measured
    Nu s a
nuMeasure = \ na na' ->
    exp (-na) - exp (-na')

{- | Our choice of utility function on the nonnegative reals -}
nuUtility :: forall s a. Floating a => Nu s a -> Nu s a
nuUtility = \ na ->
    log $ 1 + na

{- | Very slow due to the use of 'Math.Calculus.Simpson.integrate'! -}
instance forall s a. (Ord a, Floating a) => DRandVar s (Nu s a) where
    plt :: s -> Nu s a -> Nu s a
    plt = \ _ na -> case compare 0 na of
        GT -> 0
        _  -> Math.Calculus.Simpson.integrate 0.001 nuMeasure (const 1) 0 na

    mean :: s -> Nu s a
    mean = \ _ ->
        Math.Calculus.Simpson.integrate 0.001 nuMeasure nuUtility 0 10

    mlt :: s -> Nu s a -> Nu s a
    mlt = \ _ na -> case compare 0 na of
        GT -> 0
        _  -> Math.Calculus.Simpson.integrate 0.001 nuMeasure nuUtility 0 na


-- * Implementations of 'StratRep'

-- ** Strategies as threshold functions

{- | Representation of strategies as
threshold functions on dependent distributions with
__surely__ terminating state space recursion
and payoff \(0\) at the terminus
-}
newtype FunRep :: Type -> Type -> Type where
    FunRep :: forall s a. { unFunRep :: s -> a } -> FunRep s a

instance forall s a. (TransState s a, DRandVar s a) => StratRep (FunRep s a) s a where
    apply :: FunRep s a -> Strategy s a
    apply = \ (FunRep f) s a ->
        case compare (f s) a of
            GT -> Reject
            _  -> Accept

    thresh :: (s -> a) -> FunRep s a
    thresh = FunRep

    eval :: FunRep s a -> s -> a
    eval = \ x@(FunRep f) s ->
        let contrib = \ s' -> let ctf = f s' in
                mgt s ctf + plt s ctf * eval x s' 
        in  sum . fmap (uncurry (*) . fmap contrib) $ transition s


-- ** Strategies as memoized threshold functions

{- | Representation of strategies as 'HashMap'-memoized
threshold functions on dependent distributions with
__surely__ terminating state space recursion
and payoff \(0\) at the terminus
-}
data HashRep :: Type -> Type -> Type where
    HashRep :: forall s a. { unHashRep :: s -> a } -> HashRep s a

instance forall s a. (Hashable s, TransState s a, DRandVar s a) => StratRep (HashRep s a) s a where
    type MemoType (HashRep s a) = IORef (HashMap s a)

    memo :: MemoType (HashRep s a)
    memo = unsafeDupablePerformIO $ newIORef HashMap.empty

    apply :: HashRep s a -> Strategy s a
    apply = \ (HashRep f) s a ->
        case compare (f s) a of
            GT -> Reject
            _  -> Accept

    thresh :: (s -> a) -> HashRep s a
    thresh = HashRep

    eval :: HashRep s a -> s -> a
    eval =
        let plan :: HashMap s a -> HashSet s -> [s] -> s -> Pair (HashSet s) [s]
            plan = \ m0 m1 l s ->
                case HashMap.member s m0 || HashSet.member s m1 of
                    True  -> Pair m1 l
                    False -> foldl' (\ (Pair m1' l') (_, s') -> plan m0 m1' l' s') (Pair (HashSet.insert s m1) (s : l)) $ transition @_ @a s
        in  \ x s -> unsafeDupablePerformIO $ do
                let contrib = \ m s' s'' -> let ctf = fromJust $ HashMap.lookup s'' m in
                        mgt s' ctf + plt s' ctf * eval x s''
                    compute = \ s' -> do
                        m <- readIORef memo
                        let a' = sum . fmap (uncurry (*) . fmap (contrib m s')) $ transition s'
                        modifyIORef' memo $ HashMap.insert s' a'
                        pure a'
                m <- readIORef memo
                case plan m HashSet.empty [] s of
                    Pair _ []            -> pure . fromJust $ HashMap.lookup s m
                    Pair _ (sInit : ss') -> do
                        aInit <- compute sInit
                        foldlM (const compute) aInit ss'


-- * Demonstrations

{- |
> optStratMuAsFun = unFunRep optStrat

The optimal strategy for
the uniform distribution on the unit interval with utility(a) = a,
represented as a threshold function\;
is \(O(2^n)\) in time and space
due to the lack of memoization

==== __Demo__
>>> :set -XHaskell2010 -XTypeApplications -Wall
>>> :set +s
>>> mapM_ print . flip map [0..4] $ optStratMuAsFun @Double
Mu 0.5
Mu 0.625
Mu 0.6953125
Mu 0.741729736328125
Mu 0.7750815008766949
(0.01 secs, 833,336 bytes)
>>> :r
>>> mapM_ print . flip map [10..14] $ optStratMuAsFun @Double
Mu 0.8707450655319368
Mu 0.8790984845741084
Mu 0.886407072790247
Mu 0.8928587493462872
Mu 0.8985983731421081
(0.11 secs, 102,302,560 bytes)
>>> :r
>>> mapM_ print . flip map [20..24] $ optStratMuAsFun @Double
Mu 0.923096456398081
Mu 0.9260535339073471
Mu 0.9287875738311431
Mu 0.9313231786515705
Mu 0.9336814315468326
(86.59 secs, 104,019,460,968 bytes)
-}
optStratMuAsFun :: forall a. (Ord a, Fractional a) => TransNat -> Mu TransNat a
optStratMuAsFun = unFunRep optStrat

optStratMuAsHash :: forall a. (Ord a, Fractional a) => TransNat -> Mu TransNat a
optStratMuAsHash = unHashRep optStrat

{- |
> optStratNuAsFun = unFunRep optStrat

The optimal strategy for
the exponential distribution on the nonnegative reals
with utility(a) = log(1+a),
represented as a threshold function\;
is \(O(2^n)\) in time and space
due to the lack of memoization

==== __Demo__
>>> :set -XHaskell2010 -XTypeApplications -Wall
>>> :set +s
>>> mapM_ print . flip map [0..4] $ optStratNuAsFun @Float
Nu 0.5962349
Nu 0.7612457
Nu 0.8558962
Nu 0.91740966
Nu 0.96000737
(0.83 secs, 880,714,776 bytes)
>>> :r
>>> mapM_ print . flip map [5..9] $ optStratNuAsFun @Float
Nu 0.9905981
Nu 1.0129874
Nu 1.0296412
Nu 1.042242
Nu 1.0518706
(27.95 secs, 30,776,085,696 bytes)
-}
optStratNuAsFun :: forall a. (Ord a, Floating a) => TransNat -> Nu TransNat a
optStratNuAsFun = unFunRep optStrat

{- |
> optStratNuAsHash = unHashRep optStrat

The optimal strategy for
the exponential distribution on the nonnegative reals
with utility(a) = log(1+a),
represented as a threshold function\;
is \(O(n)\) in time and space
thanks to (impure!!) memoization

No demo in the repl due to
the polymorphism of 'memo'
causing it to be reinitialized repeatedly
instead of modified an a(n impure) top level state
-}
optStratNuAsHash :: forall a. (Ord a, Floating a) => TransNat -> Nu TransNat a
optStratNuAsHash = unHashRep optStrat