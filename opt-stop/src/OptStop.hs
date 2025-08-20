{-# LANGUAGE Haskell2010
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTSyntax
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , KindSignatures
  , LambdaCase
{-, MultiParamTypeClasses (redundant) -}
  , ScopedTypeVariables
  , StandaloneDeriving
  , TupleSections
  , TypeApplications
#-}

{-# OPTIONS_GHC
    -Wall
    -fno-cse
#-}

{- | Motivating optimal stopping via backward induction via 'Data.Function.fix' -}
module OptStop
  ( -- * Context
    -- ** Transitionable state spaces
      TransState
        ( transition )
    -- ** Dependent random variables
    , DRandVar
      ( plt
      , pgt
      , mean
      , mlt
      , mgt
      )
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
  -- * Implementations of 'DRandVar'
  -- ** Box problem distribution(s)
  , BoxState
  , BoxVal
      ( BoxVal
      , unBoxVal
      )
  -- ** Secretary problem distribution(s)
  , SecState
      ( SecState )
  , SecVal
      ( SecVal
      , unSecVal
      )
  -- ** Coin toss problem distribution(s)
  , CoinState
      ( CoinState )
  , CoinVal
      ( CoinVal
      , unCoinVal
      )
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
  , boxStratFun
  , boxStratHash
  , secStratFun
  , secStratHash
  , coinStratFun
  , coinStratHash
  ) where


-- From base ^>= 4.18:

import Prelude hiding
  ( fst
  , snd
  )

import Data.Kind
  ( Type )

import GHC.Generics
  ( Generic )

import Data.Function
  ( fix )

import Data.Foldable
  ( foldlM )

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


-- Misc.

{- | Pair type strict in both type arguments -}
data Pair :: Type -> Type -> Type where
    Pair :: forall a0 a1. { _fst :: !a0, snd :: !a1 } -> Pair a0 a1


-- * Context

-- ** State spaces with nondeterministic transition

{- | Interface to a type @s@ of states
equipped with a nondeterministic 'transition' function
with transition probabilities of some type @a@
-}
class TransState s a where
    transition :: s -> [(a, s)]


-- ** Dependent random variables

{- | Interface to a dependent (on some parameter \(s\))
random variable realized as a dependent probability measure
\(\mu_s\) on \(\mathbb{R}\) representing a payoff\;
should satisfy

[nonnegativity of 'plt'] @forall (s :: s) (a :: a). plt s a >= 0@

[nonnegativity of 'pgt'] @forall (s :: s) (a :: a). pgt s a >= 0@

[normalization] @forall (s :: s) (a :: a). plt s a + pgt s a = 1@

[coherence] @forall (s :: s) (a :: a). mlt s a + mgt s a = mean s@

(the latter two laws being automatic when
precisely a minimal subset of methods is implemented)
-}
class (Ord a, Num a) => DRandVar s a where
    {-# MINIMAL (plt | pgt), ((mean, mlt) | (mean, mgt) | (mlt, mgt)) #-}

    {- | Dependent probability that a sampled value
    is less than the argument\; if \(x\) is said argument,
    then \(\int_{t\ \colon\ \left(-\infty,x\right)}\ \text{d}\mu_s\)
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
    \(\int_{t\ \colon\ \left(-\infty,\infty\right)}t\ \text{d}\mu_s\)
    -}
    mean :: s -> a
    mean = \ s ->
        mlt s 0 + mgt s 0

    {- | Dependent contribition to the expectation
    by values less than the argument\;
    if \(x\) is said argument, then
    \(\int_{t\ \colon\ \left(-\infty,x\right)}t\ \text{d}\mu_s\)
    -}
    mlt :: s -> a -> a
    mlt = \ s a ->
        mean s - mgt s a

    {- | Dependent contribition to the expectation
    by values greater than __or equal to__ the argument\;
    if \(x\) is said argument, then
    \(\int_{t\ \colon\ \left[x,\infty\right)}t\ \text{d}\mu_s\)
    -}
    mgt :: s -> a -> a
    mgt = \ s a ->
        mean s - mlt s a


-- ** Strategy representations

{- | Type representing at a given phase of the game
either 'Accept'ing  or 'Reject'ing the current payoff
-}
data Choice where
    Accept, Reject :: Choice

deriving instance Show Choice -- /Just for convenience in the REPL/

{- | Type encoding the fully abstract notion of a \"strategy\"\;
in practice we (can) only work with it through its representations
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


-- * Generic solution

-- ** Generic optimal stopping

{- |
> bootstrap = thresh . eval

Key \"bootstrapping\" function that,
from the strategy represented by the argument,
constructs a strategy with at least as great expectation
(contingent upon the relevant 'DRandVar' instance having
monotonic payoff functions as stipulated)
-}
bootstrap :: forall x s a.
    (DRandVar s a, StratRep x s a) => x -> x
bootstrap = thresh . eval

{- |
> optStrat = fix bootstrap

Generic optimal strategy (representation) as
the unique 'Data.Function.fix'ed point of 'bootstrap'
(contingent upon the relevant 'DRandVar' instance having
monotonic payoff functions as stipulated)
-}
optStrat :: forall x s a.
    (DRandVar s a, StratRep x s a) => x
optStrat = fix bootstrap


-- * Implementations of 'DRandVar'

-- ** Box problem distribution(s)

{- | Type of natural numbers equipped with
the standard terminating (deterministic)
recursion as its transition function
-}
data BoxState where
    BoxState ::
        !Word -> -- ^ Remaining samples from the box (not including the present one)
        BoxState

deriving instance Generic BoxState
deriving instance Eq BoxState
instance Hashable BoxState -- /Resolved via Generic BoxState/
deriving instance Show BoxState -- /Just for convenience in the REPL/

instance forall a. Num a => TransState BoxState a where
    transition :: BoxState -> [(a, BoxState)]
    transition = \case
        BoxState 0 -> []
        BoxState n -> pure . (1,) . BoxState $ n - 1

{- | Uniform distributions on the unit interval
independent of their first type parameter @s@
-}
newtype BoxVal :: Type -> Type -> Type where
    BoxVal :: forall s a. { unBoxVal :: a } -> BoxVal s a

deriving instance forall s a. Eq a => Eq (BoxVal s a)
deriving instance forall s a. Ord a => Ord (BoxVal s a)
deriving instance forall s a. Num a => Num (BoxVal s a)
deriving instance forall s a. Fractional a => Fractional (BoxVal s a)
deriving instance forall s a. Floating a => Floating (BoxVal s a) -- /Not used here/
deriving instance forall s a. Show a => Show (BoxVal s a) -- /Just for convenience in the REPL/

instance forall s a. (Ord a, Fractional a) => DRandVar s (BoxVal s a) where
    plt :: s -> BoxVal s a -> BoxVal s a
    plt = \ _ msa -> case (compare 0 msa, compare 1 msa) of
        (GT, _ ) -> 0
        (_ , LT) -> 1
        _        -> msa

    mean :: s -> BoxVal s a
    mean = \ _ ->
        1 / 2

    mlt :: s -> BoxVal s a -> BoxVal s a
    mlt = \ _ msa -> case (compare 0 msa, compare 1 msa) of
        (GT, _ ) -> 0
        (_ , LT) -> 1 / 2
        _        -> msa * msa / 2


-- ** Secretary problem distribution

{- | Type of pairs of naturals representing
the total number of candidates under consideration
and those remaining (not including
the present candidate being considered)
respectively
-}
data SecState where
    SecState ::
        !Word -> -- ^ Total number of secretaries considered
        !Word -> -- ^ Remaining secretaries to consider (not including the present one)
        SecState

deriving instance Generic SecState
deriving instance Eq SecState
instance Hashable SecState -- /Resolved via Generic SecState/
deriving instance Show SecState -- /Just for convenience in the REPL/

instance forall a. Fractional a => TransState SecState a where
    transition :: SecState -> [(a, SecState)]
    transition = \case
        SecState _ 0 -> []
        SecState n r -> pure . (1,) . SecState n $ r - 1 

{- | Dependent probability distribution from
[the secretary problem](https://en.wikipedia.org/wiki/Secretary_problem)
where the payoff is \(1\) if the best candidate is selected
and \(0\) otherwise\; if \(r\) candidates
(not including the one presently being considered)
remain to be seen, then the probability that
the present candidate exceeds all prior candidates
is \(\tfrac{1}{n-r}\),
and in that case the conditional expected payoff
of selecting the present candidate is \(\tfrac{n-r}{n}\).
-}
newtype SecVal :: Type -> Type -> Type where
    SecVal :: forall s a. { unSecVal :: a } -> SecVal s a

deriving instance forall s a. Eq a => Eq (SecVal s a)
deriving instance forall s a. Ord a => Ord (SecVal s a)
deriving instance forall s a. Num a => Num (SecVal s a)
deriving instance forall s a. Fractional a => Fractional (SecVal s a)
deriving instance forall s a. Floating a => Floating (SecVal s a)  -- /Not used here/
deriving instance forall s a. Show a => Show (SecVal s a) -- /Just for convenience in the REPL/

instance forall a. (Ord a, Fractional a) => DRandVar SecState (SecVal SecState a) where
    plt :: SecState -> SecVal SecState a -> SecVal SecState a
    plt = \ (SecState n r) xsa -> case (compare 0 xsa, compare (fromIntegral (n - r) / fromIntegral n) xsa) of
        (GT, _ ) -> 0
        (_ , GT) -> (1 -) . recip . fromIntegral $ n - r
        _        -> 1

    mean :: SecState -> SecVal SecState a
    mean = \ (SecState n _) -> recip $ fromIntegral n

    mlt :: SecState -> SecVal SecState a -> SecVal SecState a
    mlt = \ (SecState n r) xsa -> case compare (fromIntegral (n - r) / fromIntegral n) xsa of
        GT -> 0
        _  -> recip $ fromIntegral n


-- ** Coin toss problem distribution

{- | TO WRITE! -}
data CoinState where
    CoinState ::
        !Word -> -- ^ Coins flipped thus far
        !Word -> -- ^ Coins remaining to flip
        !Word -> -- ^ Count of heads among coins flipped
        CoinState

deriving instance Generic CoinState
deriving instance Eq CoinState
instance Hashable CoinState -- /Obtained from Generic SecState/
deriving instance Show CoinState -- /Just for convenience in the REPL/

instance forall a. Fractional a => TransState CoinState a where
    transition :: CoinState -> [(a, CoinState)]
    transition = \case
        CoinState _ 0 _ -> []
        CoinState q r s ->
            [ (1 / 2,) . CoinState (q + 1) (r - 1) $ s
            , (1 / 2,) . CoinState (q + 1) (r - 1) $ s + 1
            ]

{- | TO WRITE! -}
newtype CoinVal :: Type -> Type -> Type where
    CoinVal :: forall s a. { unCoinVal :: a } -> CoinVal s a

deriving instance forall s a. Eq a => Eq (CoinVal s a)
deriving instance forall s a. Ord a => Ord (CoinVal s a)
deriving instance forall s a. Num a => Num (CoinVal s a)
deriving instance forall s a. Fractional a => Fractional (CoinVal s a)
deriving instance forall s a. Floating a => Floating (CoinVal s a) -- /Not used here/
deriving instance forall s a. Show a => Show (CoinVal s a) -- /Just for convenience in the REPL/

instance forall a. (Ord a, Fractional a) => DRandVar CoinState (CoinVal CoinState a) where
    plt :: CoinState -> CoinVal CoinState a -> CoinVal CoinState a
    plt = \ (CoinState q _ s) psa -> case compare (fromIntegral s / fromIntegral q) psa of
        GT -> 0
        _  -> 1

    mean :: CoinState -> CoinVal CoinState a
    mean = \ (CoinState q _ s) -> fromIntegral s / fromIntegral q

    mlt :: CoinState -> CoinVal CoinState a -> CoinVal CoinState a
    mlt = \ (CoinState q _ s) psa -> case compare (fromIntegral s / fromIntegral q) psa of
        GT -> 0
        _  -> fromIntegral s / fromIntegral q


-- * Implementations of 'StratRep'

-- ** Strategies as threshold functions

{- | Representation of strategies as
threshold functions on dependent distributions with
__surely__ terminating state space transition
and payoff \(0\) at the terminus

Generally exponentially inefficient
due to the absence of memoization!
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
__surely__ terminating state space transition
and payoff \(0\) at the terminus

Calls 'Data.Maybe' on the value
of a 'Data.IORef.IORef' updated
via a polymorphic use of 'unsafePerformIO',
hence 'eval' and functions calling it
crash in the REPL (but work, at least
as far as I can tell, in compiled code)
-}
data HashRep :: Type -> Type -> Type where
    HashRep :: forall s a. { unHashRep :: s -> a, _memo :: !(IORef (HashMap s a)) } -> HashRep s a

instance forall s a. (Hashable s, TransState s a, DRandVar s a) => StratRep (HashRep s a) s a where
    apply :: HashRep s a -> Strategy s a
    apply = \ (HashRep f _) s a ->
        case compare (f s) a of
            GT -> Reject
            _  -> Accept

    thresh :: (s -> a) -> HashRep s a
    thresh = flip HashRep . unsafeDupablePerformIO $ newIORef HashMap.empty

    eval :: HashRep s a -> s -> a
    eval = \ x@(HashRep _ rm) s -> unsafeDupablePerformIO $ do
        m <- readIORef rm
        let -- | Backward rose tree DFS to determine order of subevaluations of @eval x@
            -- (said order is the reverse of the dependency order, modulo redundancy)
            plan :: HashSet s -> [s] -> s -> Pair (HashSet s) [s]
            plan = \ n l s' ->
                case HashMap.member s' m || HashSet.member s' n of
                    True  -> Pair n l
                    False ->
                        foldl'
                            ( \ (Pair n' l') (_, s'') -> plan n' l' s'' )
                            ( Pair (HashSet.insert s' n) (s' : l) )
                            ( transition @_ @a s' )
            -- | Subevaluations of @eval x@ on dependencies @s'@ of @s@ as 'IO' actions to be monadically (left-)folded
            eval' :: s -> IO a
            eval' = \ s' -> do
                m' <- readIORef rm
                let contrib = \ s'' ->
                        let ctf = fromJust $ HashMap.lookup s'' m'
                        in  mgt s' ctf + plt s' ctf * eval x s''
                    a' = sum . fmap (uncurry (*) . fmap contrib) $ transition s'
                modifyIORef' rm $ HashMap.insert s' a'
                pure a'
        case snd $ plan HashSet.empty [] s of
            []          -> pure . fromJust $ HashMap.lookup s m
            sInit : ss' -> do
                aInit <- eval' sInit
                foldlM (const eval') aInit ss'


-- * Demonstrations

{- |
> boxStratFun = unFunRep optStrat

The optimal strategy for the box problem
represented as a threshold function\;
is \(O(2^n)\) in time and space
due to the lack of memoization

==== __Demo__
>>> :set -XHaskell2010 -XTypeApplications -Wall +s
>>> mapM_ print . flip fmap [0..4] $ boxStratFun @Double
Nu 0.0
Nu 0.5
Nu 0.625
Nu 0.6953125
Nu 0.741729736328125
(0.00 secs, 851,352 bytes)
>>> :r
>>> mapM_ print . flip fmap [10..14] $ boxStratFun @Double
Nu 0.861098212205712
Nu 0.8707450655319368
Nu 0.8790984845741084
Nu 0.886407072790247
Nu 0.8928587493462872
(0.14 secs, 158,961,688 bytes)
>>> :r
>>> mapM_ print . flip fmap [20..24] $ boxStratFun @Double
Nu 0.9198874457215742
Nu 0.923096456398081
Nu 0.9260535339073471
Nu 0.9287875738311431
Nu 0.9313231786515705
(120.52 secs, 162,009,936,832 bytes)
-}
boxStratFun :: forall a. (Ord a, Fractional a) => BoxState -> BoxVal BoxState a
boxStratFun = unFunRep optStrat

{- |
> boxStratHash = unHashRep optStrat

The optimal strategy for the box problem
represented as a threshold function\;
is \(O(n)\) in time and space(!)
thanks to (impure!!) memoization
-}
boxStratHash :: forall a. (Ord a, Fractional a) => BoxState -> BoxVal BoxState a
boxStratHash = unHashRep optStrat

secStratFun :: forall a. (Ord a, Fractional a) => SecState -> SecVal SecState a
secStratFun = unFunRep optStrat

secStratHash :: forall a. (Ord a, Fractional a) => SecState -> SecVal SecState a
secStratHash = unHashRep optStrat

coinStratFun :: forall a. (Ord a, Fractional a) => CoinState -> CoinVal CoinState a
coinStratFun = unFunRep optStrat

coinStratHash :: forall a. (Ord a, Fractional a) => CoinState -> CoinVal CoinState a
coinStratHash = unHashRep optStrat