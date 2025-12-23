{-# LANGUAGE Haskell2010
  , DeriveGeneric
  , DerivingStrategies
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
#-}

{-# OPTIONS_GHC
    -Wall
    -fno-cse
    -fno-full-laziness
#-}

{- | Motivating optimal stopping via backward induction via 'Data.Function.fix'
    and implementing it as such for three well-known optimal stopping problems
-}
module OptStop
  ( -- * Context
    -- ** Time-homogeneous Markov chains
      Markov
        ( transition )
    , recurse
    , induct
    -- ** Dependent random variables
    , DRandVar
      ( plt
      , pge
      , mean
      , mlt
      , mge
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
      ( BoxState )
  , BoxVal
  -- ** Secretary problem distribution(s)
  , SecState
      ( SecState )
  , SecVal
  -- ** Coin toss problem distribution(s)
  , CoinState
      ( CoinState )
  , CoinVal
  -- * Implementations of 'StratRep'
  -- ** Strategies as threshold functions
  , FunRep
      ( FunRep
      , unFunRep
      )
  -- ** Strategies as memoized threshold functions    
  , MemoRep
      ( MemoRep
      , unMemoRep
      , _appMemo
      , _evalMemo
      )
  , clearMemo
  -- * Demonstrations
  , boxStratFun
  , boxStratMemo
  , secStratFun
  , secStratMemo
  , coinStratFun
  , coinStratMemo
  ) where


-- + Imports

-- ++ From base >= 4.18 && < 4.22

import Data.Kind
  ( Type )

import GHC.Generics
  ( Generic )

import Data.Function
  ( fix )

import Data.Bifunctor
  ( Bifunctor
  , second
  )

import Data.Foldable
  ( traverse_ )

import Data.Maybe
  ( fromMaybe
  , fromJust
  )

import Data.IORef
  ( IORef
  , newIORef
  , writeIORef
  , modifyIORef'
  , readIORef
  )

import System.IO.Unsafe
  ( unsafeDupablePerformIO )


-- ++ From hashable ^>= 1.5 and unordered-containers ^>= 0.2

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

import qualified Data.HashSet as HashSet
  ( empty
  , insert
  , member
  )


-- ++ From transformers ^>= 0.6

import Control.Monad.Trans.State.Lazy
    ( evalState
    , modify'
    , get
    )


-- _ Misc. util

sndmap :: forall t a b0 b1.
    Bifunctor t =>
    (b0 -> b1) -> t a b0 -> t a b1
sndmap = second


-- * Context

-- ** Time-homogeneous Markov chains

{- | Interface to a type @s@ of states
    equipped with a nondeterministic 'transition' function
    taking transition probabilities of some type @a@

    Injective in the type argument @s@ (in order that
    'recurse' and 'induct' below be unambiguously-typed)
-}
class Markov s a | s -> a where
    {- | Transition function of time-homogeneous Markov chain -}
    transition :: s -> [(a, s)]

{- | Contingencies of the argument with respect to
   backward induction on the 'Markov' instance
-}
recurse :: forall s a.
    Markov s a =>
    s -> [s]
recurse = fmap snd . transition

{- | Construction of, given (as the first argument) a recursion
    (prototypically @recurse@) on @s@ and (as the second)
    an initial state in @s@,
    a resulting nonredundant 'Data.List.List' of states
    with the properties that the contingencies of the initial state
    with respect to said recursion are in said result and moreover 
    that any state in said result it itself preceded therein
    by its contingencies\; non-terminating
    if no 'Data.List.List' with these properties exists

    (Implemented as rose tree DFS caching reached states
    in a 'Data.HashSet.HashSet'; it can be viewed a fused fold
    in the 'Control.Monad.Trans.State.Lazy' monad
    of the rose tree obtained by unfolding the arguments.)
-}
induct :: forall s.
    (Eq s, Hashable s) =>
    (s -> [s]) -> s -> [s]
induct = \ r ->
    let inductR = \case
            []      -> pure mempty
            s : ss' -> do
                cs <- get
                case HashSet.member s cs of
                    True  -> inductR ss'
                    False -> do
                        xcss' <- inductR $ r s
                        modify' (HashSet.insert s)
                        xcss'' <- inductR ss'
                        pure $ xcss' <> pure s <> xcss''
    in  flip evalState HashSet.empty . inductR . r


-- ** Dependent random variables

{- | Interface to a dependent (on some parameter \(s\))
    random variable realized as a dependent probability measure
    \(\mu_s\) on \(\mathbb{R}\) representing a payoff\;
    should satisfy

    [nonnegativity of 'plt'] @forall (s :: s) (a :: a). plt s a >= 0@

    [nonnegativity of 'pge'] @forall (s :: s) (a :: a). pge s a >= 0@

    [normalization] @forall (s :: s) (a :: a). plt s a + pge s a = 1@

    [coherence] @forall (s :: s) (a :: a). mlt s a + mge s a = mean s@

    (the latter two laws being automatic when
    a MINIMAL subset of methods is implemented)
-}
class (Ord a, Num a) => DRandVar s a where
    {-# MINIMAL (plt | pge), ((mean, mlt) | (mean, mge) | (mlt, mge)) #-}

    {- | Dependent probability that a sampled value
        is less than the argument\; if \(x\) is said argument,
        then \(\int_{t\ \colon\ \left(-\infty,x\right)}\ \text{d}\mu_s\)
    -}
    plt :: s -> a -> a
    plt = \ s a ->
        1 - pge s a

    {- | Dependent probability that a sampled value
        is greater than __or equal to__ the argument\; if \(x\) is said argument,
        then \(\int_{t\ \colon\ \left[x,\infty\right)}\ \text{d}\mu_s\)
    -}
    pge :: s -> a -> a
    pge = \ s a ->
        1 - plt s a

    {- | Dependent expectation of the distribution, i.e.
        \(\int_{t\ \colon\ \left(-\infty,\infty\right)}t\ \text{d}\mu_s\)
    -}
    mean :: s -> a
    mean = \ s ->
        mlt s 0 + mge s 0

    {- | Dependent contribition to the expectation
        by values less than the argument\;
        if \(x\) is said argument, then
        \(\int_{t\ \colon\ \left(-\infty,x\right)}t\ \text{d}\mu_s\)
    -}
    mlt :: s -> a -> a
    mlt = \ s a ->
        mean s - mge s a

    {- | Dependent contribition to the expectation
        by values greater than __or equal to__ the argument\;
        if \(x\) is said argument, then
        \(\int_{t\ \colon\ \left[x,\infty\right)}t\ \text{d}\mu_s\)
    -}
    mge :: s -> a -> a
    mge = \ s a ->
        mean s - mlt s a


-- ** Strategy representations

{- | Type representing at a given phase of the game
    either 'Accept'ing  or 'Reject'ing the current payoff
-}
data Choice where
    Accept, Reject :: Choice

deriving stock instance Show Choice -- /Just for convenience in the REPL/

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

    [coherence] @forall (t :: s -> a) (s :: s) (a :: a). apply (thresh t) s a = case compare (t s) a of GT -> Reject; _ -> Accept@

    Injective in the type argument @x@ (in order that
    'bootstrap' and 'optStrat' below be unambiguously-typed)
-}
class DRandVar s a => StratRep x s a | x -> s, x -> a where
    {- | Unpacking of the argument
        into the strategy it represents\;
        not actually used below, but pedagogically useful
        to have/implement anyway
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
-}
bootstrap :: forall x s a.
    (DRandVar s a, StratRep x s a) => x -> x
bootstrap = thresh . eval

{- |
    > optStrat = fix bootstrap

    Generic optimal strategy (representation) as
    the unique 'Data.Function.fix'ed point of 'bootstrap'
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
data BoxState :: Type -> Type where
    BoxState :: forall a.
        !Word -> -- ^ Remaining samples from the box (not including the one in hand)
        BoxState a

deriving stock instance forall a. Generic (BoxState a)
deriving stock instance forall a. Eq (BoxState a)
instance forall a. Hashable (BoxState a) -- /Resolved via Generic BoxState/
deriving stock instance forall a. Show (BoxState a) -- /Just for convenience in the REPL/

instance forall a. Num a => Markov (BoxState a) a where
    transition :: BoxState a -> [(a, BoxState a)]
    transition = \case
        BoxState 0 -> []
        BoxState n -> pure . (1,) . BoxState $ n - 1

{- | Uniform distributions on the unit interval
    (independent of their first type parameter @s@)
-}
newtype BoxVal :: Type -> Type where
    BoxVal :: forall a. a -> BoxVal a

deriving newtype instance forall a. Eq a => Eq (BoxVal a)
deriving newtype instance forall a. Ord a => Ord (BoxVal a)
deriving newtype instance forall a. Num a => Num (BoxVal a)
deriving newtype instance forall a. Fractional a => Fractional (BoxVal a)
deriving newtype instance forall a. Floating a => Floating (BoxVal a) -- /Not used here/
deriving stock instance forall a. Show a => Show (BoxVal a) -- /Just for convenience in the REPL/

instance forall s a. (Ord a, Fractional a) => DRandVar s (BoxVal a) where
    plt :: s -> BoxVal a -> BoxVal a
    plt = \ _ ba -> case (compare 0 ba, compare 1 ba) of
        (GT, _ ) -> 0
        (_ , LT) -> 1
        _        -> ba

    mean :: s -> BoxVal a
    mean = \ _ ->
        1 / 2

    mlt :: s -> BoxVal a -> BoxVal a
    mlt = \ _ ba -> case (compare 0 ba, compare 1 ba) of
        (GT, _ ) -> 0
        (_ , LT) -> 1 / 2
        _        -> ba * ba / 2


-- ** Secretary problem distribution

{- | Type of pairs of naturals representing
    the number of candidates so far considered
    and those remaining (not including the one in hand)
    respectively
-}
data SecState :: Type -> Type where
    SecState :: forall a.
        !Word -> -- ^ Secretaries considered thus far (including the one in hand)
        !Word -> -- ^ Remaining secretaries to consider (not including the one in hand)
        SecState a

deriving stock instance forall a. Generic (SecState a)
deriving stock instance forall a. Eq (SecState a)
instance forall a. Hashable (SecState a) -- /Resolved via Generic SecState/
deriving stock instance Show (SecState a) -- /Just for convenience in the REPL/

instance forall a. Fractional a => Markov (SecState a) a where
    transition :: SecState a -> [(a, SecState a)]
    transition = \case
        SecState _ 0 -> []
        SecState s r -> pure . (1,) $ SecState (s + 1) (r - 1) 

{- | Dependent probability distribution from
    [the secretary problem](https://en.wikipedia.org/wiki/Secretary_problem)
    where the payoff is \(1\) if the best candidate is selected
    and \(0\) otherwise\; if \(s\) candidates have been seen
    (including the present one) and \(r\) remain to be considered,
    then the probability that the present candidate
    exceeds all prior candidates is \(\tfrac{1}{s}\),
    and in that case the conditional expected payoff
    of selecting the present candidate is \(\tfrac{s}{s+r}\).
-}
newtype SecVal :: Type -> Type where
    SecVal :: forall a. a -> SecVal a

deriving newtype instance forall a. Eq a => Eq (SecVal a)
deriving newtype instance forall a. Ord a => Ord (SecVal a)
deriving newtype instance forall a. Num a => Num (SecVal a)
deriving newtype instance forall a. Fractional a => Fractional (SecVal a)
deriving newtype instance forall a. Floating a => Floating (SecVal a)  -- /Not used here/
deriving newtype instance forall a. Show a => Show (SecVal a) -- /Just for convenience in the REPL/

instance forall a. (Ord a, Fractional a) => DRandVar (SecState (SecVal a)) (SecVal a) where
    plt :: SecState (SecVal a) -> SecVal a -> SecVal a
    plt = \ (SecState s r) sa -> case (compare 0 sa, compare (fromIntegral s / fromIntegral (s + r)) sa) of
        (GT, _ ) -> 0
        (_ , GT) -> (1 -) . recip $ fromIntegral s
        _        -> 1

    mean :: SecState (SecVal a) -> SecVal a
    mean = \ (SecState s r) -> recip . fromIntegral $ s + r

    mlt :: SecState (SecVal a) -> SecVal a -> SecVal a
    mlt = \ (SecState s r) sa -> case compare (fromIntegral s / fromIntegral (s + r)) sa of
        GT -> 0
        _  -> recip . fromIntegral $ s + r


-- ** Coin toss problem distribution

{- | Type of triples of naturals representing
    the number of coins thus far flipped,
    the number of coins remaining to flip,
    and the count of heads among those coins flipped
-}
data CoinState :: Type -> Type where
    CoinState :: forall a.
        !Word -> -- ^ Coins flipped thus far
        !Word -> -- ^ Coins remaining to flip
        !Word -> -- ^ Count of heads among coins flipped
        CoinState a

deriving stock instance forall a. Generic (CoinState a)
deriving stock instance forall a. Eq (CoinState a)
instance forall a. Hashable (CoinState a) -- /Obtained from Generic SecState/
deriving stock instance Show (CoinState a) -- /Just for convenience in the REPL/

instance forall a. Fractional a => Markov (CoinState a) a where
    transition :: CoinState a -> [(a, CoinState a)]
    transition = \case
        CoinState _ 0 _ -> []
        CoinState q r s ->
            [ (1 / 2,) . CoinState (q + 1) (r - 1) $ s
            , (1 / 2,) . CoinState (q + 1) (r - 1) $ s + 1
            ]

{- | Dependent probability distribution from
    [the coin tossing problem](https://en.wikipedia.org/wiki/Optimal_stopping#Coin_tossing)
    where the payoff is the proportion of coin flips
    resulting in heads prior to exiting or running out.
-}
newtype CoinVal :: Type -> Type where
    CoinVal :: forall a. a -> CoinVal a

deriving newtype instance forall a. Eq a => Eq (CoinVal a)
deriving newtype instance forall a. Ord a => Ord (CoinVal a)
deriving newtype instance forall a. Num a => Num (CoinVal a)
deriving newtype instance forall a. Fractional a => Fractional (CoinVal a)
deriving newtype instance forall a. Floating a => Floating (CoinVal a) -- /Not used here/
deriving newtype instance forall a. Show a => Show (CoinVal a) -- /Just for convenience in the REPL/

instance forall a. (Ord a, Fractional a) => DRandVar (CoinState (CoinVal a)) (CoinVal a) where
    plt :: CoinState (CoinVal a) -> CoinVal a -> CoinVal a
    plt = \ (CoinState q _ s) ca -> case compare (fromIntegral s / fromIntegral q) ca of
        GT -> 0
        _  -> 1

    mean :: CoinState (CoinVal a) -> CoinVal a
    mean = \ (CoinState q _ s) -> case compare 0 q of
        LT -> fromIntegral s / fromIntegral q
        _  -> 0

    mlt :: CoinState (CoinVal a) -> CoinVal a -> CoinVal a
    mlt = \ (CoinState q _ s) ca -> case compare (fromIntegral s / fromIntegral q) ca of
        GT -> 0
        _  -> fromIntegral s / fromIntegral q


-- * Implementations of 'StratRep'

-- ** Strategies as threshold functions

{- | Representation of strategies as
    threshold functions on dependent distributions
    with __surely__ terminating state space transition
    and payoff \(0\) at the terminus

    Generally exponentially inefficient
    due to the absence of memoization in 'eval'!
-}
newtype FunRep :: Type -> Type -> Type where
    FunRep :: forall s a. { unFunRep :: s -> a } -> FunRep s a

instance forall s a. (Markov s a, DRandVar s a) => StratRep (FunRep s a) s a where
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
                mge s ctf + plt s ctf * eval x s'
        in  sum . fmap (uncurry (*) . sndmap contrib) $ transition s


-- ** Strategies as memoized threshold functions

{- | Representation of strategies as impurely memoized
    threshold functions on dependent distributions with
    __surely__ terminating state space transition
    and payoff \(0\) at the terminus

    Generally efficient across applications as 'eval'
    calls 'Data.Maybe.fromJust' on the value
    of two 'Data.IORef.IORef's
    updated via 'unsafeDupablePerformIO\;
    convenience functions are given below to clear
    their cached values as desired
-}
data MemoRep :: Type -> Type -> Type where
    MemoRep :: forall s a. {
        unMemoRep :: s -> a ,
        _appMemo :: !(IORef (HashMap s a)) ,
        _evalMemo :: !(IORef (HashMap s a)) } ->
        MemoRep s a

instance forall s a. (Hashable s, Markov s a, DRandVar s a) => StratRep (MemoRep s a) s a where
    {-# NOINLINE apply #-}
    apply :: MemoRep s a -> Strategy s a
    apply = \ (MemoRep f rm0 _) s ->
        let a0 = unsafeDupablePerformIO $ fromMaybe (f s) . HashMap.lookup s <$> readIORef rm0
        in  \ a -> case compare a0 a of
                GT -> Reject
                _  -> Accept

    {-# NOINLINE thresh #-}
    thresh :: (s -> a) -> MemoRep s a
    thresh = \ f -> unsafeDupablePerformIO $ do
        rm0 <- newIORef HashMap.empty
        rm1 <- newIORef HashMap.empty
        pure $ MemoRep f rm0 rm1

    {-# NOINLINE eval #-}
    eval :: MemoRep s a -> s -> a
    eval = \ (MemoRep f rm0 rm1) ->
        let eval' = \ s -> do
                msa0 <- readIORef rm0
                msa1 <- readIORef rm1
                flip fromMaybe (pure <$> HashMap.lookup s msa1) $ do
                    let contrib = \ s' ->
                            let ctf = fromJust $ HashMap.lookup s' msa0
                                ctg = fromJust $ HashMap.lookup s' msa1
                            in  mge s ctf + plt s ctf * ctg
                        a' = sum . fmap (uncurry (*) . sndmap contrib) $ transition s
                    modifyIORef' rm1 . HashMap.insert s $ a'
                    modifyIORef' rm0 . HashMap.insert s $ f s
                    pure a'
        in  \ s -> unsafeDupablePerformIO $ do
                ms0 <- readIORef rm0
                ms1 <- readIORef rm1
                let cached = \ s' -> HashMap.member s' ms0 && HashMap.member s' ms1             
                traverse_ eval' $ induct (filter (not . cached) . recurse) s
                eval' s

{- | Manually clears memoized values from a 'MemoRep'\;
    unsafe if used concurrently with 'eval'
    due to the 'fromJust' coercions in the latter
    that assume the presence of certain priorly memoized values.
-}
{-# NOINLINE clearMemo #-}
clearMemo :: forall s a. MemoRep s a -> ()
clearMemo = \ (MemoRep _ rm0 rm1) -> unsafeDupablePerformIO $ do
    writeIORef rm0 HashMap.empty
    writeIORef rm1 HashMap.empty
    pure ()


-- * Demonstrations

{- |
    > boxStratFun = unFunRep optStrat

    The optimal strategy for the box problem
    represented as a threshold function\;
    is \(O(2^n)\) in time and space
    due to the lack of memoization

==== __Demo__
>>> :set -XHaskell2010 -XTypeApplications -Wall +s
>>> mapM_ print . flip fmap [0..4] $ boxStratFun @Double . BoxState
BoxVal 0.0
BoxVal 0.5
BoxVal 0.625
BoxVal 0.6953125
BoxVal 0.741729736328125
(0.01 secs, 837,552 bytes)
>>> :r
>>> mapM_ print . flip fmap [10..14] $ boxStratFun @Double . BoxState
BoxVal 0.861098212205712
BoxVal 0.8707450655319368
BoxVal 0.8790984845741084
BoxVal 0.886407072790247
BoxVal 0.8928587493462872
(0.15 secs, 132,562,672 bytes)
>>> :r
>>> mapM_ print . flip fmap [20..24] $ boxStratFun @Double . BoxState
BoxVal 0.9198874457215742
BoxVal 0.923096456398081
BoxVal 0.9260535339073471
BoxVal 0.9287875738311431
BoxVal 0.9313231786515705
(114.77 secs, 134,965,076,632 bytes)
-}
boxStratFun :: forall a. (Ord a, Fractional a) => BoxState (BoxVal a) -> BoxVal a
boxStratFun = unFunRep optStrat

{- |
    > boxStratMemo = unMemoRep optStrat

    The optimal strategy for the box problem
    represented as a threshold function\;
    is \(O(n^2)\) in time and \(O(n)\) in space
    thanks to (impure!) memoization

==== __Demo__
>>> :set -XHaskell2010 -XTypeApplications -Wall +s
>>> mapM_ print . flip fmap [0..4] $ boxStratFun @Double . BoxState
BoxVal 0.0
BoxVal 0.5
BoxVal 0.625
BoxVal 0.6953125
BoxVal 0.741729736328125
(0.00 secs, 794,312 bytes)
>>> :r
>>> mapM_ print . flip fmap [10..14] $ boxStratFun @Double . BoxState
BoxVal 0.861098212205712
BoxVal 0.8707450655319368
BoxVal 0.8790984845741084
BoxVal 0.886407072790247
BoxVal 0.8928587493462872
(0.00 secs, 990,288 bytes)
>>> :r
>>> mapM_ print . flip fmap [20..24] $ boxStratFun @Double . BoxState
BoxVal 0.9980172182771205
BoxVal 0.9980191839888009
BoxVal 0.998021145804836
BoxVal 0.9980231037367988
BoxVal 0.9980250577962165
(0.04 secs, 43,967,864 bytes)
-}
boxStratMemo :: forall a. (Ord a, Fractional a) => BoxState (BoxVal a) -> BoxVal a
boxStratMemo = unMemoRep optStrat

{- |
    > secStratFun = unFunRep optStrat

    The optimal strategy for the secretary problem
    represented as a threshold function\;
    is \(O(2^n)\) in time and space
    due to the lack of memoization

==== __Demo__
>>> :set -XHaskell2010 -XTypeApplications -Wall +s
>>> mapM_ print . flip fmap [0..4] $ secStratFun @Double . (\ s -> SecState s $ 5 - s)
0.3666666666666667
0.3666666666666667
0.3666666666666667
0.33333333333333337
0.2
(0.00 secs, 1,100,936 bytes)
>>> :r
>>> mapM_ print . flip fmap [0,2..8] $ secStratFun @Double . (\ s -> SecState s $ 10 - s)
0.36535714285714294
0.36535714285714294
0.36535714285714294
0.3172619047619048
0.1875
(0.01 secs, 9,270,304 bytes)
>>> :r
>>> mapM_ print . flip fmap [0,4..16] $ secStratFun @Double . (\ s -> SecState s $ 20 - s)
0.3657878273687097
0.3657878273687097
0.3657878273687097
0.3113769033254328
0.1826593137254902
(5.54 secs, 7,015,796,792 bytes)
-}
secStratFun :: forall a. (Ord a, Fractional a) => SecState (SecVal a) -> SecVal a
secStratFun = unFunRep optStrat

{- |
    > secStratMemo = unMemoRep optStrat

    The optimal strategy for the secretary problem
    represented as a threshold function\;
    is \(O(n^2)\) in time and \(O(n)\) in space
    thanks to (impure!) memoization

==== __Demo__
>>> :set -XHaskell2010 -XTypeApplications -Wall +s
>>> mapM_ print . flip fmap [0,2..8] $ secStratMemo @Double . (\ s -> SecState s $ 10 - s)
0.36535714285714294
0.36535714285714294
0.36535714285714294
0.3172619047619048
0.1875
(0.00 secs, 981,080 bytes)
>>> :r
>>> mapM_ print . flip fmap [0,20..80] $ secStratMemo @Double . (\ s -> SecState s $ 100 - s)
0.3673782318678297
0.3673782318678297
0.3663560527548714
0.30740292913958817
0.17929481058854396
(0.00 secs, 3,089,184 bytes)
>>> :r
>>> mapM_ print . flip fmap [0,200..800] $ secStratMemo @Double . (\ s -> SecState s $ 1000 - s)
0.36782771826585464
0.36782771826585464
0.3665000271807624
0.306584704443603
0.17859181015336933
(0.03 secs, 49,693,288 bytes)
-}
secStratMemo :: forall a. (Ord a, Fractional a) => SecState (SecVal a) -> SecVal a
secStratMemo = unMemoRep optStrat

{- |
    > coinStratFun = unFunRep optStrat

    The optimal strategy for the coin toss problem
    represented as a threshold function\;
    is \(O(2^{2^n})\) in time and space
    due to the lack of memoization

==== __Demo__
>>> :set -XHaskell2010 -XTypeApplications -Wall +s
>>> mapM_ print . flip fmap [0,2..4] $ coinStratFun @Double . (\ s -> CoinState s (5 - s) (quot s 2))
0.0
0.6041666666666666
0.5
(0.01 secs, 4,814,680 bytes)
>>> :r
>>> mapM_ print . flip fmap [0,2..8] $ coinStratFun @Double . (\ s -> CoinState s (10 - s) (quot s 2))
0.0
0.6136315724206349
0.5757192460317461
0.5543154761904762
0.5277777777777778
(3.25 secs, 4,232,519,632 bytes)
-}
coinStratFun :: forall a. (Ord a, Fractional a) => CoinState (CoinVal a) -> CoinVal a
coinStratFun = unFunRep optStrat

{- |
    > coinStratFun = unMemoRep optStrat

    The optimal strategy for the coin toss problem
    represented as a threshold function\;
    is \(O(n^3)\) in time and \(O(n^2)\) in space
    thanks to (impure!) memoization

==== __Demo__
>>> :set -XHaskell2010 -XTypeApplications -Wall +s
>>> mapM_ print . flip fmap [0,2..4] $ coinStratMemo @Double . (\ s -> CoinState s (5 - s) (quot s 2))
0.0
0.6041666666666666
0.5
(0.00 secs, 1,308,256 bytes)
>>> :r
>>> mapM_ print . flip fmap [0,2..8] $ coinStratMemo @Double . (\ s -> CoinState s (10 - s) (quot s 2))
0.0
0.6136315724206349
0.5757192460317461
0.5543154761904762
0.5277777777777778
(0.01 secs, 2,907,152 bytes)
>>> :r
>>> mapM_ print . flip fmap [0,20..80] $ coinStratMemo @Double . (\ s -> CoinState s (100 - s) (quot s 2))
0.0
0.5333078812258507
0.5164026209205757
0.5096352567961813
0.5062560322091281
(0.15 secs, 209,793,872 bytes)
-}
coinStratMemo :: forall a. (Ord a, Fractional a) => CoinState (CoinVal a) -> CoinVal a
coinStratMemo = unMemoRep optStrat