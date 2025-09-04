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
    in a 'Data.HashSet.HashSet')
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
    the total number of candidates under consideration
    and those remaining (not including the one in hand)
    respectively
-}
data SecState :: Type -> Type where
    SecState :: forall a.
        !Word -> -- ^ Total number of secretaries considered
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
newtype SecVal :: Type -> Type where
    SecVal :: forall a. a -> SecVal a

deriving instance forall a. Eq a => Eq (SecVal a)
deriving instance forall a. Ord a => Ord (SecVal a)
deriving instance forall a. Num a => Num (SecVal a)
deriving instance forall a. Fractional a => Fractional (SecVal a)
deriving instance forall a. Floating a => Floating (SecVal a)  -- /Not used here/
deriving instance forall a. Show a => Show (SecVal a) -- /Just for convenience in the REPL/

instance forall a. (Ord a, Fractional a) => DRandVar (SecState (SecVal a)) (SecVal a) where
    plt :: SecState (SecVal a) -> SecVal a -> SecVal a
    plt = \ (SecState n r) sa -> case (compare 0 sa, compare (fromIntegral (n - r) / fromIntegral n) sa) of
        (GT, _ ) -> 0
        (_ , GT) -> (1 -) . recip . fromIntegral $ n - r
        _        -> 1

    mean :: SecState (SecVal a) -> SecVal a
    mean = \ (SecState n _) -> recip $ fromIntegral n

    mlt :: SecState (SecVal a) -> SecVal a -> SecVal a
    mlt = \ (SecState n r) sa -> case compare (fromIntegral (n - r) / fromIntegral n) sa of
        GT -> 0
        _  -> recip $ fromIntegral n


-- ** Coin toss problem distribution

{- | TODO: WRITE -}
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

{- | TODO: WRITE -}
newtype CoinVal :: Type -> Type where
    CoinVal :: forall a. a -> CoinVal a

deriving instance forall a. Eq a => Eq (CoinVal a)
deriving instance forall a. Ord a => Ord (CoinVal a)
deriving instance forall a. Num a => Num (CoinVal a)
deriving instance forall a. Fractional a => Fractional (CoinVal a)
deriving instance forall a. Floating a => Floating (CoinVal a) -- /Not used here/
deriving instance forall a. Show a => Show (CoinVal a) -- /Just for convenience in the REPL/

instance forall a. (Ord a, Fractional a) => DRandVar (CoinState (CoinVal a)) (CoinVal a) where
    plt :: CoinState (CoinVal a) -> CoinVal a -> CoinVal a
    plt = \ (CoinState q _ s) ca -> case compare (fromIntegral s / fromIntegral q) ca of
        GT -> 0
        _  -> 1

    mean :: CoinState (CoinVal a) -> CoinVal a
    mean = \ (CoinState q _ s) -> fromIntegral s / fromIntegral q

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

    Generall efficient across applications as 'eval'
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

{- | TODO: WRITE -}
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
    is \(O(n)\) in time and space(!)
    thanks to (impure!!) memoization
-}
boxStratMemo :: forall a. (Ord a, Fractional a) => BoxState (BoxVal a) -> BoxVal a
boxStratMemo = unMemoRep optStrat

{- | TODO: WRITE -}
secStratFun :: forall a. (Ord a, Fractional a) => SecState (SecVal a) -> SecVal a
secStratFun = unFunRep optStrat

{- | TODO: WRITE -}
secStratMemo :: forall a. (Ord a, Fractional a) => SecState (SecVal a) -> SecVal a
secStratMemo = unMemoRep optStrat

{- | TODO: WRITE -}
coinStratFun :: forall a. (Ord a, Fractional a) => CoinState (CoinVal a) -> CoinVal a
coinStratFun = unFunRep optStrat

{- | TODO: WRITE -}
coinStratMemo :: forall a. (Ord a, Fractional a) => CoinState (CoinVal a) -> CoinVal a
coinStratMemo = unMemoRep optStrat