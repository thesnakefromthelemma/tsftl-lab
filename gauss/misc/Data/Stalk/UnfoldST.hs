{-# LANGUAGE Haskell2010
  , KindSignatures
  , MagicHash
  , PatternSynonyms
  , RankNTypes
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Safely, lazily, and fusibly unfolding 'Data.Stalk.Stalk's
    within 'Control.Monad.ST.ST'
-}
module Data.Stalk.UnfoldST
  ( -- * 'unfoldST'
    unfoldST
  ) where


-- * Imports

-- ** base

import Data.Kind
  ( Type )

import GHC.Exts
  ( runRW# )

import Control.Monad.ST
  ( ST )

import GHC.ST
  ( pattern ST )


-- ** (internal)

import Data.Stalk
  ( Stalk
  , build
  )


-- * 'unfoldST'

{- | A safe, lazy, and fusible unfolding
    (\'a la 'Data.List.unfoldr') of 'Data.Stalk.Stalk's
    within 'Control.Monad.ST.ST'\; seems like it should be standard?
    Well, implementing it correctly is hard! (Cf. discussion elsewhere?)

    Note that the explicit passing of the state tokens @s@\*
    in this implementation merely serves as proof to the reader
    that the relevant 'Control.Monad.ST.ST' actions
    execute in the correct order; the pattern in which
    the results of the latter are demanded ensures
    that this will happen even if we discard the former
-}
{-# INLINE unfoldST #-}
unfoldST :: forall (ref :: Type -> Type) a b.
    (forall s. ref s -> ST s (Either b (a, ref s))) ->
    (forall s. ST s (ref s)) ->
    Stalk b a
unfoldST =  \ t (ST xr0) ->
    build $ \ g f ->
        let unfoldSTR = \ r s -> let ST xebar' = t r in case xebar' s of
                (# s', Right (a, r') #) -> g a $ unfoldSTR r' s'
                (# _ , Left b        #) -> f b
        in  runRW# $ \ s0 -> case xr0 s0 of
                (# s0', r0 #) -> unfoldSTR r0 s0'