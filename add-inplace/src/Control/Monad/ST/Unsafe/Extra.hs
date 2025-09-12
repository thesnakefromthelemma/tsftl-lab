{-# LANGUAGE Haskell2010
  , ScopedTypeVariables
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Lazy 'traverse' and 'sequence' in the 'ST' monad -}
module Control.Monad.ST.Unsafe.Extra
  ( -- * Lazy 'traverse' and 'sequence' in the 'ST' monad
    traverseSTLazyUnsafe
  , sequenceSTLazyUnsafe
  ) where


-- + Imports

-- ++ From base >= 4.21 && << 4.22

import qualified GHC.Exts as List
  ( build )

import GHC.ST
  ( ST
      ( ST )
  )


-- * Lazy 'traverse' and 'sequence' in the 'ST' monad

{-# INLINE traverseSTLazyUnsafe #-}
traverseSTLazyUnsafe :: forall s a0 a1. (a0 -> ST s a1) -> [a0] -> ST s [a1]
traverseSTLazyUnsafe = \ f sa0 -> ST $ \ s ->
    let sa1 = List.build $ \ g b ->
            let buildF = \ a0 -> case f a0 of
                    ST xsa1 -> g $ case xsa1 s of
                        (# _, a1 #) -> a1
            in  foldr buildF b sa0
    in  (# s, sa1 #)

{-# INLINE sequenceSTLazyUnsafe #-}
sequenceSTLazyUnsafe :: forall s a. [ST s a] -> ST s [a]
sequenceSTLazyUnsafe = traverseSTLazyUnsafe id