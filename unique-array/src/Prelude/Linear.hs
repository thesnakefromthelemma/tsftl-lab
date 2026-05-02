{-# LANGUAGE Haskell2010
  , DataKinds
  , GADTs
  , LinearTypes
  , PatternSynonyms
  , PolyKinds
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Miscellaneous linear utilities -}
module Prelude.Linear
  ( -- * Strict unrestricted modality
    Ur
      ( Ur )
    -- * Kind and multiplicity-polymorphic 'Prelude.($)' operator
  , ($)
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import Prelude hiding
  ( ($) )

import GHC.Exts
  ( TYPE
  , RuntimeRep
      ( BoxedRep )
  , pattern Lifted
  , Multiplicity
  )


-- * Strict unrestricted modality

{- | Strict unrestricted modality -}
data Ur :: forall (r ::  RuntimeRep). TYPE r -> TYPE (BoxedRep Lifted) where
    Ur :: forall a. !a -> Ur a


-- * Kind and multiplicity-polymorphic 'Prelude.($)' operator

infixr 0 $
{- | Kind and multiplicity-polymorphic 'Prelude.($)' operator -}
{-# INLINE ($) #-}
($) :: forall
    (p :: Multiplicity) (q :: Multiplicity)
    (ra :: RuntimeRep) (rb :: RuntimeRep)
    (a :: TYPE ra) (b :: TYPE rb).
    (a %q -> b) %p -> a %q -> b
($) = \ f -> f