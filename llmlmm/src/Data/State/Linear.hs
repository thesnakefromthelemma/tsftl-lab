{-# LANGUAGE Haskell2010
  , DataKinds
  , FlexibleInstances
  , GADTSyntax
  , InstanceSigs
  , LinearTypes
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TemplateHaskell
  , TupleSections
  , TypeApplications
  , UnboxedTuples
  , UnliftedNewtypes
#-}

{-| @-Woverlapping-patterns@ and @Winaccessible-code@ are disabled
    as they only fire due to the match on 'UnsafeRefl'
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-overlapping-patterns
    -Wno-inaccessible-code
#-}

{- | Linear low-level 'Control.Monad.ST.runST' -}
module Data.State.Linear
  ( -- * 'State#'-parametrized allocation tokens
    State#
      ( State# )
    -- * Linear low-level 'Control.Monad.ST.runST'
  , runST#
    -- _ 'State#' token manipulation
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( TYPE
  , RuntimeRep
      ( TupleRep
      , BoxedRep
      )
  , pattern Lifted
  , pattern One
  , runRW#
  )

import qualified GHC.Exts as GHC
  ( State# )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( mkName
  , pattern ConT
  , pattern AppT
  , pattern VarT
  )

-- ++ (internal)

import Prelude.Linear
  ( Urlike (..) )

import Prelude.Linear.TH
  ( deriveUrlike )


-- * 'State#'-parametrized allocation tokens

{- | 'State#'-parametrized allocation tokens -}
newtype State# :: TYPE (BoxedRep Lifted) -> TYPE (TupleRep '[]) where
    State# ::
        forall (s :: TYPE (BoxedRep Lifted)).
        GHC.State# s %One-> State# s


-- * Linear low-level 'Control.Monad.ST.runST'

{- | Linear low-level 'Control.Monad.ST.runST'\;
    note that in this paradigm the 'State#' values
    do not represent the \"state of the real world\",
    but instead tokens affording resource allocation
    (after consumption as which they cease to be passed around).
-}
{-# INLINE runST# #-}
runST# ::
    forall (r :: RuntimeRep) (a :: TYPE r).
    (forall s. State# s %One-> a) -> a
runST# = \ x -> runRW# (\ s -> x (State# s))


-- _ 'State#' token manipulation

{- | Instantiates 'Urlike' for (@forall s.@) @State# s@ -}
$(pure
    [ deriveUrlike
        ( TupleRep [ ] )
        ( AppT
            ( ConT ''State# )
            ( VarT (mkName "s") ) ) ] -- There's no point in being explicit about this quantification thanks to GHC-71492
  )