{-# LANGUAGE Haskell2010
  , DataKinds
  , GHCForeignImportPrim
  , MagicHash
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TemplateHaskell
  , UnboxedTuples
  , UnliftedFFITypes
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Representation-polymorphic 'Control.Monad.ST.runST' -}
module Data.State
  ( -- * 'runST#'
    runST#
    -- * 'State#' token forking
  , fork0from1#
  , fork1from1#
  , fork2from1#
  , fork3from1#
  , fork4from1#
  , fork5from1#
  , fork6from1#
  , fork7from1#
  , fork8from1#
  , fork0from2#
  , fork1from2#
  , fork2from2#
  , fork3from2#
  , fork4from2#
  , fork5from2#
  , fork6from2#
  , fork7from2#
  , fork8from2#
  , fork0from3#
  , fork1from3#
  , fork2from3#
  , fork3from3#
  , fork4from3#
  , fork5from3#
  , fork6from3#
  , fork7from3#
  , fork8from3#
  , fork0from4#
  , fork1from4#
  , fork2from4#
  , fork3from4#
  , fork4from4#
  , fork5from4#
  , fork6from4#
  , fork7from4#
  , fork8from4#
  , fork0from5#
  , fork1from5#
  , fork2from5#
  , fork3from5#
  , fork4from5#
  , fork5from5#
  , fork6from5#
  , fork7from5#
  , fork8from5#
  , fork0from6#
  , fork1from6#
  , fork2from6#
  , fork3from6#
  , fork4from6#
  , fork5from6#
  , fork6from6#
  , fork7from6#
  , fork8from6#
  , fork0from7#
  , fork1from7#
  , fork2from7#
  , fork3from7#
  , fork4from7#
  , fork5from7#
  , fork6from7#
  , fork7from7#
  , fork8from7#
  , fork0from8#
  , fork1from8#
  , fork2from8#
  , fork3from8#
  , fork4from8#
  , fork5from8#
  , fork6from8#
  , fork7from8#
  , fork8from8#
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( RuntimeRep
  , TYPE
  , State#
  , runRW#
  )

-- ++ (internal)

import Data.State.TH
  ( declareForkState# )


-- * Representation-polymorphic 'Control.Monad.ST.runST'

{- | Representation-polymorphic 'Control.Monad.ST.runST' -}
{-# INLINE runST# #-}
runST# ::
    forall {r :: RuntimeRep} (a :: TYPE r).
    (forall s. State# s -> a) -> a
runST# = runRW#


-- * 'State#' token forking

{- | TemplateHaskell generation of 'State#' token forking -}
$(sequence $ do
    n_in <- [ 1 .. 8 ]
    n_out <- [ 0 .. 8 ]
    [ declareForkState# n_in n_out ]
  )