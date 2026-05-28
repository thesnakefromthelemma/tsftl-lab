{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , FlexibleInstances
  , GHCForeignImportPrim
  , InstanceSigs
  , LinearTypes -- Just to make 'Statelike' easier to recycle
  , MagicHash
  , MultiParamTypeClasses
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  , UnboxedTuples
  , UnliftedFFITypes
#-}

{-| @-Woverlapping-patterns@ and @Winaccessible-code@ are disabled
    as they only fire due to the match on 'UnsafeRefl'.
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-inaccessible-code
    -Wno-overlapping-patterns
#-}

{- | Low-level 'Control.Monad.ST.runST' -}
module Data.State
  ( -- * 'runST#'
    runST#
    -- * Representation-polymorphic interface to linearly synchronizable types
  , Statelike
      ( sync1
      , sync2
      , sync3
      , sync4
      , sync5
      , sync6
      , sync7
      , sync8
#if FULL
      , sync9
      , sync10
      , sync11
      , sync12
      , sync13
      , sync14
      , sync15
      , sync16
      , sync17
      , sync18
      , sync19
      , sync20
      , sync21
      , sync22
      , sync23
      , sync24
      , sync25
      , sync26
      , sync27
      , sync28
      , sync29
      , sync30
      , sync31
      , sync32
      , sync33
      , sync34
      , sync35
      , sync36
      , sync37
      , sync38
      , sync39
      , sync40
      , sync41
      , sync42
      , sync43
      , sync44
      , sync45
      , sync46
      , sync47
      , sync48
      , sync49
      , sync50
      , sync51
      , sync52
      , sync53
      , sync54
      , sync55
      , sync56
      , sync57
      , sync58
      , sync59
      , sync60
      , sync61
      , sync62
      , sync63
      , sync64
#endif
      )
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
  ( declareStatelike
  , declareStatelikeState#
  )


-- * Low-level 'Control.Monad.ST.runST'

{- | Low-level 'Control.Monad.ST.runST' -}
{-# INLINE runST# #-}
runST# ::
    forall (r :: RuntimeRep) (a :: TYPE r).
    (forall s. State# s -> a) -> a
runST# = runRW#


-- * TemplateHaskell generation of representation-polymorphic interface to linearly synchronizable types

{- | Declares representation-polymorphic interface to linearly synchronizable types -}
$(sequence
    [ declareStatelike ]
  )

{- | Declared 'Statelike' instance of 'GHC.Exts.State#' -}
$( declareStatelikeState# )