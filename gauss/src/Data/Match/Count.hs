{-# LANGUAGE Haskell2010
  , DerivingStrategies
  , GADTSyntax
  , StandaloneDeriving
#-}

{-# OPTIONS_GHC -Wall #-}

{-| 'Count' and 'CountMat' -}
module Data.Match.Count
  ( -- * 'CountMat'
    Count
      ( Zero
      , One
      , Many
      )
    -- * 'CountMat'
  , CountMat
      ( CountMat
      , sizeIn
      , sizeOut
      , fun
      )
  ) where


-- * 'Count'

{- | Type representing matrix entries\;
    either 'Zero' (eliminated), 'One' (unit),
    or 'Many' (requiring elimination)
-}
data Count where
    Zero, One, Many :: Count

deriving stock instance Eq Count
deriving stock instance Ord Count
deriving stock instance Show Count


-- *  'CountMat'

{- | Type representing matrices with
    entries summarized as 'Count' values
-}
data CountMat where
    CountMat ::
      { sizeIn :: !Int -- ^ Input dimension
      , sizeOut :: !Int -- ^ Output dimension
      , fun :: !(Int -> Int -> Count) } -> -- ^ Entry accessor
      CountMat