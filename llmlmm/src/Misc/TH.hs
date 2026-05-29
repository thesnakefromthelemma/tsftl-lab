{-# LANGUAGE Haskell2010
  , LambdaCase
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Miscllaneous TemplateHaskell helpers -}
module Misc.TH
  ( -- * Miscllaneous TemplateHaskell helpers
    guardExts
  , guardValue
  , guardType
  , guardRange
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import Data.Foldable
  ( traverse_ )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( Name
  , Q
  , Extension
  , isExtEnabled
  , lookupValueName
  , lookupTypeName
  )


-- * Miscllaneous TemplateHaskell helpers

{- | Given arguments @spl@, @sext@,
    representing the text of splice
    and a list of extensions required by it,
    fails iff any of said extensions are not enabled
    (and does nothing otherwise)
-}
guardExts :: String -> [Extension] -> Q ()
guardExts = \ spl -> traverse_ (\ ext -> isExtEnabled ext >>= \case
    True  -> pure ()
    False -> fail $ spl<> " requires that -X" <> show ext <> " be enabled."
  )

{- | Given arguments @spl@, @nm@,
    representing the text of splice
    and the name of a value required by it,
    fails iff said value is not in scope
    and returns said name otherwise
-}
guardValue :: String -> String -> Q Name
guardValue = \ spl lit -> lookupValueName lit >>= \case
    Just nm -> pure nm
    Nothing -> fail $ spl <> " requires that \'" <> lit <>"\' be in scope."

{- | Given arguments @spl@, @nm@,
    representing the text of splice
    and the name of a type required by it,
    fails iff said type is not in scope
    and returns said name otherwise
-}
guardType :: String -> String -> Q Name
guardType = \ spl lit -> lookupTypeName lit >>= \case
    Just nm -> pure nm
    Nothing -> fail $ spl <> " requires that \'" <> lit <> "\' be in scope."

{- | Given arguments @spl@, @nm@,
    representing the text of splice
    and the name of a type required by it,
    fails iff said type is not in scope
    and returns said name otherwise
-}
guardRange :: forall a. (Eq a, Enum a, Show a) =>
    String -> String -> a -> a -> a -> Q ()
guardRange = \ spl lit a_min a_max a -> case elem a $ enumFromTo a_min a_max of
    True  -> pure ()
    False -> fail $ spl <> " requires that " <> lit <> " be in "
        <> "[ " <> show a_min <> " .. " <> show a_max <> " ]."