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
  , guardInstance
  , guardRange
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import Data.Foldable
  ( traverse_ )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( Name
  , Type
  , Q
  , Extension
  , isExtEnabled
  , lookupValueName
  , lookupTypeName
  , isInstance
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

{- | Given arguments @spl@, @nm@, @[ ty0 .. ty{n-1} ]@
    representing the text of splice
    the name of a class,
    and a list of types,
    fails iff there is no @nm [ ty0 .. ty{n-1} ]@ instance in scope
    (and does nothing otherwise)
-}
guardInstance :: String -> Name -> [Type] -> Q ()
guardInstance = \ spl nm sty -> isInstance nm sty >>= \case
    True  -> pure ()
    False -> fail $ spl <> " requires that a(n) @"
        <> show nm <> foldMap (\ ty -> " (" <> show ty <> ")") sty
        <> "@ instance be in scope."

{- | Given arguments @spl@, @nm@, @a_min@, @a_max@, @a@,
    representing the text of splice
    the name of a type required by it,
    an (inclusive) lower bound,
    an (inclusive) upper bound,
    and an 'Enum' value,
    fails iff @a@ is not in @[ a_min .. a_max ]@
    (and does nothing otherwise)
-}
guardRange :: forall a. (Eq a, Enum a, Show a) =>
    String -> String -> a -> a -> a -> Q ()
guardRange = \ spl lit a_min a_max a -> case elem a $ enumFromTo a_min a_max of
    True  -> pure ()
    False -> fail $ spl <> " requires that " <> lit <> " be in "
        <> "[ " <> show a_min <> " .. " <> show a_max <> " ]."