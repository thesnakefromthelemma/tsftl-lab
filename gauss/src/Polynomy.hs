{-# LANGUAGE Haskell2010
  , GADTSyntax
  , InstanceSigs
  , KindSignatures
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall #-}

module Polynomy
  ( -- * Polynomy
    gauss
  , degauss
  , Bias
      ( Bias
      , unBias
      )
  , Action
      ( Action
      , unAction
      )
  , InTerm
      ( InTerm
      , inTermBias
      , inTermAction
      )
  , Free
      ( Free
      , unFree
      )
  , Sym
      ( Sym
      , unSym
      )
  , OutTerm
      ( OutTerm
      , outTermFree
      , outTermSym
      )
  ) where


-- * Imports

-- ** vector

import qualified Data.Vector.Generic as V
  ( snoc
  , scanr1'
  , zipWith
  , foldr
  , uncons
  )

import qualified Data.Vector.Unboxed as VU
  ( Vector )


-- * Polynomy

gauss :: VU.Vector Word -> VU.Vector Word
gauss = \ vn -> case V.uncons vn of
    Just (_, vn') -> V.zipWith (-) vn $ V.snoc vn' 0
    Nothing       -> vn

degauss :: VU.Vector Word -> VU.Vector Word
degauss = V.scanr1' (+)

showWithBefore :: String -> VU.Vector Word -> String -> String
showWithBefore = \ str0 vn ->
    (str0 ++) . fmap flip V.foldr (\ n str -> show n ++ " " ++ str) vn

newtype Bias where
    Bias :: { unBias :: VU.Vector Word } -> Bias

instance Show Bias where
    show :: Bias -> String
    show = \ (Bias vnb) ->
        showWithBefore "< " vnb ">"

newtype Action where
    Action :: { unAction :: VU.Vector Word } -> Action

instance Show Action where
    show :: Action -> String
    show = \ (Action vna) ->
        showWithBefore "[ " vna "]"

data InTerm where
    InTerm :: {
        inTermBias :: !Bias ,
        inTermAction :: !Action } ->
        InTerm

instance Show InTerm where
    show :: InTerm -> String
    show = \ (InTerm (Bias vnb) (Action vna)) ->
        showWithBefore "< " vnb .
            showWithBefore ": " vna $
                "]"

newtype Free where
    Free :: { unFree :: VU.Vector Word } -> Free

instance Show Free where
    show :: Free -> String
    show = \ (Free vnf) ->
        showWithBefore "( " vnf ")"

newtype Sym where
    Sym :: { unSym :: VU.Vector Word } -> Sym

instance Show Sym where
    show :: Sym -> String
    show = \ (Sym vns) ->
        showWithBefore "{ " vns "}"

data OutTerm where
    OutTerm :: {
        outTermFree :: !Free ,
        outTermSym :: !Sym } ->
        OutTerm

instance Show OutTerm where
    show :: OutTerm -> String
    show = \ (OutTerm (Free vnf) (Sym vns)) ->
        showWithBefore "( " vnf .
            showWithBefore "; " (degauss vns) $
                "}"