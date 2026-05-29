{-# LANGUAGE Haskell2010
  , CPP
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , TemplateHaskellQuotes
#-}

{-# OPTIONS_GHC -Wall #-}

{- 
Note [Future work]
~~~~~~~~~~~~~~~~~~

  * GHC: Inline primops definable

  * 'declareForkState#' FFIs 'Data.State.PrimOps.Cmm.noPrimOp' as an inline primop
-}

{- | TemplateHaskell generation of 'State#' token forking -}
module Data.State.TH
  ( -- * TemplateHaskell generation of 'State#' token forking
    declareForkState#
  ) where

-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( State# )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( mkName
  , newName
  , pattern UnboxedTupleT
  , pattern ArrowT
  , pattern ConT
  , pattern AppT
  , pattern VarT
  , pattern PlainTV
  , pattern SpecifiedSpec
  , pattern ForallT
  , Dec
  , pattern Prim
  , pattern Safe
  , pattern ImportF
  , pattern ForeignD
  , Q
  , pattern GHCForeignImportPrim
  , pattern MagicHash
  , pattern ScopedTypeVariables
  , pattern UnboxedTuples
  , pattern UnliftedFFITypes
  )

-- ++ (internal)

import Misc.TH
  ( guardExts
  , guardRange
  )


-- * TemplateHaskell generation of 'State#' token forking

{- | Given arguments @n_in@, @n_out@,
    generates the forking of @n_out@ 'State#' tokens
    from  @n_in@ 'State#' tokens\;
    morally equivalent to the @CPP@ macro
    @
        #define DECLARE_FORK_STATE(N_IN, N_OUT)                                          \
        foreign import prim "noPrimOp"                                                   \
            forkN_OUTfromN_IN# ::                                                        \
                forall s. (# State# s, ..., State# s #) -> (# State# s, ..., State# s #)
    @
    Requires @-XGHCForeignImportPrim -XMagicHash -XScopedTypeVariables -XUnboxedTuples -XUnliftedFFITypes@.
    Requires that @N_IN@ be in @[ 1 .. 64 ]@.
    Requires that @N_OUT@ be in @[ 0 .. 64 ]@.    
-}
declareForkState# :: Int -> Int -> Q Dec
declareForkState# = \ n_in n_out -> do
    guardExts
      ( "\'Data.State.declareForkState#\'" )
      [ GHCForeignImportPrim
      , MagicHash
      , ScopedTypeVariables
      , UnboxedTuples
      , UnliftedFFITypes ]
    guardRange
      ( "\'Data.State.declareForkState#\'" )
      ( "@n_in@" )
      ( 1 )
      ( 64 )
      ( n_in )
    guardRange
      ( "\'Data.State.declareForkState#\'" )
      ( "@n_in@" )
      ( 0 )
      ( 64 )
      ( n_out )
    let fork_n_nm = mkName $ "fork" <> show n_out <> "from" <> show n_in <> "#"
    s_nm <- newName "s"
    let tup_n_in_ty = foldr (\ _ b ->
            AppT
              ( b )
              ( AppT
                  ( ConT ''State# )
                  ( VarT s_nm ) )
          ) ( UnboxedTupleT n_in ) [ 0 .. n_in - 1 ]
    let tup_n_out_ty = foldr (\ _ b ->
            AppT
              ( b )
              ( AppT
                  ( ConT ''State# )
                  ( VarT s_nm ) )
          ) ( UnboxedTupleT n_out ) [ 0 .. n_out - 1 ]
    pure
      ( ForeignD ( ImportF
          ( Prim )
          ( Safe )
          ( "noPrimOp" )
          ( fork_n_nm )
          ( ForallT
              [ PlainTV
                  ( s_nm )
                  ( SpecifiedSpec ) ]
              [ ]
              ( AppT ( AppT
                  ( ArrowT )
                  ( tup_n_in_ty ) )
                  ( tup_n_out_ty ) ) ) ) )