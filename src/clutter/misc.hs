{-# LANGUAGE ScopedTypeVariables #-}

module Misc
    ( (<*$)
    , (<*.)
    , (.<**)
    , (<**$)
    ) where


-- POINTLESS COMBINATORS --

infixr 8 <*$
(<*$) :: forall a b c. (a -> b -> c) -> b -> a -> c
(<*$) = flip

infixr 8 <*.
(<*.) :: forall a b b' c. (a -> b' -> c) -> (b -> b') -> a -> b -> c
(<*.) = (fmap . ((.) <*$) <*$)

infixr 8 .<**
(.<**) :: forall a b c c'. (c -> c') -> (a -> b -> c) -> a -> b -> c'
(.<**) = fmap . (.)

infixr 8 <**$
(<**$) :: forall a b c d. (a -> b -> c -> d) -> c -> a -> b -> d
(<**$) = (<*$) . fmap (<*$)
