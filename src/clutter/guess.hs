{-# LANGUAGE
      BangPatterns
    , LambdaCase
    , ScopedTypeVariables
    #-}

module Guess
    ( main
    ) where


-- IMPORTS --

import Prelude hiding
    ( getLine )

import Data.Function
    ( fix )

import Control.Monad
    ( (<=<) )

import Control.Monad.State.Strict
    ( StateT
    , get
    , put
    , lift
    , execStateT
    )

import System.Random.Stateful
    ( uniformRM
    , globalStdGen
    )

import Text.Read
    ( readMaybe )

import qualified Prelude
    ( getLine )

import System.IO
    ( stdin
    , BufferMode
        ( LineBuffering )
    , hSetBuffering
    )


-- CODE --

infixr 1 <<
(<<) :: forall m a b. Monad m => m b -> m a -> m b
(<<) = flip (>>)

qual :: forall a. (a -> Bool) -> a -> Maybe a
qual = \p -> \a -> case p a of
    True  -> Just a
    False -> Nothing

getLine :: IO String
getLine = Prelude.getLine << hSetBuffering stdin LineBuffering

readQual :: forall a. Read a => (a -> Bool) -> IO a
readQual = \p -> fix $ \readQualP ->
    maybe (readQualP << putStrLn "Invalid input! Try again.") pure =<< (qual p <=< readMaybe) <$> getLine

chideIf :: Bool -> String
chideIf = \case
    True  -> " (Hint: That was a silly guess.)"
    False -> ""

guess :: Int -> Int -> Int -> StateT ((Int, Int), Int) IO ()
guess = \lb -> \ub -> \ans -> fix $ \guessP ->
    do  g <- lift $ readQual (\x -> x >= lb && x < ub)
        ((klb, kub), gn) <- get
        let klb', kub', gn' :: Int
            !klb' = max klb $ 1 + g      
            !kub' = min kub g
            !gn' = 1 + gn
        case compare ans g of
            LT -> guessP << (lift . putStrLn $ "Too high!" ++ chideIf (kub <= g)) << put ((klb, kub'), gn')
            GT -> guessP << (lift . putStrLn $ "Too low!" ++ chideIf (klb > g)) << put ((klb', kub), gn')
            EQ -> (lift . putStrLn $ "Got it!") << put ((klb, kub), gn')

plu :: forall a. Integral a => a -> String
plu = \a -> case 1 == a of
    True  -> ""
    False -> "es" 

main :: IO ()
main = do
    putStrLn "Pick an integer. (Suggestion: 0)"
    lb <- readQual (const True)
    putStrLn $ "Pick an integer > " ++ show lb ++ ". (Suggestion: 63)"
    ub <- readQual (> lb)
    ans <- uniformRM (lb, ub - 1) globalStdGen
    putStrLn $ "I'm thinking of an integer x such that " ++ show lb ++ " <= x < " ++ show ub ++ ", can you guess it?"
    ((klb, kub), gn) <- execStateT (guess lb ub ans) ((lb, ub), 0)
    putStrLn $ "Success in " ++ show gn ++ " guess" ++ plu gn ++ ".\n\
               \At the time of your final guess, you knew that " ++ show klb ++ " <= x < " ++ show kub ++ ", \
               \hence a 1/" ++ (show $ kub - klb) ++ " chance of success.\n\
               \Skill or just luck?"
