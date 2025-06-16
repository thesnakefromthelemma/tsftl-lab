{-# LANGUAGE
      TupleSections
    , TypeApplications
    #-}

module PermOrd
    ( pDivOrd
    , eLogOrd
    , estELogOrd
    , showELogOrd
    , texELogOrd
    ) where


-- IMPORTS --

import Data.Function
    ( fix )

import Data.List 
    ( iterate'   
    , intersperse
    )

import qualified Data.Vector.Strict as VS
    ( Vector
    , singleton
    , length
    , last
    , init
    , take
    , cons
    , sum
    )

import Data.Ratio
    ( numerator  
    , denominator
    )


-- PRIME GENERATOR --

relPrimes :: [Int] -> [Int]
relPrimes = \sn -> (2 :) . filter (\x -> and . fmap ((0 /=) . mod x) $ takeWhile ((x >=) . (^ (2 :: Int))) sn) $ iterate' (2 +) 3

primes :: [Int]
primes =
    let cache :: [Int]
        cache = fix relPrimes
    in  relPrimes cache


-- MAIN COMPUTATION(S) --

seed :: VS.Vector Rational
seed = VS.singleton 1

reap :: Int -> VS.Vector Rational -> Rational
reap = \q ->
    VS.sum . VS.take (q - 1)

grow :: Int -> Int -> VS.Vector Rational -> VS.Vector Rational
grow = \q -> \r -> \sp -> -- UNDESIRED BEHAVIOR/ERROR IF q < 1 OR r < 0 OR q < length sp
    let r0, r1 :: Rational
        r0 = 1 / (fromIntegral r + 1)
        r1 = (fromIntegral r) / (fromIntegral r + 1)
    in  case compare q (VS.length sp) of
            GT -> VS.cons (r0 * reap q sp) $ fmap (r1 *) sp
            EQ -> VS.cons (r0 * reap q sp + r1 * (VS.last sp)) . fmap (r1 *) $ VS.init sp
            _  -> undefined

pDivOrd :: Int -> Int -> Rational
pDivOrd = \n -> \q -> -- UNDESIRED BEHAVIOR IF q < 1
    (1 -) . reap q $ foldr (grow q) seed [1 .. n-1]

eLogOrd :: Int -> [(Int, Rational)]
eLogOrd = \n -> -- UNDESIRED BEHAVIOR IF n < 0
    fmap (\q -> (q,) . sum . fmap (pDivOrd n) . takeWhile (n >=) $ fmap (q ^) ([1..] :: [Int])) $ takeWhile (n >=) primes


-- POSTPROCESSING --

ester :: [(Int, Rational)] -> Double
ester = sum . fmap (\(q, p) -> (fromRational p) * log (fromIntegral q))

shower :: [(Int, Rational)] -> String
shower =
    foldr (++) "" . intersperse " + " . fmap (\(q, p) -> (show $ numerator p) ++ " / " ++ (show $ denominator p) ++ " * log(" ++ show q ++ ")")

texer :: [(Int, Rational)] -> String
texer =     
    foldr (++) "" . intersperse "\\ +\\ " . fmap (\(q, p) -> "\\frac{" ++ (show $ numerator p) ++ "}{" ++ (show $ denominator p) ++ "}\\log(" ++ show q ++ ")")

estELogOrd :: Int -> Double
estELogOrd =
    ester . eLogOrd

showELogOrd :: Int -> String
showELogOrd =
    shower . eLogOrd

texELogOrd :: Int -> String
texELogOrd =
    texer . eLogOrd
