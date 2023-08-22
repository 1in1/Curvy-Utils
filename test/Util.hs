module Util 
    ( quadraticFromRoots
    , cubicFromRoots
    , uncurry3
    , genPrime
    , genRationalBoundedArguments
    , genRationalTriple
    , genInertRational
    , genBoundedInertRational
) where

import Data.Maybe
import Data.Ratio
import Math.NumberTheory.Primes (isPrime)

import Test.QuickCheck

quadraticFromRoots :: Rational -> Rational -> (Rational, Rational)
quadraticFromRoots a b = (-(a+b), a*b)
cubicFromRoots :: Rational -> Rational -> Rational -> (Rational, Rational, Rational)
cubicFromRoots a b c = (-(a+b+c), a*b + a*c+ b*c, -(a*b*c))

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

genPrime :: Gen Integer
genPrime = chooseAny `suchThat` (isJust . isPrime)
genRationalBoundedArguments :: Integer -> Gen Rational
genRationalBoundedArguments n = (%) <$>
    chooseInteger (-n, n) <*>
    suchThat (chooseInteger (-n, n)) (/=0) 
genRationalTriple :: Integer -> Gen (Rational, Rational, Rational)
genRationalTriple n = (,,) <$>
    genRationalBoundedArguments n <*>
    genRationalBoundedArguments n <*>
    genRationalBoundedArguments n
genInertRational :: Integer -> Gen Rational
genInertRational p = (%) <$>
    (chooseAny `suchThat` ((/= 0) . (`mod` p))) <*>
    (chooseAny `suchThat` ((/= 0) . (`mod` p)))
genBoundedInertRational :: Integer -> Integer -> Gen Rational
genBoundedInertRational p n = (%) <$>
    (chooseInteger (-n, n) `suchThat` ((/= 0) . (`mod` p))) <*>
    (chooseInteger (-n, n) `suchThat` ((/= 0) . (`mod` p)))
