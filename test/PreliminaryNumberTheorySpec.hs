module PreliminaryNumberTheorySpec where

import Control.Arrow
import Control.Lens
import Data.Maybe
import Data.Ratio
import Data.Set (Set)
import Math.NumberTheory.Primes
import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as Set

import PreliminaryNumberTheory

quadraticFromRoots :: Rational -> Rational -> (Rational, Rational)
quadraticFromRoots a b = (-(a+b), a*b)
cubicFromRoots :: Rational -> Rational -> Rational -> (Rational, Rational, Rational)
cubicFromRoots a b c = (-(a+b+c), a*b + a*c+ b*c, -(a*b*c))

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

genPrime = chooseAny `suchThat` (isJust . isPrime) :: Gen Integer
genRationalBoundedArguments :: Integer -> Gen Rational
genRationalBoundedArguments n = (%) <$>
    chooseInteger (-n, n) <*>
    suchThat (chooseInteger (-n, n)) (/=0) 
genRationalPair :: Integer -> Gen (Rational, Rational)
genRationalPair n = (,) <$>
    genRationalBoundedArguments n <*>
    genRationalBoundedArguments n
genRationalTriple :: Integer -> Gen (Rational, Rational, Rational)
genRationalTriple n = (,,) <$>
    genRationalBoundedArguments n <*>
    genRationalBoundedArguments n <*>
    genRationalBoundedArguments n

spec :: Spec
spec = do
    describe "PreliminaryNumberTheory.squareRootRational" $ do
        it "returns the correct square root" $ property $
            (==) <$> squareRootRational . (^2) <*> Just . abs
        it "returns Nothing for non-square rationals" $
            forAll ((,) <$> chooseAny `suchThat` (isJust . isPrime) <*> genRationalBoundedArguments 1000) $
            isNothing . squareRootRational . uncurry (*) . ((% 1) *** (^2))

    describe "PreliminaryNumberTheory.cubeRootRational" $ do
        it "returns the correct cube root" $ property $
            (==) <$> cubeRootRational . (^3) <*> Just
        it "returns Nothing for non-cube rationals" $
            forAll ((,) <$> genPrime <*> genRationalBoundedArguments 1000) $
            isNothing . cubeRootRational . uncurry (*) . ((% 1) *** (^3))

    describe "PreliminaryNumberTheory.primesUpTo" $ do
        it "returns prime numbers up to 13" $ do
            primesUpTo 13 `shouldBe` ([2, 3, 5, 7, 11, 13] :: [Integer])
        it "returns prime numbers up to 14" $ do
            primesUpTo 14 `shouldBe` ([2, 3, 5, 7, 11, 13] :: [Integer])

    describe "PreliminaryNumberTheory.isSquareRational" $ do
        it "correctly identifies square" $ property $
            (== True) . isSquareRational . (^2)
        it "correctly identifies non-square" $ do
            isSquareRational ((-169) % 4) `shouldBe` False
        it "does not claim 2n and n are both square (unless 0)" $
            forAll (genRationalBoundedArguments 1000 `suchThat` (/= 0))
            ((\ a -> not (isSquareRational a && isSquareRational ((2%1)*a))) :: Rational -> Bool)

    describe "PreliminaryNumberTheory.isSquareModN" $ do
        it "correctly identifies square" $ do
            isSquareModN 5 4 `shouldBe` True
        it "correctly identifies non-square" $ do
            isSquareModN 7 6 `shouldBe` False

    describe "PreliminaryNumberTheory.rationalQuadraticRoots" $ do
        it "returns both roots of a quadratic with rational roots" $ property $
            (==) <$> 
            Set.fromList . uncurry rationalQuadraticRoots . uncurry quadraticFromRoots <*> 
            Set.fromList . (^..each)
        it "returns an empty list for a quadratic with irrational real roots" $
            forAll genPrime $
            \ p -> null $ rationalQuadraticRoots 0 (- (p%1))
        it "returns an empty list for a quadratic with complex roots" $
            forAll (genRationalBoundedArguments 1000 `suchThat` (/= 0)) $
            \ n -> null $ rationalQuadraticRoots 0 (abs n)

    describe "PreliminaryNumberTheory.rationalCubicRoots" $ do
        it "returns roots of cubic with two distinct roots" $ do
            rationalCubicRoots ((-43) % 12) (65 % 18) ((-25) % 36) `shouldMatchList` [1 % 4, 5 % 3, 5 % 3]
        it "returns roots of cubic with one distinct roots" $ do
            rationalCubicRoots ((-5) % 1) (25 % 3) ((-125) % 27) `shouldMatchList` [5 % 3, 5 % 3, 5 % 3]
        it "returns roots of monic cubic with three rational roots" $
            forAll (genRationalTriple 10000) $
            (==) <$> 
            Set.fromList . uncurry3 rationalCubicRoots . uncurry3 cubicFromRoots <*> 
            Set.fromList . (^..each)
        it "returns rational root of cubic with two irrational real roots" $
            forAll ((,) <$> genPrime <*> genRationalBoundedArguments 10000)
            (\(p, r) -> [r] == rationalCubicRoots (-r) (-p%1) ((p%1)*r))
        it "returns rational root of cubic with two complex roots" $
            forAll ((,) <$> genPrime <*> genRationalBoundedArguments 10000) $
            \ (n, r) -> [r] == rationalCubicRoots (-r) (n%1) (-(n%1)*r)
        it "returns an empty list for a cubic with no rational roots" $
            forAll genPrime $
            \ p -> null $ rationalCubicRoots 0 0 (p%1)
