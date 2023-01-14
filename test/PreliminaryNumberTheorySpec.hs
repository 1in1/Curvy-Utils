module PreliminaryNumberTheorySpec where

import Control.Lens
import Data.Ratio
import Data.Set (Set)
import Debug.Trace
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

spec :: Spec
spec = do
    describe "PreliminaryNumberTheory.squareRootRational" $ do
        it "returns the correct square root" $ property $
            (==) <$> squareRootRational . (^2) <*> Just . abs

    describe "PreliminaryNumberTheory.cubeRootRational" $ do
        it "returns the correct cube root" $ property $
            (==) <$> cubeRootRational . (^3) <*> Just

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
        it "does not claim 2n and n are both square, unless 0" $ property
            ((\ a -> (a == 0) || not (isSquareRational a && isSquareRational ((2%1)*a))) :: Rational -> Bool)

    describe "PreliminaryNumberTheory.isSquareModN" $ do
        it "correctly identifies square" $ do
            isSquareModN 5 4 `shouldBe` True
        it "correctly identifies non-square" $ do
            isSquareModN 7 6 `shouldBe` False

    describe "PreliminaryNumberTheory.rationalQuadraticRoots" $ do
        it "returns roots of a quadratic by property" $ property $
            (==) <$> 
            Set.fromList . uncurry rationalQuadraticRoots . uncurry quadraticFromRoots <*> 
            Set.fromList . (^..each)

    describe "PreliminaryNumberTheory.rationalCubicRoots" $ do
        it "returns roots of cubic with three distinct roots" $ do
            rationalCubicRoots ((-131) % 12) (53 % 3) ((-15) % 4) `shouldMatchList` [1 % 4, 5 % 3, 9 % 1]
        it "returns roots of cubic with two distinct roots" $ do
            rationalCubicRoots ((-43) % 12) (65 % 18) ((-25) % 36) `shouldMatchList` [1 % 4, 5 % 3, 5 % 3]
        it "returns roots of cubic with one distinct roots" $ do
            rationalCubicRoots ((-5) % 1) (25 % 3) ((-125) % 27) `shouldMatchList` [5 % 3, 5 % 3, 5 % 3]
