module PreliminaryNumberTheorySpec where

import Test.Hspec
import Data.Ratio

import PreliminaryNumberTheory

spec :: Spec
spec = do
    describe "PreliminaryNumberTheory.primesUpTo" $ do
        it "returns prime numbers up to 13" $ do
            primesUpTo 13 `shouldBe` ([2, 3, 5, 7, 11, 13] :: [Integer])
        it "returns prime numbers up to 14" $ do
            primesUpTo 14 `shouldBe` ([2, 3, 5, 7, 11, 13] :: [Integer])

    describe "PreliminaryNumberTheory.isSquare" $ do
        it "correctly identifies square" $ do
            isSquare (169 % 4) `shouldBe` True
        it "correctly identifies non-square" $ do
            isSquare ((-169) % 4) `shouldBe` False

    describe "PreliminaryNumberTheory.isSquareModN" $ do
        it "correctly identifies square" $ do
            isSquareModN 5 4 `shouldBe` True
        it "correctly identifies non-square" $ do
            isSquareModN 7 6 `shouldBe` False

    describe "PreliminaryNumberTheory.rationalQuadraticRoots" $ do
        it "returns roots of quadratic with distinct roots" $ do
            rationalQuadraticRoots ((-23) % 12) (5 % 12) `shouldMatchList` [1 % 4, 5 % 3]
        it "returns roots of quadratic with indistinct roots" $ do
            rationalQuadraticRoots ((-10) % 3) (25 % 9) `shouldMatchList` [5 % 3, 5 % 3]

    describe "PreliminaryNumberTheory.rationalCubicRoots" $ do
        it "returns roots of cubic with three distinct roots" $ do
            rationalCubicRoots ((-131) % 12) (53 % 3) ((-15) % 4) `shouldMatchList` [1 % 4, 5 % 3, 9 % 1]
        it "returns roots of cubic with two distinct roots" $ do
            rationalCubicRoots ((-43) % 12) (65 % 18) ((-25) % 36) `shouldMatchList` [1 % 4, 5 % 3, 5 % 3]
        it "returns roots of cubic with one distinct roots" $ do
            rationalCubicRoots ((-5) % 1) (25 % 3) ((-125) % 27) `shouldMatchList` [5 % 3, 5 % 3, 5 % 3]
