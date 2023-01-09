{-# LANGUAGE DataKinds #-}
module AlgorithmsSpec where

import GHC.TypeLits
import Data.Proxy
import Test.Hspec
import Test.QuickCheck
import Data.Ratio

import PreliminaryNumberTheory
import EllipticCurves
import Algorithms

curve = Curve 0 0 0 (-43) 166 :: Curve Rational
p = Planar 3 8 :: ProjectivePoint Rational
curve2 = Curve 1 1 (-1) (-3) 3 :: Curve Rational
p2 = Planar 1 0 :: ProjectivePoint Rational
generalCubic = GeneralCubic 2 0 0 3 0 0 9 0 9

spec :: Spec
spec = do
    describe "Algorithms.quotientCurve" $ do
        it "correctly reduces a curve w.r.t. a finite subgroup of points" $ do
            quotientCurve curve2 (take 7 $ iterate (ellipticAddition curve2 p2) p2) `shouldBe` Curve 1 1 (-1) (-213) (-1257)

    describe "Algorithms.isRationalTorsion" $ do
        it "identifies a rational torsion point" $ do
            isRationalTorsion curve p `shouldBe` True

    describe "Algorithms.lutzNagelRationalTorsion" $ do
        it "identifies all rational torsion points" $ do
            lutzNagelRationalTorsion curve `shouldMatchList` [
                Infinity, 
                Planar 3 8,
                Planar (-5) 16,
                Planar 11 32,
                Planar 3 (-8),
                Planar (-5) (-16), 
                Planar 11 (-32)
                ]

    describe "Algorithms.rational2Torsion" $ do
        it "identifies the rational torsion points of order dividing 2" $ do
            rational2Torsion curve `shouldMatchList` [Infinity]

    describe "Algorithms.singularities" $ do
        it "identifies singularities on the curve" $ do
            singularities (Curve 0 0 1 0 0 :: Curve (PrimeFieldElem 5)) `shouldMatchList` [
                Planar (PrimeFieldElem 0) (PrimeFieldElem 0)
                ]

    describe "Algorithms.nagellsAlgorithm" $ do
        it "converts a general cubic into weierstrass form" $ do
            nagellsAlgorithm generalCubic `shouldBe` (Curve 0 0 0 0 ((-432)*(2^2)*(3^4)) :: Curve Rational)

    -- Known currently failing - algorithm heuristics currently insufficient
    describe "Algorithms.rankOfImageOfAlpha" $ do
        it "correctly computes the rank of alpha's image" $ do
            rankOfImageOfAlpha (Curve 0 0 0 20 0) `shouldBe` 1
        it "correctly computes the rank of alpha's image" $ do
            rankOfImageOfAlpha (Curve 0 0 0 1 0) `shouldBe` 0
        it "correctly computes the rank of alpha's image" $ do
            rankOfImageOfAlpha (Curve 0 0 0 (-5) 0) `shouldBe` 1

    -- Known currently failing - algorithm heuristics currently insufficient
    describe "Algorithms.rationalRank" $ do
        it "correctly deduces rank of a curve" $ do
            rationalRank (Curve 0 0 0 1 0) `shouldBe` 0
        it "correctly deduces rank of a curve" $ do
            rationalRank (Curve 0 0 0 (-1) 0) `shouldBe` 2
