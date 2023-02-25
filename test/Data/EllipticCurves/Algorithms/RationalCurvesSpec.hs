module Data.EllipticCurves.Algorithms.RationalCurvesSpec (spec) where

import Test.Hspec

import Data.EllipticCurves
import Data.EllipticCurves.Algorithms.RationalCurves

spec :: Spec
spec = do
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

    describe "Algorithms.rankOfImageOfAlpha" $ do
        it "correctly computes the rank of alpha's image (a4=-1)" $ do
            rankOfImageOfAlpha (Curve 0 0 0 (-1) 0) `shouldBe` Just 1
        it "correctly computes the rank of alpha's image (a4=4)" $ do
            rankOfImageOfAlpha (Curve 0 0 0 4 0) `shouldBe` Just 1
        it "correctly computes the rank of alpha's image (a4=-5)" $ do
            rankOfImageOfAlpha (Curve 0 0 0 (-5) 0) `shouldBe` Just 2
        it "correctly computes the rank of alpha's image (a4=20)" $ do
            rankOfImageOfAlpha (Curve 0 0 0 20 0) `shouldBe` Just 1

    describe "Algorithms.rationalRank" $ do
        it "correctly deduces rank of a curve (a4=-1)" $ do
            rationalRank (Curve 0 0 0 (-1) 0) `shouldBe` Just 0
        it "correctly deduces rank of a curve (a4=-5)" $ do
            rationalRank (Curve 0 0 0 (-5) 0) `shouldBe` Just 1
        it "correctly deduces rank of a curve (a4=20)" $ do
            rationalRank (Curve 0 0 0 20 0) `shouldBe` Just 1
        it "correctly deduces rank of a p-curve: p=73" $ do
            rationalRank (Curve 0 0 0 73 0) `shouldBe` Just 2
        it "correctly deduces rank of a p-curve: p=89" $ do
            rationalRank (Curve 0 0 0 89 0) `shouldBe` Just 2
        -- Known that current algorithm heuristics currently insufficient
        -- Solution should be 0
        it "correctly deduces rank of a p-curve: p=17" $ do
            rationalRank (Curve 0 0 0 17 0) `shouldBe` Nothing
        -- Known that current algorithm heuristics currently insufficient
        -- Solution should be 0
        it "correctly deduces rank of a p-curve: p=41" $ do
            rationalRank (Curve 0 0 0 41 0) `shouldBe` Nothing

    where
    curve = Curve 0 0 0 (-43) 166 :: Curve Rational
    p = Planar 3 8 :: ProjectivePoint Rational
