module Data.EllipticCurves.Algorithms.RationalCurvesSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Data.EllipticCurves
import Data.EllipticCurves.Algorithms.RationalCurves
import Util

spec :: Spec
spec = do
    describe "Algorithms.RationalCurves.isRationalTorsion" $ do
        it "identifies a rational torsion point" $ do
            isRationalTorsion curve p `shouldBe` True

    describe "Algorithms.RationalCurves.lutzNagellRationalTorsion" $ do
        it "identifies all rational torsion points" $ do
            lutzNagellRationalTorsion curve `shouldMatchList` [
                Infinity, 
                Planar 3 8,
                Planar (-5) 16,
                Planar 11 32,
                Planar 3 (-8),
                Planar (-5) (-16), 
                Planar 11 (-32)
                ]

    describe "Algorithms.RationalCurves.rational2Torsion" $ do
        it "identifies the rational torsion points of order dividing 2" $ do
            rational2Torsion curve `shouldMatchList` [Infinity]

    describe "Algorithms.RationalCurves.rankOfImageOfAlpha" $ do
        it "correctly computes the rank of alpha's image (a4=-1)" $ do
            rankOfImageOfAlpha (Curve 0 0 0 (-1) 0) `shouldBe` Just 1
        it "correctly computes the rank of alpha's image (a4=4)" $ do
            rankOfImageOfAlpha (Curve 0 0 0 4 0) `shouldBe` Just 1
        it "correctly computes the rank of alpha's image (a4=-5)" $ do
            rankOfImageOfAlpha (Curve 0 0 0 (-5) 0) `shouldBe` Just 2
        it "correctly computes the rank of alpha's image (a4=20)" $ do
            rankOfImageOfAlpha (Curve 0 0 0 20 0) `shouldBe` Just 1

    describe "Algorithms.RationalCurves.rationalRank" $ do
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

    -- We test this functionality with friendly reduced curves
    describe "Algorithms.RationalCurves.reductionType" $ do
        it "correctly identifies curves of good reduction" $
            forAll (
                (,,) <$> 
                genBoundedInertRational 5 10 <*> 
                genBoundedInertRational 5 10 <*>
                chooseInteger (1, 10)
            ) $
            (== good) . 
            reductionType .
            (\(a, b, v) -> Curve 0 0 0 a (b * (5^^v)))
        it "correctly identifies curves of bad additive reduction" $
            forAll (
                (,,) <$> 
                genBoundedInertRational 5 10 <*> 
                genBoundedInertRational 5 10 <*>
                chooseInteger (1, 10)
            ) $
            (== additive) . 
            reductionType .
            (\(a, b, v) -> Curve 0 0 0 (5*a) (b * (5^^v)))
        it "correctly identifies curves of bad multiplicative split reduction" $ do
            reductionType (Curve 0 0 (-1) 0 35) `shouldBe` 
                (RationalPrimeReduction $ BadReduction $ MultiplicativeReduction SplitMultiplicativeReduction :: RationalPrimeReduction 5)
        it "correctly identifies curves of bad multiplicative non-split reduction" $ do
            reductionType (Curve 0 0 (-1) 0 35) `shouldBe` 
                (RationalPrimeReduction $ BadReduction $ MultiplicativeReduction NonSplitMultiplicativeReduction :: RationalPrimeReduction 7)

    where
    curve = Curve 0 0 0 (-43) 166 :: Curve Rational
    p = Planar 3 8 :: ProjectivePoint Rational
    good = RationalPrimeReduction GoodReduction :: RationalPrimeReduction 5
    additive = RationalPrimeReduction (BadReduction AdditiveReduction) :: RationalPrimeReduction 5
