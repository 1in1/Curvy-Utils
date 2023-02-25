module Data.EllipticCurvesSpec (spec) where

import Data.Ratio
import Test.Hspec
import Test.QuickCheck

import Data.EllipticCurves
import Data.EllipticCurves.PreliminaryNumberTheory

spec :: Spec
spec = do
    describe "EllipticCurves.ellipticAddition" $ do
        it "correctly adds points along a curve" $ do
            ellipticAddition (Curve 0 0 0 0 17 :: Curve Rational) (Planar (-1) 4) (Planar 2 5 ) 
            `shouldBe` Planar ((-8) % 9) ((-109) %  27)

    describe "EllipticCurves.ellipticNMult" $ do
        it "correctly doubles points" $ do
            ellipticNMult curve 2 p `shouldBe` Planar (-5) (-16)
        it "correctly multiplies by 8" $ do
            ellipticNMult curve 8 p `shouldBe` p

    describe "EllipticCurves.ellipticInverse" $ do
        it "correctly finds inverses of points on curves" $ do
            ellipticInverse curve p `shouldBe` ellipticNMult curve 6 p

    describe "EllipticCurves.weierstrassDiscriminant" $ do
        it "correctly computes the discriminant of a non-singular curve" $ do
            weierstrassDiscriminant (Curve 0 0 (-4) 0 16 :: Curve Rational) `shouldBe` (-45056)
        it "correctly computes the discriminant of a singular curve" $ do
            weierstrassDiscriminant (Curve 0 0 1 0 0 :: Curve Rational) `shouldBe` 0
        it "correctly computes the discriminant of a curve of form y^2 = x^3 + ax + b" $ property
            (\ (a,b) -> weierstrassDiscriminant (Curve 0 0 0 a b) == simpleDisc a b)

    describe "EllipticCurves.jInvariant" $ do
        it "correctly computes the j invariant of a non-singular curve" $ do
            jInvariant (Curve 0 0 (-4) 0 16 :: Curve Rational) `shouldBe` Just ((-4096) % 11)
        it "correctly computes the j invariant of a curve of form y^2 = x^3 + ax + b" $ property
            (\ (a,b) -> jInvariant (Curve 0 0 0 a b) == simpleJ a b)

    describe "EllipticCurves.curveContainsPoint" $ do
        it "identifies when a point is on the curve" $ do
            curveContainsPoint curve p `shouldBe` True
        it "identifies when a point is not on the curve" $ do
            curveContainsPoint curve (Planar 3 80) `shouldBe` False

    where
    curve = Curve 0 0 0 (-43) 166 :: Curve Rational
    p = Planar 3 8 :: ProjectivePoint Rational
    simpleDisc :: Rational -> Rational -> Rational
    simpleDisc a b = (-16) * (4 * (a^i3) + 27 * (b^i2))
    simpleJ :: Rational -> Rational -> Maybe Rational
    simpleJ a b 
        | 0 == simpleDisc a b = Nothing 
        | otherwise = Just $ ((2^i8) * (3^i3) * (a^i3)) / (4 * (a^i3) + 27 * (b^i2))
