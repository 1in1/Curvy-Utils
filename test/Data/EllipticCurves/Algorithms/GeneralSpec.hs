module Data.EllipticCurves.Algorithms.GeneralSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Data.EllipticCurves
import Data.EllipticCurves.Algorithms.General
import Data.EllipticCurves.PreliminaryNumberTheory

spec :: Spec
spec = do
    -- Note this is NOT true left inverse, because more than one substitution may give the same curve
    describe "Algorithms.General.isomorphismBetween" $ do
        it "is a left-inverse of the substitution function, up to sign of u" $ property
            (\(Positive u) r s t -> isomorphismBetween curve (substitute curve u r s t) == Just (u, r, s, t))

    describe "Algorithms.General.quotientCurve" $ do
        it "correctly reduces a curve w.r.t. a finite subgroup of points" $ do
            quotientCurve curve2 (take 7 $ iterate (ellipticAddition curve2 p2) p2) `shouldBe` Curve 1 1 (-1) (-213) (-1257)

    describe "Algorithms.General.nagellsAlgorithm" $ do
        it "converts a general cubic with zero constant term into weierstrass form" $ do
            nagellsAlgorithm generalCubic `shouldBe` (Curve 0 0 0 0 ((-432)*(2^i2)*(3^i4)) :: Curve Rational)
        it "converts a general cubic with non-zero constant term into weierstrass form" $ do
            nagellsAlgorithm generalCubicWithConstant `shouldBe` (Curve 0 0 0 0 (-432) :: Curve Rational)

    where
    curve = Curve 0 0 0 (-43) 166 :: Curve Rational
    curve2 = Curve 1 1 (-1) (-3) 3 :: Curve Rational
    p2 = Planar 1 0 :: ProjectivePoint Rational
    generalCubic = GeneralCubic 2 0 0 3 0 0 9 0 9 0 (0,0)
    generalCubicWithConstant = GeneralCubic 1 0 0 1 0 0 0 0 0 (-1) (0,1)
