module Algorithms.GeneralSpec where

import Data.Proxy
import Data.Ratio
import GHC.TypeLits
import Test.Hspec
import Test.QuickCheck

import PreliminaryNumberTheory
import EllipticCurves
import Algorithms.General

curve = Curve 0 0 0 (-43) 166 :: Curve Rational
p = Planar 3 8 :: ProjectivePoint Rational
curve2 = Curve 1 1 (-1) (-3) 3 :: Curve Rational
p2 = Planar 1 0 :: ProjectivePoint Rational
generalCubic = GeneralCubic 2 0 0 3 0 0 9 0 9 0 (0,0)
generalCubicWithConstant = GeneralCubic 1 0 0 1 0 0 0 0 0 (-1) (0,1)

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
            nagellsAlgorithm generalCubic `shouldBe` (Curve 0 0 0 0 ((-432)*(2^2)*(3^4)) :: Curve Rational)
        it "converts a general cubic with non-zero constant term into weierstrass form" $ do
            nagellsAlgorithm generalCubicWithConstant `shouldBe` (Curve 0 0 0 0 (-432) :: Curve Rational)
