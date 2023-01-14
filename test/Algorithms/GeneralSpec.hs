module Algorithms.GeneralSpec where

import GHC.TypeLits
import Data.Proxy
import Test.Hspec
import Test.QuickCheck
import Data.Ratio

import PreliminaryNumberTheory
import EllipticCurves
import Algorithms.General

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

    describe "Algorithms.nagellsAlgorithm" $ do
        it "converts a general cubic into weierstrass form" $ do
            nagellsAlgorithm generalCubic `shouldBe` (Curve 0 0 0 0 ((-432)*(2^2)*(3^4)) :: Curve Rational)
