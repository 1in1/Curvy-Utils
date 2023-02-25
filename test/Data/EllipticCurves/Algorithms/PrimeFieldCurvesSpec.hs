module Data.EllipticCurves.Algorithms.PrimeFieldCurvesSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Data.EllipticCurves
import Data.EllipticCurves.Algorithms.PrimeFieldCurves
import Data.EllipticCurves.PreliminaryNumberTheory

spec :: Spec
spec = do
    describe "Algorithms.PrimeFieldCurves.curvePointsOnFp" $ do
        it "correctly discovers all points on the finite field" $ do
            curvePointsOnFp (Curve 1 2 3 4 5 :: Curve (PrimeFieldElem 5)) `shouldMatchList`
                [ Infinity
                , Planar (PrimeFieldElem 0) (PrimeFieldElem 0)
                , Planar (PrimeFieldElem 0) (PrimeFieldElem 3)
                , Planar (PrimeFieldElem 1) (PrimeFieldElem 3)
                , Planar (PrimeFieldElem 1) (PrimeFieldElem 4)
                , Planar (PrimeFieldElem 3) (PrimeFieldElem 1)
                , Planar (PrimeFieldElem 3) (PrimeFieldElem 4)
                ]

    describe "Algorithms.PrimeFieldCurves.countCurvePointsOverExtension" $ do
        it "correctly counts the numer of points in low degree" $ do
            take 3 (countCurvePointsOverExtension (Curve 0 0 0 1 5 :: Curve (PrimeFieldElem 13))) `shouldMatchList`
                [ 9
                , 171
                , 6075
                ]

    describe "Algorithms.PrimeFieldCurves.singularities" $ do
        it "identifies the singularity on a curve y^2 = x^2(x - a) in FP_29" $ property
            ((\ a -> singularities (Curve 0 0 (fromRational $ toRational a) 0 0 :: Curve (PrimeFieldElem 29)) ==
                [ Planar (PrimeFieldElem 0) (PrimeFieldElem 0)
                ]) :: Integer -> Bool)
