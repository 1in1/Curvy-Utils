{-# LANGUAGE ScopedTypeVariables, KindSignatures, DataKinds #-}
module EllipticCurves where

import Control.Applicative
import Control.Exception
import Data.List
import Data.Ratio
import Debug.Trace

import PreliminaryNumberTheory

-- Define the structure of an elliptic curve. Any such curve can be given in Weierstrass form

-- Weierstrass form: y^2 + a1 xy + a3 y = x^3 + a2 x^2 + a4 x + a6
data Curve k = Curve { a1 :: k, a3 :: k, a2 :: k, a4 :: k, a6 :: k } deriving (Show, Eq)
-- In Weierstrass form, the only point not in the affine plane V(Z-1) is (0:1:0). We denote this Infinity here
data ProjectivePoint k = Planar k k | Infinity deriving (Show, Eq)

-- Go between homogeneous coordinates and planar ones
homogenizeCoords :: (Num k) => ProjectivePoint k -> (k, k, k)
homogenizeCoords Infinity = (0, 1, 0)
homogenizeCoords (Planar x y) = (x, y, 1)

dehomogenizeCoords :: (Eq k, Num k, Fractional k) => (k, k, k) -> ProjectivePoint k
dehomogenizeCoords (0, 1, 0) = Infinity
dehomogenizeCoords (x, y, z) = Planar (x/z) (y/z)

-- Define the group law on the curve
-- Addition with respect to the group law
ellipticAddition :: (Eq k, Fractional k) => Curve k -> ProjectivePoint k -> ProjectivePoint k -> ProjectivePoint k
ellipticAddition (Curve a1 a3 a2 a4 a6) (Planar x1 y1) (Planar x2 y2)
    | (x1 == x2) && (y1 + y2 + a1*x2 + a3 == 0) = Infinity
    | otherwise = Planar x3 y3 where
        lambda | x1 /= x2 = (y2 - y1)/(x2 - x1)
               | otherwise = (3*x1^2 + 2*a2*x1 + a4 - a1*y1)/(2*y1 + a1*x1 + a3)
        nu | x1 /= x2 = (y1*x2 - y2*x1)/(x2 - x1)
           | otherwise = (-x1^3 + a4*x1 + 2*a6 - a3*y1)/(2*y1 + a1*x1 + a3)
        x3 = lambda^2 + a1*lambda - a2 - x1 - x2
        y3 = -(lambda + a1)*x3 - nu - a3
ellipticAddition _ Infinity point = point
ellipticAddition _ point Infinity = point

-- Inverses with respect to the group law
ellipticInverse :: (Fractional k) => Curve k -> ProjectivePoint k -> ProjectivePoint k
ellipticInverse _ Infinity = Infinity
ellipticInverse (Curve a1 a3 a2 a4 a6) (Planar x y) = Planar x (-y - a1*x - a3)

-- Utility function; apply the addition function n times
ellipticNMult :: (Eq k, Fractional k) => Curve k -> Int -> ProjectivePoint k -> ProjectivePoint k
ellipticNMult curve n = foldl1 (ellipticAddition curve) . replicate n

-- Requires char K /= 2 - cf Silverman p. 42
weierstrassDiscriminant :: forall k . (Eq k, Num k, Fractional k) => Curve k -> k
weierstrassDiscriminant (Curve a1 a3 a2 a4 a6) | (0 :: k) /= (2 :: k) = d where
    b2 = a1^2 + 4*a2 -- typo in Silverman!! It gives a1^2 + 4*a4
    b4 = 2*a4 + a1*a3
    b6 = a3^2 + 4*a6
    b8 = (a1^2)*a6 + 4*a2*a6 - a1*a3*a4 + a2*(a3^2) - a4^2
    d = -(b2^2)*b8 - 8*(b4^3) - 27*(b6^2) + 9*b2*b4*b6

jInvariant :: (Eq k, Fractional k) => Curve k -> Maybe k
jInvariant (Curve a1 a3 a2 a4 a6)
    | d == 0 = Nothing
    | otherwise = Just j where
    b2 = a1^2 + 4*a2
    b4 = 2*a4 + a1*a3
    c4 = b2^2 - 24*b4
    d = weierstrassDiscriminant (Curve a1 a3 a2 a4 a6)
    j = (c4^3)/d

-- Given a ProjectivePoint, is it on the given Curve?
curveContainsPoint :: (Eq k, Num k) => Curve k -> ProjectivePoint k -> Bool
curveContainsPoint _ Infinity = True
curveContainsPoint (Curve a1 a3 a2 a4 a6) (Planar x y) = y^2 + a1*x*y + a3*y - x^3 - a2*x^2 - a4*x - a6 == 0

