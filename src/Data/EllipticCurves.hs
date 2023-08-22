module Data.EllipticCurves (
      Curve (..)
    , ProjectivePoint (..)
    , CurvePoint
    , curvePoint
    , homogenizeCoords
    , dehomogenizeCoords
    , planarPoint
    , ellipticAddition
    , ellipticInverse
    , ellipticNMult
    , b2, b4, b6, b8
    , c4, c6
    , weierstrassDiscriminant
    , jInvariant
    , ReductionType (..)
    , BadReductionType (..)
    , MultiplicativeReductionType (..)
    , curveContainsPoint
    ) where

import Data.EllipticCurves.PreliminaryNumberTheory

-- Define the structure of an elliptic curve. Any such curve can be given in Weierstrass form

-- Weierstrass form: y^2 + a1 xy + a3 y = x^3 + a2 x^2 + a4 x + a6
data Curve k = Curve { a1 :: k, a3 :: k, a2 :: k, a4 :: k, a6 :: k } deriving (Show, Eq)
-- In Weierstrass form, the only point not in the affine plane V(Z-1) is (0:1:0). We denote this Infinity here
data ProjectivePoint k = Planar k k | Infinity deriving (Show, Eq)
-- A point with its given curve
data CurvePoint k = CurvePoint (Curve k) (ProjectivePoint k)
-- Smart constructor for points on a curve
curvePoint :: (Eq k, Num k) => Curve k -> ProjectivePoint k -> Maybe (CurvePoint k)
curvePoint c p | curveContainsPoint c p = Just (CurvePoint c p)
               | otherwise = Nothing

-- Go between homogeneous coordinates and planar ones
homogenizeCoords :: (Num k) => ProjectivePoint k -> (k, k, k)
homogenizeCoords Infinity = (0, 1, 0)
homogenizeCoords (Planar x y) = (x, y, 1)

dehomogenizeCoords :: (Eq k, Fractional k) => (k, k, k) -> ProjectivePoint k
dehomogenizeCoords (0, 1, 0) = Infinity
dehomogenizeCoords (x, y, z) = Planar (x/z) (y/z)

-- Get a planar point
planarPoint :: ProjectivePoint k -> Maybe (k, k)
planarPoint (Planar x y) = Just (x, y)
planarPoint Infinity     = Nothing

-- Define the group law on the curve
-- Addition with respect to the group law
ellipticAddition :: (Eq k, Fractional k) => Curve k -> ProjectivePoint k -> ProjectivePoint k -> ProjectivePoint k
ellipticAddition (Curve a1 a3 a2 a4 a6) (Planar x1 y1) (Planar x2 y2)
    | (x1 == x2) && (y1 + y2 + a1*x2 + a3 == 0) = Infinity
    | otherwise = Planar x3 y3 where
        lambda | x1 /= x2 = (y2 - y1)/(x2 - x1)
               | otherwise = (3*x1^i2 + 2*a2*x1 + a4 - a1*y1)/(2*y1 + a1*x1 + a3)
        nu | x1 /= x2 = (y1*x2 - y2*x1)/(x2 - x1)
           | otherwise = (-x1^i3 + a4*x1 + 2*a6 - a3*y1)/(2*y1 + a1*x1 + a3)
        x3 = lambda^i2 + a1*lambda - a2 - x1 - x2
        y3 = -(lambda + a1)*x3 - nu - a3
ellipticAddition _ Infinity point = point
ellipticAddition _ point Infinity = point

-- Inverses with respect to the group law
ellipticInverse :: (Fractional k) => Curve k -> ProjectivePoint k -> ProjectivePoint k
ellipticInverse _ Infinity = Infinity
ellipticInverse (Curve a1 a3 _ _ _) (Planar x y) = Planar x (-y - a1*x - a3)

-- Utility function; apply the addition function n times
ellipticNMult :: (Eq k, Fractional k) => Curve k -> Int -> ProjectivePoint k -> ProjectivePoint k
ellipticNMult curve n = foldl1 (ellipticAddition curve) . replicate n

-- We expose the following derived values
b2 :: (Num k) => Curve k -> k
b2 (Curve a1 _  a2 _  _ ) = a1^i2 + 4*a2
b4 :: (Num k) => Curve k -> k
b4 (Curve a1 a3 _  a4 _ ) = 2*a4 + a1*a3
b6 :: (Num k) => Curve k -> k
b6 (Curve _  a3 _  _  a6) = a3^i2 + 4*a6
b8 :: (Num k) => Curve k -> k
b8 (Curve a1 a3 a2 a4 a6) = (a1^i2)*a6 + 4*a2*a6 - a1*a3*a4 + a2*(a3^i2) - a4^i2
c4 :: (Num k) => Curve k -> k
c4 curve = b2 curve^i2 - 24*b4 curve
c6 :: (Num k) => Curve k -> k
c6 curve = - b2'^i3 + 36*b2'*b4' - 216*b6' where
    b2' = b2 curve
    b4' = b4 curve
    b6' = b6 curve

discriminantAndJInvariant :: (Eq k, Fractional k) => Curve k -> (k, Maybe k)
discriminantAndJInvariant curve = (disc, j) where
    disc = -(b2'^i2)*b8' - 8*b4'^i3 - 27*b6'^i2 + 9*b2'*b4'*b6' where
        b2' = b2 curve
        b4' = b4 curve
        b6' = b6 curve
        b8' = b8 curve
    j | disc == 0 = Nothing
      | otherwise = Just $ (c4 curve^i3)/disc

weierstrassDiscriminant :: (Eq k, Fractional k) => Curve k -> k
weierstrassDiscriminant = fst . discriminantAndJInvariant

jInvariant :: (Eq k, Fractional k) => Curve k -> Maybe k
jInvariant = snd . discriminantAndJInvariant

-- Types of reductions, for curves over number fields being reduced to residue fields
data ReductionType =
    GoodReduction |
    BadReduction BadReductionType
    deriving (Eq, Show)
-- Additive reduction is also known as unstable reduction
--   It occurs when the non-singular group is the additive group of the residue field
-- Multiplicative reduction is also known as stable reduction
--   It occurs when the non-singular group is the multiplicative group of the residue field
data BadReductionType =
    AdditiveReduction |
    MultiplicativeReduction MultiplicativeReductionType
    deriving (Eq, Show)
data MultiplicativeReductionType =
    SplitMultiplicativeReduction |
    NonSplitMultiplicativeReduction
    deriving (Eq, Show)

-- Given a ProjectivePoint, is it on the given Curve?
curveContainsPoint :: (Eq k, Num k) => Curve k -> ProjectivePoint k -> Bool
curveContainsPoint _ Infinity = True
curveContainsPoint (Curve a1 a3 a2 a4 a6) (Planar x y) = y^i2 + a1*x*y + a3*y - x^i3 - a2*x^i2 - a4*x - a6 == 0
