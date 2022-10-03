module EllipticCurves where

import Control.Applicative
import Control.Exception
import Data.List
import Data.Ratio
import Debug.Trace

import PreliminaryNumberTheory

-- Define the structure of an elliptic curve. Any such curve can be given in Weierstrass form

-- Weierstrass form: y^2 + a1 xy + a3 y = x^3 + a2 x^2 + a4 x + a6
-- They are given here in the order a1 a3 a2 a4 a6
data Curve k = Curve { a1 :: k, a3 :: k, a2 :: k, a4 :: k, a6 :: k } deriving (Show, Eq)
-- In Weierstrass form, the only point not in the affine plane V(Z-1) is (0:1:0). We denote this Infinity here
data ProjectivePoint k = Planar k k
                       | Infinity deriving (Show, Eq)


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
ellipticNMult curve n = foldl (ellipticAddition curve) Infinity . replicate n



-- Requires char K /= 2 - cf Silverman p. 42
weierstrassDiscriminant :: (Eq k, Fractional k) => Curve k -> k
weierstrassDiscriminant (Curve a1 a3 a2 a4 a6) = d where -- | ((fromRational (2%1))::k) /= ((fromRational (0%1))::k) = d where
    b2 = a1^2 + 4*a2 -- typo in Silverman?? It gives a1^2 + 4*a4
    b4 = 2*a4 + a1*a3
    b6 = a3^2 + 4*a6
    b8 = (a1^2)*a6 + 4*a2*a6 - a1*a3*a4 + a2*(a3^2) - a4^2
    d = -(b2^2)*b8 - 8*(b4^3) - 27*(b6^2) + 9*b2*b4*b6

jInvariant :: (Eq k, Fractional k) => Curve k -> k
jInvariant (Curve a1 a3 a2 a4 a6) = j where
    b2 = a1^2 + 4*a2
    b4 = 2*a4 + a1*a3
    c4 = b2^2 - 24*b4
    d = weierstrassDiscriminant (Curve a1 a3 a2 a4 a6)
    j = (c4^3)/d





eval :: (Fractional k) => Curve k -> k -> k -> k
eval (Curve a1 a3 a2 a4 a6) x y = y^2 + a1*x*y + a3*y - x^3 - a2*x^2 - a4*x - a6

-- Given a ProjectivePoint, is it on the given Curve?
-- Holds iff this value is 0
ellipticEval :: (Fractional k) => Curve k -> ProjectivePoint k -> k
ellipticEval _ Infinity = 0
ellipticEval curve (Planar x y) = eval curve x y


-- Ensure a point is on the curve
data CurvePoint k = CurvePoint (Curve k) (ProjectivePoint k)
-- Smart constructor (check about forcing this method to be used)
curvePoint :: (Eq k, Fractional k) => Curve k -> ProjectivePoint k -> CurvePoint k
curvePoint curve p = assert (0 == ellipticEval curve p) $ CurvePoint curve p

-- -- Given p, reduce the (RATIONAL) curve to get a minimal disc
-- padicMinimalDiscriminant :: Curve Rational -> Integer -> Rational
-- padicMinimalDiscriminant curve p = d where
--     disc = weierstrassDiscriminant curve
--     d = until ((/= 0) . (`mod` p) . denominator) (*(p^12)) $ 
--         until ((/= 0) . (`mod` p) . numerator) (/(p^12)) disc


-- Reduce a rational curve to an integral one, with minimal coefficients
-- Dividing back down is taking a fucking long time :(
minimalIntegralForm :: Curve Rational -> Curve Rational
minimalIntegralForm (Curve a1 a3 a2 a4 a6) = Curve a1' a3' a2' a4' a6' where
    [a1', a3', a2', a4', a6'] = 
        -- until stopCondition updateDownwards $
        until (all ((== 1) . denominator)) update [a1, a3, a2, a4, a6]
   
    stopCondition = not . (any <$> dividesSufficiently <*> (foldl union [] . map primeFactors)) . map numerator

    update :: [Rational] -> [Rational]
    update [a1', a3', a2', a4', a6'] = [a1'*k, a3'*k^3, a2'*k^2, a4'*k^4, a6'*k^6] where
        k = getSixthPowerOfPrimeDenominatorDivs [a1', a3', a2', a4', a6']

    dividesSufficiently :: [Integer] -> Integer -> Bool
    dividesSufficiently [a1', a3', a2', a4', a6'] k =
        ((== 0) . mod a1') k &&
        ((== 0) . mod a3' . (^3)) k &&
        ((== 0) . mod a2' . (^2)) k &&
        ((== 0) . mod a4' . (^4)) k &&
        ((== 0) . mod a6' . (^6)) k
        
    updateDownwards :: [Rational] -> [Rational]
    updateDownwards [a1', a3', a2', a4', a6'] = [a1'/k, a3'/(k^3), a2'/(k^2), a4'/(k^4), a6'/(k^6)] where
        k = toRational $
            product $
            filter (dividesSufficiently (map numerator [a1', a3', a2', a4', a6'])) $
            primeFactors $
            foldl lcm 1 $
            filter (/= 0) $
            map numerator [a1', a3', a2', a4', a6']

    -- Take the prime factors of the lcm of the denominators, and multiply the whole equation by thier product ^6 
    getSixthPowerOfPrimeDenominatorDivs :: [Rational] -> Rational
    getSixthPowerOfPrimeDenominatorDivs = (^6) . toRational . product . primeFactors . foldl lcm 1 . filter (/= 0) . map denominator






