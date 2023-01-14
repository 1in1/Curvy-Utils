module Algorithms.RationalCurves (
      isRationalTorsion
    , lutzNagelRationalTorsion
    , rational2Torsion
    , rankOfImageOfAlpha
    , rationalRank
    ) where

import Control.Applicative
import Control.Exception
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Ratio
import GHC.TypeLits

import PreliminaryNumberTheory
import EllipticCurves

-- We then need to confirm that these points are torsion. By Mazur, it is sufficient to check up to 12P
-- (we need not check 11 but we may as well compute it anyway)
isRationalTorsion :: Curve Rational -> ProjectivePoint Rational -> Bool
isRationalTorsion curve = (Infinity `elem`) . scanl1 (ellipticAddition curve) . replicate 11

-- Only defined for integer coefficients and a1 == a3 == 0
lutzNagelRationalTorsion :: Curve Rational -> [ProjectivePoint Rational]
lutzNagelRationalTorsion (Curve 0 0 a2 a4 a6) 
    | all ((== 1) . denominator) [a2, a4, a6] = Infinity:rationalPlanarTorsion where
        disc = abs $ numerator $ weierstrassDiscriminant (Curve 0 0 a2 a4 a6)
        factorsOfDisc = filter ((== 0) . mod disc . (^2)) [1..disc]
        possibleYValues = [0] ++ factorsOfDisc ++ map negate factorsOfDisc
       
        xValuesFromY y = map toRational $ rationalCubicRoots a2 a4 (a6 - y^2)
        rationalPlanarTorsion = 
            filter (isRationalTorsion (Curve 0 0 a2 a4 a6)) $
            filter (curveContainsPoint (Curve 0 0 a2 a4 a6)) $
            concatMap ((map <$> flip Planar <*> xValuesFromY) . toRational) possibleYValues
lutzNagelRationalTorsion _ = undefined

-- For any curve over Q with a1 = 0, there is a unique value of y for planar 2-torsion points
-- This implies there are at most 4 points of order dividing 2, including infinity
rational2Torsion :: Curve Rational -> [ProjectivePoint Rational]
rational2Torsion (Curve 0 a3 a2 a4 a6) = Infinity:planarPoints where
    y0 = -a3/2
    -- Search for x solving the elliptic at this point
    -- If we knew that we had integer coords, this would be significantly easier
    planarPoints = map (`Planar` y0) $ nub $ rationalCubicRoots a2 a4 (a6 - a3 - y0^2)

-- Given a curve E : y^2 = x^3 + ax^2 + bx, consider the homomorphism
-- alpha_E : E(Q) -> Q* / (Q*)^2
-- (x,y) |-> b if (x,y) = (0,0),
--           x otherwise
-- This isogeny yields data about the rank of the elliptic curve E
rankOfImageOfAlpha :: Curve Rational -> Maybe Integer
rankOfImageOfAlpha (Curve 0 0 a b 0) | all ((== 1) . denominator) [a, b] = r where
    -- Check that a, b are integer
    absB = (abs . numerator) b
    primeFactorOfB = primeFactors absB
    -- Also want to knock out b = 0 - in this case, the curve is degenerate
    basisForContainingSpace = (-1):primeFactorOfB

    subsets [] = [[]]
    subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

    elementsToCheck = map product $ subsets basisForContainingSpace
    
    -- Begin by throwing in the point from alpha((0,0))
    imagePointFromOrigin = head $ filter (not . isSquareRational . (/ b) . toRational) elementsToCheck

    imageElements :: 
        [Integer] -> 
        [Integer] -> 
        [Integer] -> 
        Maybe [Integer]
    imageElements [] certainIn _ = Just certainIn 
    imageElements (x:xs) certainIn certainOut 
        | x `elem` certainIn = imageElements xs certainIn certainOut
        | x `elem` certainOut = imageElements xs certainIn certainOut
        | isNothing xIsValid = Nothing
        | otherwise = imageElements xs (certainIn' xIsValid) (certainOut' xIsValid) where
            xIsValid = existsIntegerSolution x (numerator a) (round (b/ toRational x))
            certainIn' (Just valid) = if valid then nub (x:(certainIn ++ map (*x) certainIn)) else certainIn
            certainOut' (Just valid) = if valid then nub (certainOut ++ map (*x) certainOut) else nub (x:certainOut)

    imageElts = imageElements elementsToCheck [imagePointFromOrigin] []
    r = round . logBase 2 . fromIntegral . length <$> imageElts

-- Given a curve E : y^2 = x^3 + ax^2 + bx, we define a dual curve E' : y^2 = x^3 - 2ax^2 + (a^2 - 4b)x
-- Considering the homs alpha_E, alpha_E', we may deduce the rank of the curve
rationalRank :: Curve Rational -> Maybe Integer
rationalRank (Curve 0 0 a b 0) | all ((== 1) . denominator) [a, b] = subtract 2 <$> liftA2 (+) r1 r2 where
    r1 = rankOfImageOfAlpha (Curve 0 0 a b 0)
    r2 = rankOfImageOfAlpha (Curve 0 0 (-2*a) (a^2 - 4*b) 0)
