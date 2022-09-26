import Control.Applicative
import Data.List
import Data.Ratio
import Debug.Trace

data ProjectivePoint = Planar Rational Rational
                     | Infinity deriving (Show, Eq)

-- Weierstrass form: y^2 + a1 xy + a3 y = x^3 + a2 x^2 + a4 x + a6
-- They are given here in the order a1 a3 a2 a4 a6
data Curve = Curve { a1 :: Rational, a3 :: Rational, a2 :: Rational, a4 :: Rational, a6 :: Rational } deriving(Eq)
instance Show Curve where
    show (Curve a1 a3 a2 a4 a6) = "E: y^2 + (" ++ (show a1) ++ ")xy + (" ++ (show a3) ++ ")y = x^3 + (" ++ (show a2) ++ ")x^2 + (" ++ (show a4) ++ ")x + ("++ (show a6) ++ ")"


ellipticAddition :: Curve -> ProjectivePoint -> ProjectivePoint -> ProjectivePoint
ellipticAddition (Curve a1 a3 a2 a4 a6) (Planar x1 y1) (Planar x2 y2)
    | (x1 == x2) && (y1 + y2 + a1*x2 + a3 == 0) = Infinity
    | otherwise = (Planar x3 y3) where
        lambda | x1 /= x2 = (y2 - y1)/(x2 - x1)
               | otherwise = (3*x1^2 + 2*a2*x1 + a4 - a1*y1)/(2*y1 + a1*x1 + a3)
        nu | x1 /= x2 = (y1*x2 - y2*x1)/(x2 - x1)
           | otherwise = (-x1^3 + a4*x1 + 2*a6 - a3*y1)/(2*y1 + a1*x1 + a3)
        x3 = lambda^2 + a1*lambda - a2 - x1 - x2
        y3 = -(lambda + a1)*x3 - nu - a3
ellipticAddition _ Infinity point = point
ellipticAddition _ point Infinity = point

ellipticInverse :: Curve -> ProjectivePoint -> ProjectivePoint
ellipticInverse _ Infinity = Infinity
ellipticInverse (Curve a1 a3 a2 a4 a6) (Planar x y) = (Planar x (-y - a1*x - a3))

ellipticNMult :: Curve -> Int -> ProjectivePoint -> ProjectivePoint
ellipticNMult curve n p = foldl (ellipticAddition curve) Infinity $ (take n . repeat) p

-- Assumes char K /= 2 - cf Silverman p. 42
weierstrassDiscriminant :: Curve -> Rational
weierstrassDiscriminant (Curve a1 a3 a2 a4 a6) = d where
    b2 = a1^2 + 4*a2 -- typo in Silverman?? It gives a1^2 + 4*a4
    b4 = 2*a4 + a1*a3
    b6 = a3^2 + 4*a6
    b8 = (a1^2)*a6 + 4*a2*a6 - a1*a3*a4 + a2*(a3^2) - a4^2
    d = -(b2^2)*b8 - 8*(b4^3) - 27*(b6^2) + 9*b2*b4*b6

eval :: Curve -> Rational -> Rational -> Rational
eval (Curve a1 a3 a2 a4 a6) x y = y^2 + a1*x*y + a3*y - x^3 - a2*x^2 - a4*x - a6

ellipticEval :: Curve -> ProjectivePoint -> Rational
ellipticEval _ Infinity = 0
ellipticEval curve (Planar x y) = eval curve x y

solutionsModP :: Curve -> Int -> [(Int, Int)]
solutionsModP curve p = 
    [(x, y) | x <- [0..(p-1)],
              y <- [0..(p-1)],
              mod (fromIntegral $ numerator $ eval curve (toRational x) (toRational y)) p == 0,
              mod (fromIntegral $ denominator $ eval curve (toRational x) (toRational y)) p /= 0]

-- Compute the rational torsion on a curve
-- By Nagel-Lutz, after a change of coordinates (valid since char Q = 0), we need only consider y=0 or y^2|D
-- This doesn't quite work because we no longer have integer coefficients after a shift :(
rationalTorsion :: Curve -> [ProjectivePoint]
-- Need to prove these points have torsion!
rationalTorsion (Curve 0 0 a2 a4 a6) = rationalPotentialTorsion where
    -- We are also assuming disc is int here!
    disc = abs $ numerator $ weierstrassDiscriminant (Curve 0 0 a2 a4 a6)
    factorsOfDisc = filter ((== 0) . (mod disc) . (^2)) [1..disc]
    possibleYValues = [0] ++ factorsOfDisc ++ map negate factorsOfDisc
    
    xValuesFromYViaRationalRootTheorem y 
        | a6 == y^2 = [0]
        | otherwise = factors ++ map negate factors where
            n = (abs . numerator) (a6 - y^2)
            factors = filter ((== 0) . (mod n)) [1..n]

    rationalPotentialTorsion = filter ((== 0) . (ellipticEval (Curve 0 0 a2 a4 a6))) $
        concat $
        [ (map (\x -> Planar (toRational x) (toRational y)) $ xValuesFromYViaRationalRootTheorem (toRational y)) | y <- possibleYValues ]
rationalTorsion (Curve a1 a3 a2 a4 a6) = (shiftMap . rationalTorsion . twist) curve where
    twist (Curve a1 a3 a2 a4 a6) = Curve 0 0 (a2 + (a1^2)/4) (a4 + a1*a3/2) (a6 + (a3^2)/4)
    shiftY :: ProjectivePoint -> ProjectivePoint
    shiftY Infinity = Infinity
    shiftY (Planar x yprime) = Planar x (yprime - (a1*x + a3)/2)
    shiftMap = map shiftY



-- We then need to confirm that these points are torsion. By Mazur, it is sufficient to check up to 12P
-- (we need not check 11 but we may as well compute it anyway)
isRationalTorsion :: Curve -> ProjectivePoint -> Bool
isRationalTorsion curve p = elem Infinity pThrough12P where
    pThrough12P = scanl (ellipticAddition curve) p $ (take 11 . repeat) p

-- Only defined for integer coefficients and a1 == a3 == 0
lutzNagelRationalTorsion :: Curve -> [ProjectivePoint]
lutzNagelRationalTorsion (Curve 0 0 a2 a4 a6) 
    | all ((== 1) . denominator) [a2, a4, a6] = [Infinity] ++ rationalPlanarTorsion where
        disc = abs $ numerator $ weierstrassDiscriminant (Curve 0 0 a2 a4 a6)
        factorsOfDisc = filter ((== 0) . (mod disc) . (^2)) [1..disc]
        possibleYValues = [0] ++ factorsOfDisc ++ map negate factorsOfDisc
       
        xValuesFromY y = map toRational $ rationalCubicRoots a2 a4 (a6 - y^2)
        rationalPlanarTorsion = 
            filter (isRationalTorsion (Curve 0 0 a2 a4 a6)) $
            filter ((== 0) . (ellipticEval (Curve 0 0 a2 a4 a6))) $
            concat $ 
            map (map <$> (flip Planar) <*> (xValuesFromY)) $
            map toRational possibleYValues
lutzNagelRationalTorsion _ = undefined

-- Find the _rational_ roots of the monic cubic x^3 + ax^2 + bx + c
rationalQuadraticRoots :: Rational -> Rational -> [Rational]
rationalQuadraticRoots a 0 = [0, -a]
rationalQuadraticRoots a b = maybePair where
    n = a^2 - 4*b
    forcedSqNum = (floor . sqrt . fromIntegral . abs . numerator) n
    forcedSqDen = (floor . sqrt . fromIntegral . denominator) n
    forcedSq = forcedSqNum % forcedSqDen
    maybePair = 
        if forcedSq^2 == n then map (+(-a/2)) $ map (*(forcedSq/2)) [1, -1]
        else []
-- The first root can be found via RRT
rationalCubicRoots :: Rational -> Rational -> Rational -> [Rational]
rationalCubicRoots a b c = roots rationalRoots where
    x3Term = foldl lcm 1 $ map denominator [a, b, c]
    possibleDenominators = filter ((== 0) . (mod x3Term)) [1..x3Term]
    absConstTerm = abs (x3Term * numerator c)
    factorsOfConstTerm = filter ((== 0) . (mod absConstTerm)) [1..absConstTerm]
    possibleNumerators = [0] ++ factorsOfConstTerm ++ map negate factorsOfConstTerm

    lagrangianLimit = foldl max 1 $ map abs [a, b, c]
    rationalRoots = filter ((== 0) . (\x -> x^3 + a*x^2 + b*x + c)) $ 
        filter ((<= lagrangianLimit) . abs) $
        (%) <$> possibleNumerators <*> possibleDenominators
    roots (firstRoot:tail) = [firstRoot] ++ rationalQuadraticRoots (a + firstRoot) (b + (a + firstRoot)*firstRoot)
    roots [] = []

-- For any curve over Q with a1 = 0, there is a unique value of y for planar 2-torsion points
-- This implies there are at most 4 points of order dividing 2, including infinity
rational2Torsion :: Curve -> [ProjectivePoint]
rational2Torsion (Curve 0 a3 a2 a4 a6) = [Infinity] ++ planarPoints where
    y0 = -a3/2
    -- Search for x solving the elliptic at this point
    -- If we knew that we had integer coords, this would be significantly easier
    planarPoints = map (\x -> Planar x y0) $ nub $ rationalCubicRoots a2 a4 (a6 - a3 - y0^2)

        



---
curve = Curve 1 0 0 4 1
p = Planar (-1%4) (1%8)
q = p
main = print $ show $ ellipticAddition curve p q

