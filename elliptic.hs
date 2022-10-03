import Control.Applicative
import Control.Exception
import Data.List
import Data.Ratio
import Debug.Trace

import PreliminaryNumberTheory

-- Weierstrass form: y^2 + a1 xy + a3 y = x^3 + a2 x^2 + a4 x + a6
-- They are given here in the order a1 a3 a2 a4 a6
data Curve k = Curve { a1 :: k, a3 :: k, a2 :: k, a4 :: k, a6 :: k } deriving (Show, Eq)
-- In Weierstrass form, the only point not in the affine plane V(Z-1) is (0:1:0). We denote this Infinity here
data ProjectivePoint k = Planar k k
                       | Infinity deriving (Show, Eq)

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

-- Define the group law
-- Addition on the curve
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

-- Inverses on the curve
ellipticInverse :: (Fractional k) => Curve k -> ProjectivePoint k -> ProjectivePoint k
ellipticInverse _ Infinity = Infinity
ellipticInverse (Curve a1 a3 a2 a4 a6) (Planar x y) = Planar x (-y - a1*x - a3)

-- Utility function; apply the addition function n times
ellipticNMult :: (Eq k, Fractional k) => Curve k -> Int -> ProjectivePoint k -> ProjectivePoint k
ellipticNMult curve n p = foldl (ellipticAddition curve) Infinity $ replicate n p

-- Requires char K /= 2 - cf Silverman p. 42
weierstrassDiscriminant :: (Eq k, Fractional k) => Curve k -> k
weierstrassDiscriminant (Curve a1 a3 a2 a4 a6) = d where -- | ((fromRational (2%1))::k) /= ((fromRational (0%1))::k) = d where
    b2 = a1^2 + 4*a2 -- typo in Silverman?? It gives a1^2 + 4*a4
    b4 = 2*a4 + a1*a3
    b6 = a3^2 + 4*a6
    b8 = (a1^2)*a6 + 4*a2*a6 - a1*a3*a4 + a2*(a3^2) - a4^2
    d = -(b2^2)*b8 - 8*(b4^3) - 27*(b6^2) + 9*b2*b4*b6

-- -- Given p, reduce the (RATIONAL) curve to get a minimal disc
-- padicMinimalDiscriminant :: Curve Rational -> Integer -> Rational
-- padicMinimalDiscriminant curve p = d where
--     disc = weierstrassDiscriminant curve
--     d = until ((/= 0) . (`mod` p) . denominator) (*(p^12)) $ 
--         until ((/= 0) . (`mod` p) . numerator) (/(p^12)) disc

jInvariant :: (Eq k, Fractional k) => Curve k -> k
jInvariant (Curve a1 a3 a2 a4 a6) = j where
    b2 = a1^2 + 4*a2
    b4 = 2*a4 + a1*a3
    c4 = b2^2 - 24*b4
    d = weierstrassDiscriminant (Curve a1 a3 a2 a4 a6)
    j = (c4^3)/d


-- Given a curve and a finite _group_ of points on the curve, construct
-- a curve isomorphic to the image under quotient by this finite group
velu :: (Eq k, Fractional k) => Curve k -> [ProjectivePoint k] -> Curve k
velu (Curve a1 a3 a2 a4 a6) f = quot where
    f2 = filter ((== Infinity) . ellipticNMult (Curve a1 a3 a2 a4 a6) 2) f
    r [] r' = r'
    r (x:xs) r' | (x `elem` r') || (ellipticInverse (Curve a1 a3 a2 a4 a6) x `elem` r') = r xs r'
                | otherwise = r xs (x:r') 
    s = f2 ++ r ((f \\ [Infinity]) \\ f2) []

    gxq (Planar xq yq) = 3*xq^2 + 2*a2*xq + a4 - a1*yq
    gyq (Planar xq yq) = -2*yq - a1*xq - a3
    tq (Planar xq yq) | Planar xq yq `elem` f2 = gxq (Planar xq yq)
                      | otherwise = 2 * gxq (Planar xq yq) - a1 * gyq (Planar xq yq)
    uq (Planar xq yq) = gyq (Planar xq yq) ^2

    t = sum $ map tq s
    w = sum $ map (\(Planar xq yq) -> uq (Planar xq yq) + xq * tq (Planar xq yq)) s

    quot = Curve a1 a3 a2 (a4 - 5*t) (a6 - (a1^2 + 4*a2)*t - 7*w)



solutionsModP :: Curve Rational -> Int -> [(Int, Int)]
solutionsModP curve p = 
    [(x, y) | x <- [0..(p-1)],
              y <- [0..(p-1)],
              mod (fromIntegral $ numerator $ eval curve (toRational x) (toRational y)) p == 0,
              mod (fromIntegral $ denominator $ eval curve (toRational x) (toRational y)) p /= 0]





-- We then need to confirm that these points are torsion. By Mazur, it is sufficient to check up to 12P
-- (we need not check 11 but we may as well compute it anyway)
isRationalTorsion :: Curve Rational -> ProjectivePoint Rational -> Bool
isRationalTorsion curve p = Infinity `elem` pThrough12P where
    pThrough12P = scanl (ellipticAddition curve) p $ replicate 11 p

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
            filter ((== 0) . ellipticEval (Curve 0 0 a2 a4 a6)) $
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


--- This is gonna be fucky
-- Assume working over a field of characteristic /= 2, 3
-- Assume we have a cubic of the form s1u^3 + s2u^2v + s3uv^2 + s4v^3 + s5u^2 + s6uv + s7v^2 + s8u + s9v = 0
data GeneralCubic k = GeneralCubic { s1 :: k, s2 :: k, s3 :: k, s4 :: k, s5 :: k, s6 :: k, s7 :: k, s8 :: k, s9 :: k } deriving(Show, Eq)
nagellsAlgorithm :: (Show k, Eq k, Fractional k) => GeneralCubic k -> Curve k
nagellsAlgorithm (GeneralCubic s1 s2 s3 s4 s5 s6 s7 s8 0) = assert (s8 /= 0) $ nagellsAlgorithm $ GeneralCubic s4 s3 s2 s1 s7 s6 s5 0 s8
nagellsAlgorithm (GeneralCubic s1 s2 s3 s4 s5 s6 s7 s8 s9) = curve where
    f3 u v = s1*(u^3) + s2*(u^2)*v + s3*u*(v^2) + s4*(v^3)
    f2 u v = s5*(u^2) + s6*u*v + s7*(v^2)
    f1 u v = s8*u + s9*v

    e3 = f3 s9 (-s8)
    e2 = f2 s9 (-s8)
    -- Probably want to assert that e3 /= e2
    
    -- Now change of variable to get gross stuff
    -- The algebra is disgusting here. What we can do, however, is interpolation to get the coefficients
    -- once we have the function itself
    r | e3 /= 0 = e2/e3
      | otherwise = 1 -- Have to handle the fact that W' becomes U'!!!
    f3' u' v' 
        | e3 /= 0 = s1*u'^3 + s2*(u'^2)*v' + s3*u'*v'^2 + s4*v'^3
        | otherwise = 
            (s8*u' + s9*v')*u'^2 + 
            (s5*u'^2 + s6*u'*v' + s7*v'^2)*u' + 
            (s1*u'^3 + s2*(u'^2)*v' + s3*u'*v'^2 + s4*v'^3)
    f2' u' v' 
        | e3 /= 0 = 
            (s5*u'^2 + s6*u'*v' + s7*v'^2) + 
            r*(-3*s1*s9*u'^2 + s2*(-2*s9*u'*v' + s8*u'^2) + s3*(2*s8*u'*v' - s9*v'^2) + 3*s4*s8*v'^2)
        | otherwise =
            r*(-3*s1*s9*u'^2 + s2*(-2*s9*u'*v' + s8*u'^2) + s3*(2*s8*u'*v' - s9*v'^2) + 3*s4*s8*v'^2) +
            r*(-2*s5*s9*u' - s6*s9*v' + s6*s8*u' + 2*s7*s8*v')
    f1' u' v'
        | e3 /= 0 =
            (s8*u' + s9*v') + 
            r*(-2*s5*s9*u' - s6*s9*v' + s6*s8*u' + 2*s7*s8*v') +
            (r^2)*(3*s1*(s9^2)*u' + s2*((s9^2)*v' - 2*s9*s8*u') + s3*((s8^2)*u' - 2*s8*s9*v') + 3*s4*(s8^2)*v')
        | otherwise =
            (r^2)*(3*s1*(s9^2)*u' + s2*((s9^2)*v' - 2*s9*s8*u') + s3*((s8^2)*u' - 2*s8*s9*v') + 3*s4*(s8^2)*v') +
            (r^2)*(s5*(s9^2) - s6*s8*s9 + s7*(s8^2))*u'
    psi3 = f3' 1
    psi2 = f2' 1
    psi1 = f1' 1
    delta t = psi2 t ^2 - 4* psi1 t * psi3 t
    t0 = -s8/s9
    ro 0 = delta 1
    ro tau = (tau^4) * delta (t0 + 1/tau)

    -- We now compute the coefficients of ro, which is a cubic polynomial in tau
    -- Suppose ro = c tau^3 + d tau^2 + e tau + k
    -- Lagrange interpolation gives us these values - note that we need 4 values to recreate 
    -- a cubic exactly, and since char K /= 2,3, we have that {0, 1, 2, 3} are distinct elements
    y0 = ro 0
    y1 = ro 1
    y2 = ro 2
    y3 = ro 3

    -- Need special handling for 0, because haskell cant work out the 1/0 thing

    c = (-y0/6) + (y1/2) + (-y2/2) + (y3/6)
    d = y0 + (-5*y1/2) + (2*y2) + (-y3/2)
    e = (-11*y0/6) + (3*y1) + (-3*y2/2) + (y3/3)
    k = y0

    -- If c == 0, the curve was not elliptic
    curve = assert (c /= 0) $ Curve 0 0 d (c*e) ((c^2)*k) 


-- Given a curve E : y^2 = x^3 + ax^2 + bx, consider the isogeny 
-- alpha_E : E(Q) -> Q* / (Q*)^2
-- (x,y) |-> b if (x,y) = (0,0),
--           x otherwise
-- This isogeny yields data about the rank of the elliptic curve E
rankOfImageOfAlpha :: Curve Rational -> Integer
rankOfImageOfAlpha (Curve 0 0 a b 0) | all ((== 1) . denominator) [a, b] = r where
    -- Check that a, b are integer
    absB = (abs . numerator) b
    primeFactorOfB = primeFactors absB
    -- Also want to knock out b = 0
    basisForContainingSpace = (-1):primeFactorOfB

    
    -- Then work through the space...
    subsets [] = [[]]
    subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

    elementsToCheck = map product $ subsets basisForContainingSpace
    
    -- Begin by throwing in the point from alpha((0,0))
    imagePointFromOrigin = head $ filter (isSquare . (/ b) . toRational) elementsToCheck

    imageElements [] certainIn _ = certainIn 
    imageElements (x:xs) certainIn certainOut 
        | x `elem` certainIn = imageElements xs certainIn certainOut
        | x `elem` certainOut = imageElements xs certainIn certainOut
        | otherwise = imageElements xs (certainIn' xIsValid) (certainOut' xIsValid) where
            xIsValid = existsIntegerSolution x (numerator a) (round (b/ toRational x))
            certainIn' (Just valid) = if valid then nub (x:(certainIn ++ map (*x) certainIn)) else certainIn
            certainOut' (Just valid) = if valid then nub (certainOut ++ map (*x) certainOut) else nub (x:certainOut)

    r = round $ logBase 2 $ fromIntegral $ length $ imageElements elementsToCheck [imagePointFromOrigin] []

rationalRank :: Curve Rational -> Integer
rationalRank (Curve 0 0 a b 0) | all ((== 1) . denominator) [a, b] = r1 + r2 - 2 where
    r1 = rankOfImageOfAlpha (Curve 0 0 a b 0)
    r2 = rankOfImageOfAlpha (Curve 0 0 (-2*a) (a^2 - 4*b) 0)

