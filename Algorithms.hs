import GHC.TypeLits
import Data.Proxy
import Data.List
import Data.Ratio
import Debug.Trace
import Control.Applicative
import Control.Exception
import Control.Monad (join)
import Control.Arrow ((***))

import PreliminaryNumberTheory
import EllipticCurves

-- Given a curve and a finite _group_ of points on the curve, construct
-- a curve isomorphic to the image under quotient by this finite group
-- Algorithm of Velu - c.f. Velu, Isogenies between Elliptic Curves
quotientCurve :: (Eq k, Fractional k) => Curve k -> [ProjectivePoint k] -> Curve k
quotientCurve (Curve a1 a3 a2 a4 a6) f = quot where
    f2 = filter (/= Infinity) $ filter ((== Infinity) . ellipticNMult (Curve a1 a3 a2 a4 a6) 2) f
    r [] r' = r'
    r (x:xs) r' | (x `elem` r') || (ellipticInverse (Curve a1 a3 a2 a4 a6) x `elem` r') = r xs r'
                | otherwise = r xs (x:r') 
    s = f2 ++ r (f \\ (Infinity:f2)) []

    gxq (Planar xq yq) = 3*xq^2 + 2*a2*xq + a4 - a1*yq
    gyq (Planar xq yq) = -2*yq - a1*xq - a3
    tq (Planar xq yq) | Planar xq yq `elem` f2 = gxq (Planar xq yq)
                      | otherwise = 2 * gxq (Planar xq yq) - a1 * gyq (Planar xq yq)
    uq (Planar xq yq) = gyq (Planar xq yq) ^2

    t = sum $ map tq s
    w = sum $ map (\(Planar xq yq) -> uq (Planar xq yq) + xq * tq (Planar xq yq)) s

    quot = Curve a1 a3 a2 (a4 - 5*t) (a6 - (a1^2 + 4*a2)*t - 7*w)

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

-- Which points exist on E(F_p)?
curvePointsOnFp :: forall p . KnownNat p => Curve (PrimeFieldElem p) -> [ProjectivePoint (PrimeFieldElem p)]
curvePointsOnFp curve = Infinity:filter (curveContainsPoint curve) (Planar <$> lst <*> lst) where
    p = natVal (Proxy :: Proxy p)
    lst = map fromInteger [0..(p-1)]

-- Returns the formal derivative function in x, y, z coordinates
formalDerivatives :: (Num k) => Curve k -> ProjectivePoint k -> (k, k, k)
formalDerivatives (Curve a1 a3 a2 a4 a6) = ((,,) <$> dxF <*> dyF <*> dzF) . homogenizeCoords where
    dxF (x, y, z) = a1*y*z - 3*x^2 - 2*a2*x*z - a4*z^2
    dyF (x, y, z) = 2*y*z + a1*x*z + 2*a3*z^2
    dzF (x, y, z) = y^2 + a1*x*y + 2*a3*y*z - a2*x^2 - 2*a4*x*z - 3*a6*z^2

-- Test if a point is singular. Note that by definition an elliptic curve is non-singular, but 
-- it is of interest to know when and where its reduction modulo some prime is singular
isSingular :: (Eq k, Num k) => Curve k -> ProjectivePoint k -> Bool
isSingular curve = (== (0,0,0)) . formalDerivatives curve 

-- Identify singular points on the curve
singularities :: forall p . KnownNat p => Curve (PrimeFieldElem p) -> [ProjectivePoint (PrimeFieldElem p)]
singularities = filter <$> isSingular <*> curvePointsOnFp

-- Assume working over a field of characteristic /= 2, 3
-- Assume we have a cubic of the form s1u^3 + s2u^2v + s3uv^2 + s4v^3 + s5u^2 + s6uv + s7v^2 + s8u + s9v = 0
--
-- c.f. Elliptic Curve Handbook, Ian Connell
data GeneralCubic k = GeneralCubic { s1 :: k, s2 :: k, s3 :: k, s4 :: k, s5 :: k, s6 :: k, s7 :: k, s8 :: k, s9 :: k } deriving (Show, Eq)

nagellsAlgorithm :: (Enum k, Show k, Eq k, Fractional k) => GeneralCubic k -> Curve k
nagellsAlgorithm (GeneralCubic s1 s2 s3 s4 s5 s6 s7 s8 0) = assert (s8 /= 0) $ nagellsAlgorithm $ GeneralCubic s4 s3 s2 s1 s7 s6 s5 0 s8
nagellsAlgorithm (GeneralCubic s1 s2 s3 s4 s5 s6 s7 s8 s9) = curve where
    f3 u v = s1*(u^3) + s2*(u^2)*v + s3*u*(v^2) + s4*(v^3)
    f2 u v = s5*(u^2) + s6*u*v + s7*(v^2)
    f1 u v = s8*u + s9*v

    e3 = f3 s9 (-s8)
    e2 = f2 s9 (-s8)
    
    -- Now change of variable to get gross stuff
    -- The algebra is unpleasant here - what we can do, however, is interpolation to get the coefficients
    -- once we have the function itself
    r | e3 /= 0 = e2/e3
      | otherwise = 1 
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

    interpolatedCubicPolynomial f = [c, d, e, k] where
        y0 = f 0; y1 = f 1; y2 = f 2; y3 = f 3
        c = (-y0/6) + (y1/2) + (-y2/2) + (y3/6)
        d = y0 + (-5*y1/2) + (2*y2) + (-y3/2)
        e = (-11*y0/6) + (3*y1) + (-3*y2/2) + (y3/3)
        k = y0

    -- Assumes that f is a polynomial of degree <= 4
    leadingCoeff f = sum $ map ((/) <$> f <*> (product . filter (/= 0) . (`map` [0..4]) . subtract)) [0..4]
        
    -- ro 0 is the leading coefficient of delta - we give it directly to avoid division by zero issues
    ro 0 = leadingCoeff delta
    ro tau = (tau^4) * delta (t0 + 1/tau)

    -- We now compute the coefficients of ro, which is a cubic polynomial in tau
    -- Suppose ro = c tau^3 + d tau^2 + e tau + k
    -- Lagrange interpolation gives us these values - note that we need 4 values to recreate 
    -- a cubic exactly, and since char K /= 2,3, we have that {0, 1, 2, 3} are distinct elements
    [c, d, e, k] = interpolatedCubicPolynomial ro

    -- If c == 0, the curve was not elliptic
    curve = assert (c /= 0) $ 
            assert ((e2 /= 0) || (e3 /= 0)) $
            Curve 0 0 d (c*e) ((c^2)*k) 


-- Given a curve E : y^2 = x^3 + ax^2 + bx, consider the homomorphism
-- alpha_E : E(Q) -> Q* / (Q*)^2
-- (x,y) |-> b if (x,y) = (0,0),
--           x otherwise
-- This isogeny yields data about the rank of the elliptic curve E
rankOfImageOfAlpha :: Curve Rational -> Integer
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
    imagePointFromOrigin = head $ filter (isSquare . (/ b) . toRational) elementsToCheck

    imageElements [] certainIn _ = certainIn 
    imageElements (x:xs) certainIn certainOut 
        | x `elem` certainIn = imageElements xs certainIn certainOut
        | x `elem` certainOut = imageElements xs certainIn certainOut
        | otherwise = imageElements xs (certainIn' xIsValid) (certainOut' xIsValid) where
            xIsValid = existsIntegerSolution x (numerator a) (round (b/ toRational x))
            certainIn' (Just valid) = if valid then nub (x:(certainIn ++ map (*x) certainIn)) else certainIn
            certainOut' (Just valid) = if valid then nub (certainOut ++ map (*x) certainOut) else nub (x:certainOut)

    r = (round . logBase 2 . fromIntegral . length) $ imageElements elementsToCheck [imagePointFromOrigin] []

-- Given a curve E : y^2 = x^3 + ax^2 + bx, we define a dual curve E' : y^2 = x^3 - 2ax^2 + (a^2 - 4b)x
-- Considering the homs alpha_E, alpha_E', we may deduce the rank of the curve
rationalRank :: Curve Rational -> Integer
rationalRank (Curve 0 0 a b 0) | all ((== 1) . denominator) [a, b] = r1 + r2 - 2 where
    r1 = rankOfImageOfAlpha (Curve 0 0 a b 0)
    r2 = rankOfImageOfAlpha (Curve 0 0 (-2*a) (a^2 - 4*b) 0)

