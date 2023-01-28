module Algorithms.General (
      isomorphismBetween
    , substitute
    , quotientCurve
    , formalDerivatives
    , isSingular
    , GeneralCubic (..)
    , nagellsAlgorithm
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Ratio
import Data.Tuple
import GHC.TypeLits

import PreliminaryNumberTheory
import EllipticCurves

-- Are two curves isomorphic to one another over k?
-- If so, deduce a  substitution of the form
-- x = u^2 x' + r,
-- y = u^3 y' + u^2 s x' + t,
-- with u, r, s, t \in k and u /= 0
-- and return the transformation used
--
-- For now, let's only worry about rational curves
isomorphismBetween :: Curve Rational -> 
                      Curve Rational -> 
                      Maybe (Rational, Rational, Rational, Rational)
isomorphismBetween (Curve a1 a3 a2 a4 a6) (Curve a1' a3' a2' a4' a6') = iso where
    b2 = a1^2 + 4*a2;  b2' = a1'^2 + 4*a2'
    b4 = 2*a4 + a1*a3; b4' = 2*a4' + a1'*a3'
    c4 = b2^2 - 24*b4; c4' = b2'^2 - 24*b4'
    ratio | (c4 == 0) || (c4' == 0) = Nothing
          | otherwise = Just (c4 / c4')

    -- There are potentially two options for u from this; the positive and negative square root
    -- We work out both, and check if they give the correct substitution

    -- We have u^4 c4' = c4. So if we can take square roots;
    uVals :: Maybe [Rational]
    uVals = fmap (\z -> [z, -z]) . squareRootRational =<< squareRootRational =<< ratio
    -- We have ua1' = a1 + 2s. So if we can divide by 2;
    sVals :: Maybe [Rational]
    sVals = map (\justU -> (justU*a1' - a1)/2) <$> uVals
    -- We have u^2 a2' = a2 - s a1 + 3r - s^2. So if we can divide by 3;
    rVals :: Maybe [Rational]
    rVals = liftM2 ((map (\(justU, justS) -> ((justU^2)*a2' - a2 + justS^2 + justS*a1)/3) .) . zip) uVals sVals
    -- We have u^3 a3' = a3 + r a1 + 2t. So if we can divide by 2;
    tVals :: Maybe [Rational]
    tVals = liftM2 ((map (\(justU, justR) -> ((justU^3)*a3' - a3 - justR*a1)/2) .) . zip) uVals rVals

    possibleMappings :: Maybe [(Rational, Rational, Rational, Rational)]
    possibleMappings = liftM4 zip4 uVals rVals sVals tVals

    iso = find (\(u, r, s, t) -> substitute (Curve a1 a3 a2 a4 a6) u r s t == Curve a1' a3' a2' a4' a6') 
        =<< possibleMappings

-- Substitute x = u^2 x + r, y = u^3 y' + u^2 s x' + t
substitute :: (Eq k, Fractional k) => Curve k -> k -> k -> k -> k -> Curve k
substitute (Curve a1 a3 a2 a4 a6) u r s t
    | u == 0 = undefined
    | otherwise = Curve a1' a3' a2' a4' a6' where
    a1' = (a1 + 2*s)/u
    a2' = (a2 - s*a1 + 3*r - s^2)/u^2
    a3' = (a3 + r*a1 + 2*t)/u^3
    a4' = (a4 - s*a3 + 2*r*a2 - (r*s + t)*a1 + 3*r^2 - 2*s*t)/u^4
    a6' = (a6 + r*a4 + r^2 * a2 + r^3 - t*a3 - t^2 - r*t*a1)/u^6

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

-- Assume working over a field of characteristic /= 2, 3
-- Assume we have a cubic of the form s1u^3 + s2u^2v + s3uv^2 + s4v^3 + s5u^2 + s6uv + s7v^2 + s8u + s9v + c = 0,
-- with a given rational point (u, v) = (p, q)
--
-- c.f. Elliptic Curve Handbook, Ian Connell
data GeneralCubic k = GeneralCubic { 
      s1 :: k
    , s2 :: k
    , s3 :: k
    , s4 :: k
    , s5 :: k
    , s6 :: k
    , s7 :: k
    , s8 :: k
    , s9 :: k
    , c :: k
    , rationalPoint :: (k, k)
    } deriving (Show, Eq)

nagellsAlgorithm :: forall k . (Enum k, Show k, Eq k, Fractional k) => GeneralCubic k -> Curve k
nagellsAlgorithm (GeneralCubic s1 s2 s3 s4 s5 s6 s7 s8 0 0 p) 
    | ((0 :: k) /= (2 :: k)) && ((0 :: k) /= (3 :: k)) = assert (s8 /= 0) $ 
        nagellsAlgorithm $ 
        GeneralCubic s4 s3 s2 s1 s7 s6 s5 0 s8 0 (swap p)
nagellsAlgorithm (GeneralCubic s1 s2 s3 s4 s5 s6 s7 s8 s9 0 (0,0)) 
    | ((0 :: k) /= (2 :: k)) && ((0 :: k) /= (3 :: k)) = curve where
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
-- If we're given a rational point not at (0,0), move it - this ensures the constant term is 0
nagellsAlgorithm (GeneralCubic s1 s2 s3 s4 s5 s6 s7 s8 s9 c (p,q)) 
    = nagellsAlgorithm $ GeneralCubic s1 s2 s3 s4 
        (3*s1*p + s2*q + s5) 
        (2*s2*p + 2*s3*q + s6) 
        (s3*p + 3*s4*q + s7) 
        (3*s1*p^2 + 2*s2*p*q + s2*q^2 + 2*s5*p + s6*q + s8) 
        (s2*p^2 + 2*s3*p*q + 3*s4*q^2 + s6*p + 2*s7*q + s9) 
        0 
        (0,0)
