module Data.EllipticCurves.Algorithms.PrimeFieldCurves (
      curvePointsOnFp
    , countCurvePointsOverExtension
    , singularities
    ) where

import Data.Proxy
import Data.Ratio
import GHC.TypeLits
import Numeric.AD
import Numeric.AD.Internal.Tower

import Data.EllipticCurves
import Data.EllipticCurves.Algorithms.General
import Data.EllipticCurves.PreliminaryNumberTheory

-- Which points exist on E(F_p)?
curvePointsOnFp :: forall p . KnownNat p => Curve (PrimeFieldElem p) -> [ProjectivePoint (PrimeFieldElem p)]
curvePointsOnFp curve = Infinity:filter (curveContainsPoint curve) (Planar <$> lst <*> lst) where
    p = natVal (Proxy :: Proxy p)
    lst = map fromInteger [0..(p-1)]

-- Find the order of E(F_p^r), for arbitrary r, using the zeta function
countCurvePointsOverExtension :: forall p . KnownNat p => Curve (PrimeFieldElem p) -> [Integer]
countCurvePointsOverExtension curve = curvePointCounts where
    q = toRational $ natVal (Proxy :: Proxy p) :: Rational
    q' = auto q :: AD s (Tower Rational)
    n = toRational $ length $ curvePointsOnFp curve :: Rational
    a = q + 1 - n :: Rational
    a' = auto a :: AD s (Tower Rational)

    -- Note this is exp(f(t)), for f(t) = n1 t + n2/2 t^2 + n3/3 t^3 + ...
    zeta :: AD s (Tower Rational) -> AD s (Tower Rational)
    zeta t = (1 - a'*t + q'*t^i2) / ((1 - t) *(1 - q'*t))

    -- The derivatives of the zeta function will be:
    -- d^k/dt^k (exp(f(t))) = exp(f(t)) ( \sum_{i1+...+ir=k, ij>0} d^i1/dt^i1 f(t) * ... * d^ir/dt^ir f(t) )
    -- But we note that d^r/dt^r f(t) | t = 0 is #E(F_p^r)
    derivatives = diffs zeta 0 :: [Rational]
    idxs :: Int -> [[Int]]
    idxs = (map idxs' [0..] !!) where
        idxs' 0 = [[]]
        idxs' m = [ i:xs | i <- [1..m], xs <- idxs (m-i) ] -- TODO can throw away ordering 

    fDerivs :: Int -> Rational
    fDerivs = (map fDerivs' [0..] !!) where
        fDerivs' 0 = 0
        fDerivs' 1 = derivatives!!1
        fDerivs' i = (derivatives!!i) - sum (init $ map (product . map fDerivs) $ idxs i)

    -- We need to account for the extra factors gained by differentiating multiple times
    factorials :: [Rational]
    factorials = scanl1 (*) [1..]
    curvePointCounts = map numerator $ zipWith (/) (map fDerivs [1..]) (1:factorials)


-- Identify singular points on the curve
singularities :: forall p . KnownNat p => Curve (PrimeFieldElem p) -> [ProjectivePoint (PrimeFieldElem p)]
singularities = filter <$> isSingular <*> curvePointsOnFp
