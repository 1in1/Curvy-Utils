module Data.EllipticCurves.Algorithms.PrimeFieldCurves (
      curvePointsOnFp
    , singularities
    ) where

import Data.Proxy
import Data.Ratio
import GHC.TypeLits
import Numeric.AD
import Numeric.AD.Internal.Forward
import Numeric.AD.Internal.Tower

import Data.EllipticCurves
import Data.EllipticCurves.Algorithms.General
import Data.EllipticCurves.PreliminaryNumberTheory

-- Which points exist on E(F_p)?
curvePointsOnFp :: forall p . KnownNat p => Curve (PrimeFieldElem p) -> [ProjectivePoint (PrimeFieldElem p)]
curvePointsOnFp curve = Infinity:filter (curveContainsPoint curve) (Planar <$> lst <*> lst) where
    p = natVal (Proxy :: Proxy p)
    lst = map fromInteger [0..(p-1)]

-- Identify singular points on the curve
singularities :: forall p . KnownNat p => Curve (PrimeFieldElem p) -> [ProjectivePoint (PrimeFieldElem p)]
singularities = filter <$> isSingular <*> curvePointsOnFp
