import Data.Ratio
import Data.List

-- We can make this a coproduct with the basepoint too... 
data ProjectivePoint = Planar Rational Rational 
                     | Infinity deriving(Show, Eq)

-- In Weierstrass form, we can describe the equation as y^2 = x^3 + ax + b (free in a and b)
-- The basepoint is necessarily the point at infinity
data Curve = Curve Rational Rational deriving(Show, Eq)

--- Let's do this quickly and not like a muppet

pointFromGrad :: ProjectivePoint -> ProjectivePoint -> Rational -> ProjectivePoint
pointFromGrad (Planar x1 y1) (Planar x2 y2) m = Planar x3 y3 where
    v = y1 - m*x1
    x3 = m^2 - x1 - x2
    y3 = -(m*x3 + v)

groupInv :: ProjectivePoint -> ProjectivePoint
groupInv (Planar x y) = Planar x (-y)
groupInv Infinity = Infinity

groupProduct :: Curve -> ProjectivePoint -> ProjectivePoint -> ProjectivePoint
groupProduct (Curve a b) (Planar x1 y1) (Planar x2 y2)
    | (Planar x1 y1) == (Planar x2 y2) = pointFromGrad (Planar x1 y1) (Planar x2 y2) ((3*(x1^2) - a) / (2*y1))
    -- In this case, the line through the two must hit the basepoint at infinity as the unique third point
    -- Necessarily if y1 == y2 and x1 /= x2, p = -q
    | x1 == x2 = Infinity
    | otherwise = pointFromGrad (Planar x1 y1) (Planar x2 y2) ((y2 - y1) / (x2 - x1))
groupProduct _ Infinity point = point
groupProduct _ point Infinity = point


-- Next thing we can try doing is working over general elliptic curves, rather than forcing Weierstrass form...

-- These can be described as y^2 + axy + by = x^3 + cx^2 + dx + e
-- Internally, we will store this as a curve in Weiestrass form, along with the data to shift points
data EllipticCurve = EllipticCurve Curve Rational Rational Rational deriving(Show, Eq)

ellipticCurveFromGeneralForm :: Rational -> Rational -> Rational -> Rational -> Rational -> EllipticCurve
ellipticCurveFromGeneralForm a b c d e = (EllipticCurve (Curve p q) xshiftInConst yshiftInX yshiftInConst) where
    -- Under the substitution y = y' - (ax + b)/2, the equation becomes
    -- y'^2 = x^3 + (c + a^2 /2)x^2 + (d + ab)x + (e + b^2)
    -- Write this as y'^2 = x^3 + c'x^2 + d'x + e'
    -- Then substituting x = x' - c'/3, the equation becomes
    -- y'^2 = x'^3 + (d' - c'^2 / 3)x' + (e' - c'd'/3 + 2c'^3 /27)
    cprime = c + (a^2)/2
    dprime = d + a*b
    eprime = e + b^2
    p = dprime - (cprime^2)/3
    q = eprime - cprime*dprime/3 + 2*(cprime^3)/27
    xshiftInConst = cprime/3
    yshiftInX = a/2
    yshiftInConst = b/2

generalPointToWeiestrassPoint :: EllipticCurve -> ProjectivePoint -> ProjectivePoint
generalPointToWeiestrassPoint (EllipticCurve _ xshiftInConst yshiftInX yshiftInConst) (Planar x y) = (Planar xprime yprime) where
    xprime = x + xshiftInConst
    yprime = y + yshiftInX*x + yshiftInConst

weiestrassPointToGeneralPoint :: EllipticCurve -> ProjectivePoint -> ProjectivePoint
weiestrassPointToGeneralPoint (EllipticCurve _ xshiftInConst yshiftInX yshiftInConst) (Planar xprime yprime) = (Planar x y) where
    x = xprime - xshiftInConst
    y = yprime yshiftInX*x + yshiftInConst

groupProduct :: EllipticCurve -> ProjectivePoint -> ProjectivePoint -> ProjectivePoint
groupProduct (EllipticCurve curve xshiftInConst yshiftInX yshiftInConst) p q = pq where
    pprime = generalPointToWeiestrassPoint p
    qprime = generalPointToWeiestrassPoint q
    pprimeqprime = groupProduct curve pprime qprime
    pq = weiestrassPointToGeneralPoint pprimeqprime
    
-- We can get one without y by transforming y = y' - (ax + b)/2
-- Then get one depressed in x by transforming x = x' - c/3



---


bp = Planar 0 0
curve = Curve (-25) 0
p = Planar (25%4) (75%8)
main = print $ show $ groupProduct curve p p

