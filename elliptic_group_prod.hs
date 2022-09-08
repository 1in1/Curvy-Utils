import Data.Ratio
import Data.List

-- We can make this a coproduct with the basepoint too... 
data Point = Point Rational Rational deriving(Show, Eq)

-- In Weierstrass form, we can describe the equation as y^2 = x^3 + ax + b (free in a and b)
-- The basepoint is necessarily the point at infinity
data Curve = Curve Rational Rational deriving(Show, Eq)

-- We describe a line by a point on it, and its gradient
data Line = Line Point Rational deriving(Show, Eq)
evaluate :: Line -> Rational -> Rational
evaluate (Line (Point x0 y0) m) x = m*(x - x0) + y0

data MonicQuadratic = MonicQuadractic Rational Rational deriving(Show, Eq)
ratRoots :: MonicQuadratic -> [Rational]
ratRoots (MonicQuadractic b c) = [(-b + forcedSqrt)/2, (-b - forcedSqrt)/2] where
    quadDisc = b^2 - 4*c
    forcedSqrt :: Rational
    forcedSqrt = (round $ sqrt $ fromIntegral $ numerator quadDisc)
        % (round $ sqrt $ fromIntegral $ denominator quadDisc)
    

data MonicCubic = MonicCubic Rational Rational Rational deriving(Show, Eq)
-- We have cubic = (x - l)(x^2 + (b+l)x - d/l)
-- So we just need to find the roots of the quadratic bit
otherRationalRoots :: Rational -> MonicCubic -> [Rational]
otherRationalRoots 0 (MonicCubic b c d) = ratRoots $ MonicQuadractic b c
otherRationalRoots l (MonicCubic b c d) = ratRoots $ MonicQuadractic (b + l) (-d / l)


intersectionsGivenPoint :: Curve -> Line -> Point -> [Point]
intersectionsGivenPoint (Curve a b) (Line (Point x y) m) (Point u _) =
    map (\z -> Point z (evaluate (Line (Point x y) m) z))
    $ otherRationalRoots u
    $ MonicCubic (-(m^2)) (a - 2*m*y + 2*x*(m^2)) (b - (y^2) + 2*m*x*y - (m^2)*(x^2))


-- Analytic result
tangent :: Curve -> Point -> Line
tangent (Curve a b) (Point x y) = Line (Point x y) m where
    m = (3*(x^2) + a) / (2*y)

spanningLine :: Point -> Point -> Line
spanningLine (Point x1 y1) (Point x2 y2) = Line (Point x1 y2) m where
    m = (y2 - y1) / (x2 - x1)


getThirdPoint :: Curve -> Point -> Point -> Point
getThirdPoint curve p q = point where
    -- Determine the line to use
    line | p == q    = tangent curve p
         | otherwise = spanningLine p q
    -- Then find the intersections of this line and the curve
    meetingPoints = intersectionsGivenPoint curve line p
    -- This process should leave precisely one
    point = head $ filter (\pt -> (pt /= p) && (pt /= q)) meetingPoints
    
chord_and_tangent :: Curve -> Point -> Point -> Point
chord_and_tangent curve p q = point where
    (Point x y) = getThirdPoint curve p q
    point = Point x (-y)


--- Let's do this quickly and not like a muppet
instaCompute :: Curve -> Point -> Point -> Point
instaCompute (Curve a b) (Point x1 y1) (Point x2 y2) = Point x3 y3 where
    m = (y2 - y1) / (x2 - x1)
    v = y1 - m*x1
    x3 = m^2 - x1 - x2
    y3 = m*x3 + v


---


--main = print (show (roots (Regular (205 % 24) (- 49 % 12) (3 % 8))))
--main = print (show (roots (Depressed (-4) (0))))
-- main = print $ show $ rationalRoots (RegularCubic 1 (205%24) (-49%12) (3%8))
-- main = print $ show $ rationalRoots (RegularCubic 1 0 (-4) 0)
-- main = print $ show $ intersections (Curve(-25) 0) (Line (Point (25 % 4) (75 % 8)) (59%12))
bp = Point 0 0
curve = Curve (-25) 0
p = Point (25%4) (75%8)
main = print $ show $ instaCompute curve p bp

