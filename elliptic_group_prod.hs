import Data.Ratio
import Data.List

-- dydx :: Numeric -> Numeric -> Numeric
dydx x y = (3*x - 1) / (2*y + 1)

o = (-1, 0)
p = (0, 0)
curr = (1/4, -5/8)



---

tangent_at_point :: Rational -> Rational -> Rational
tangent_at_point x y = (3*x - 1) / (2*y + 1)



data MonicCubic = Regular Rational Rational Rational | Depressed Rational Rational

-- Forced square root
sq :: Rational -> Double
sq x = fromIntegral (round (fromRational x ** (1/2)))

-- This is a shit way to do it
-- If we know that they're all rational, we can just clear out denominators and use the rational root theorem
-- to pick out one root
-- From there quadratic solution will be ez

roots :: MonicCubic -> [Double]
roots (Depressed p q) = points where
    pr :: Double
    pr = fromRational p
    qr :: Double
    qr = fromRational q
    kth_root :: Rational -> Double
    kth_root k = 2 * (sqrt (-pr/3)) * cos(acos((3*qr/(2*pr)) * sqrt(-3/pr))/3 - fromRational k * 2 * pi / 3)
    points = [ kth_root x | x <- [0,1,2] ] 
roots (Regular b c d) = points where
    -- First depress the cubic, then shift the roots
    depressed = Depressed (c - (b^2)/3) (2*(b^3)/27 - c*b/3 + d)
    points = [ x - (fromRational (b/3)) | x <- roots depressed ]

data Cubic = RegularCubic Rational Rational Rational Rational 
           | IntCubic Integer Integer Integer Integer deriving(Show)

evaluate :: Cubic -> Rational -> Rational
evaluate (IntCubic a b c d) r = evaluate (RegularCubic (a%1) (b%1) (c%1) (d%1)) r
evaluate (RegularCubic a b c d) r = a*(r^3) + b*(r^2) + c*(r) + d

-- This bit is hugely inefficient ://
rationalRoots :: Cubic -> [Rational]
rationalRoots (IntCubic a b c d) = points where
    -- Get the divisors of a and d. We know that p | d, and q | a
    -- No accounting for multiple roots yet 
    aDivisors = [ n | n <- [1..round(sqrt(abs(a)))], abs(a) `mod` n == 0 ]
    dDivisors | d /= 0    = divs (abs d)
              | c /= 0    = divs (abs c) `union` [0]
              | b /= 0    = divs (abs b) `union` [0]
              | otherwise = [0]
              where divs l = [ n | n <- [1..l], l `mod` n == 0 ]
        
    posAndNegDDivisors = union dDivisors (map (\n -> -n) dDivisors)
    -- We now have a finite list of rationals to check
    points = take 3 [ p%q | p <- posAndNegDDivisors,
                            q <- aDivisors,
                            evaluate (IntCubic a b c d) (p%q) == 0 ]
rationalRoots (RegularCubic a b c d) = points where
    -- Clear out denominators
    m :: Rational
    m = toRational $ foldr lcm 1 $ map denominator [a, b, c, d]
    points = rationalRoots (IntCubic (round (a*m)) (round (b*m)) (round (c*m)) (round (d*m)))


---

-- Different way of doing rational roots

ratyRoots :: Cubic -> [Rational]
ratyRoots (IntCubic a b c d) = points where
    -- Get the divisors of a and d. We know that p | d, and q | a
    -- No accounting for multiple roots yet
    aDivisors = [ n | n <- [1..abs(a)], abs(a) `mod` n == 0 ]
    dDivisors | d /= 0    = divs (abs d)
              | c /= 0    = divs (abs c) `union` [0]
              | b /= 0    = divs (abs b) `union` [0]
              | otherwise = [0]
              where divs l = [ n | n <- [1..l], l `mod` n == 0 ]
        
    posAndNegDDivisors = union dDivisors (map (\n -> -n) dDivisors)
    -- We now have a finite list of rationals to check
    points = take 3 [ p%q | p <- posAndNegDDivisors,
                            q <- aDivisors,
                            evaluate (IntCubic a b c d) (p%q) == 0 ]
ratyRoots (RegularCubic a b c d) = points where
    -- Clear out denominators
    m :: Rational
    m = toRational $ foldr lcm 1 $ map denominator [a, b, c, d]
    points = ratyRoots (IntCubic (round (a*m)) (round (b*m)) (round (c*m)) (round (d*m)))






---




    
data Point = P Rational Rational deriving(Show, Eq)
-- In Weierstrass form, we can describe the equation as y^2 = x^3 + ax + b (free in a and b)
data Curve = C Rational Rational Point deriving(Show, Eq)
-- We describe a line by a point on it, and its gradient
data Line = L Point Rational deriving(Show, Eq)
evaluateY :: Line -> Rational -> Rational
evaluateY (L (P x0 y0) m) x = m*(x - x0) + y0
--intersections :: Curve -> Line -> [Rational]
--intersections curve line = int_set where
   
-- Need to find intersection points of a straight line with an elliptic curve
-- These are given by substituting and finding the zeros of a cubic
intersections :: Curve -> Line -> [Point]
intersections (C a b _) (L (P x y) m) = 
    map (\z -> P z (evaluateY (L (P x y) m) z))
    $ rationalRoots 
    $ RegularCubic 1 (-(m^2)) (a - 2*m*y + 2*x*(m^2)) (b - (y^2) + 2*m*x*y - (m^2)*(x^2))


-- To compute the group product, we need to draw some lines
-- If the points are the same, we need to take the tangent

-- Analytic result
tangent :: Curve -> Point -> Line
tangent (C a b _) (P x y) = L (P x y) m where
    m = (3*(x^2) + a) / (2*y)

spanningLine :: Point -> Point -> Line
spanningLine (P x1 y1) (P x2 y2) = L (P x1 y2) m where
    m = (y2 - y1) / (x2 - x1)


chord_and_tangent :: Curve -> Point -> Point -> Point
chord_and_tangent (C a b basepoint) p q = point where
    -- In these cases we already have a point on both the curve and the line.... FFS
    -- We don't need to solve the fucking cubic, we can just solve the quadratic

    -- Determine the line to use
    first_line | p == q    = tangent (C a b basepoint) p
               | otherwise = spanningLine p q

    -- Then find the intersections of this line and the curve
    meetingPoints = intersections (C a b basepoint) first_line 
    
    -- Given the points of intersection, we need to decide which of them is S
    -- Cannot be p or q, so this should leave precisely one
    s = head $ filter (\pt -> (pt /= p) && (pt /= q)) meetingPoints

    -- Now intersect with the line between the basepoint and S
    second_line | s == basepoint = tangent (C a b basepoint) s
                | otherwise      = spanningLine s basepoint

    moreMeetingPoints = intersections (C a b basepoint) second_line
    point = head $ filter (\pt -> (pt /= s) && (pt /= basepoint)) moreMeetingPoints



---


--main = print (show (roots (Regular (205 % 24) (- 49 % 12) (3 % 8))))
--main = print (show (roots (Depressed (-4) (0))))
-- main = print $ show $ rationalRoots (RegularCubic 1 (205%24) (-49%12) (3%8))
-- main = print $ show $ rationalRoots (RegularCubic 1 0 (-4) 0)
-- main = print $ show $ intersections (C (-25) 0) (L (P (25 % 4) (75 % 8)) (59%12))
main = print $ show $ chord_and_tangent (C (-25) 0 (P 0 0)) (P (25 % 4) (75 % 8)) (P (25 % 4) (75 % 8)) 

