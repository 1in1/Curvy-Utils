module PreliminaryNumberTheory where

import Debug.Trace
import Control.Exception
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Ratio
import qualified Data.Map as Map

sieve xs = sieve' xs Map.empty where
    sieve' [] table = []
    sieve' (x:xs) table =
        case Map.lookup x table of
            Nothing -> x : sieve' xs (Map.insert (x*x) [x] table)
            Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts) where
                reinsert table prime = Map.insertWith (++) (x+prime) [prime] table

primesUpTo :: Integer -> [Integer]
primesUpTo n = assert (n > 0) $ sieve [2..n]

primeFactors :: Integer -> [Integer]
primeFactors n | n == 0 = []
               | n < 0 = primeFactors (-n)
               | otherwise = filter ((== 0) . mod n) $ primesUpTo n

isSquare :: Rational -> Bool
isSquare = isJust . ratSqRoot

isSquareModN :: Integer -> Integer -> Bool
isSquareModN p x = mod x p `elem` [(x^2) `mod` p | x <- [0..(p-1)]]

-- Does there exist a solution in integers to w^2 = au^4 + bu^2 v^2 + cv^4,
-- such that not all of w, u, v are 0?
-- In general, this is an unsolved problem. We can perform a few basic checks, however
-- TODO: establish a mechanism to attack this with infinite descent?
existsIntegerSolution :: Integer -> Integer -> Integer -> Maybe Bool
existsIntegerSolution a b c 
    | contradictionByParity = Just False
    | (any contradictionByConsiderationModP . primesUpTo . foldl lcm 1 . filter (/= 0) . map abs) [a, b, c] = Just False
    | existsBruteForceSolution 1000 = Just True
    | otherwise = Nothing where -- For now
        eval (u, v) = a*(u^4) + b*(u^2)*(v^2) + c*(v^4)

        -- We cannot solve over R, let alone Z, if the LHS is positive but the RHS is negative
        contradictionByParity = all (<0) [a, b, c] || 
            all (<0) [a, -(4*a*c - (b^2))] ||
            all (<0) [c, -(4*c*a - (b^2))]

        -- Assume that u, v \in Z are coprime (else pull thier gcd into w), and consider the reduced equation at p. We may be able to establish a contradiction
        -- We can definitely make this work better...
        contradictionByConsiderationModP p = null solutionsModuloP where
            squaresModP = (nub . map ((`mod` p) . (^2))) [0..(p-1)]
            -- There are certain scenarios where we can rule out pairs (u,v) _before_ we pass to the reduced polynomial
            -- In particular, if p|u, (resp. p|v), does this force p|v (resp. p|u)?
            -- This is the case in the following specific scenario
            uFilterOut =
                if ((== 0) . (`mod` p)) a && ((/= 0) . (`mod` (p^2))) a then [(u, 0) | u <- [1..(p-1)]] -- p|v => p|u
                else []
            vFilterOut =
                if ((== 0) . (`mod` p)) c && ((/= 0) . (`mod` (p^2))) c then [(0, v) | v <- [1..(p-1)]] -- p|u => p|v
                else []
            filterOut = (0,0):(uFilterOut ++ vFilterOut)

            solutionsModuloP = filter ((`elem` squaresModP) . (`mod` p) . eval) $ ((,) <$> [0..(p-1)] <*> [0..(p-1)]) \\ filterOut

        
        existsBruteForceSolution limit = any (isSquare . toRational . eval) $ filter (/= (0,0)) ((,) <$> [0..limit] <*> [0..limit]) 


ratSqRoot :: Rational -> Maybe Rational
ratSqRoot n | forcedRoot^2 == n = Just forcedRoot
            | otherwise = Nothing where
    forcedSqNum = (floor . sqrt . fromIntegral . abs . numerator) n
    forcedSqDen = (floor . sqrt . fromIntegral . denominator) n
    forcedRoot = forcedSqNum % forcedSqDen

ratCubRoot :: Rational -> Maybe Rational
ratCubRoot n | forcedRoot^3 == n = Just forcedRoot
             | (-forcedRoot)^3 == n = Just (-forcedRoot)
             | otherwise = Nothing where
    forcedCubNum = (floor . (**(1/3)) . fromIntegral . numerator . abs) n
    forcedCubDen = (floor . (**(1/3)) . fromIntegral . denominator) n
    forcedRoot = forcedCubNum % forcedCubDen


-- Find the _rational_ roots of the monic cubic x^3 + ax^2 + bx + c
rationalQuadraticRoots :: Rational -> Rational -> [Rational]
rationalQuadraticRoots a 0 = [0, -a]
rationalQuadraticRoots a b = maybePairOfRoots sqRoot where
    n = a^2 - 4*b
    sqRoot = ratSqRoot n
    maybePairOfRoots (Just sq) = map ((+(-a/2)) . (*(sq/2))) [1, -1]
    maybePairOfRoots Nothing = []

-- The first root can be found via RRT
-- Drastic improvement likely possible by using a floating point algo and searching in that space
rationalCubicRoots :: Rational -> Rational -> Rational -> [Rational]
rationalCubicRoots a b c = roots rationalRoots where
    x3Term = foldl lcm 1 $ map denominator [a, b, c]
    possibleDenominators = filter ((== 0) . mod x3Term) [1..x3Term]
    absConstTerm = abs (x3Term * numerator c)
    factorsOfConstTerm = filter ((== 0) . mod absConstTerm) [1..absConstTerm]
    possibleNumerators = [0] ++ factorsOfConstTerm ++ map negate factorsOfConstTerm

    lagrangianLimit = foldl max 1 $ map abs [a, b, c]
    rationalRoots = filter ((== 0) . (\x -> x^3 + a*x^2 + b*x + c)) $ 
        filter ((<= lagrangianLimit) . abs) $
        (%) <$> possibleNumerators <*> possibleDenominators
    roots (firstRoot:tail) = firstRoot:rationalQuadraticRoots (a + firstRoot) (b + (a + firstRoot)*firstRoot)
    roots [] = []


