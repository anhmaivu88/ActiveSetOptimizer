module Polynomial where

import Utils
import System.Random
import Data.List
import Numeric.LinearAlgebra
import Calculus
import Text.Printf

data Monomial = Mon [Int] Double deriving (Eq,Ord)

newtype Polynomial = Poly [Monomial]

showExp 1 = ""
showExp i = "^" ++ texShow i

texShow i 
  | i < 10 = (show i)
  | otherwise = "{" ++ (show i) ++ "}" 

monMult (Mon coefs x) (Mon coefs2 y) = Mon (zipWithLength (+) coefs coefs2) (x*y)

instance Num Polynomial where
  (Poly g) * (Poly f) = Poly $ concatMap (\x -> map (monMult x) f) g
  (Poly g) + (Poly f) = Poly $ g ++ f
  negate (Poly g) = Poly $ map (\(Mon t c) -> Mon t (-c)) g
  fromInteger 0 = Poly []
  fromInteger _ = error "fromintegral polynomial"
  abs _ = error ""
  signum _ = error "" 

constPoly n x = Poly [Mon (replicate n 0) x]

instance Show Monomial where
  show (Mon _ 0) = "0" 
  show (Mon pows coef) = (printf "%0.3f" coef) ++ (concat $ zipWith (\i e -> if e == 0 then "" else "x_" ++ (texShow i) ++ (showExp e)) [1..] pows)
  
instance Show Polynomial where
  show (Poly mons) = cim " + " show mons
  
randomMonomial :: Int -> StdGen -> (StdGen -> (Int,StdGen)) -> (StdGen -> (Double,StdGen)) -> (Monomial,StdGen)
randomMonomial n g powGen coefGen = (Mon pows c,g'')
  where (c,g') = coefGen g
        (g'',pows) = mapAccumL (\acc r -> swap $ powGen acc) g' [1..n]

randomPolynomial :: Int -> StdGen -> 
                    (StdGen -> (Int,StdGen)) -> (StdGen -> (Int,StdGen)) -> (StdGen -> (Double,StdGen)) -> 
                    (Polynomial,StdGen)
randomPolynomial n g numTerms powGen coefGen = (Poly mons,g'')
  where (t,g') = numTerms g
        (g'',mons) =  mapAccumL (\acc r -> swap $ randomMonomial n acc powGen coefGen) g' [1..t]

instance Evaluable Monomial where
  eval (Mon pows coef) xs = coef * (product' $ zipWithLength (\e x -> x^e) pows $ toList xs)
  domain (Mon pows coef) = length pows
  
instance Evaluable Polynomial where
  eval (Poly mons) xs = sum' $ map (flip eval xs) mons
  domain (Poly mons) = let (d:mdoms) = map domain mons in if all (==d) mdoms then d else error "Polynomial made of incorrect number monomials"
  
instance Differentiable Monomial where
  gradient (Mon [] coef) = []
  gradient (Mon (p:pows) coef) = let gradlast = gradient (Mon pows coef)
                                 in (if p == 0 then Mon (map (const 0) $ 0:pows) 0
                                    else (Mon ((p-1):pows) (fromIntegral p * coef)))
                                         :(map (\(Mon pws c) -> (Mon (p:pws) c)) gradlast)
                                    
instance Differentiable Polynomial where
  gradient (Poly [p]) = map (Poly . return) $ gradient p
  gradient (Poly (m:mons)) = zipWith (\m' (Poly p') -> Poly (m':p')) (gradient m) (gradient $ Poly mons)
  
