{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calculus where

import Numeric.LinearAlgebra

data Obj f = Obj f [f] [[f]]

type Stalk = (Double,Vector Double, Matrix Double)

type Gradient f = [f]
type Hessian f = [[f]]

class Evaluable f where
  eval :: f -> Vector Double -> Double
  domain :: f -> Int

class (Evaluable f) => Differentiable f where
  gradient :: f -> [f]

instance (Evaluable f) => Evaluable (Obj f) where
  eval (Obj f _ _) x = eval f x
  domain (Obj f _ _) = domain f

class (Evaluable f) => Objective f where
  evalAll :: f -> Vector Double -> Stalk

instance (Evaluable f) => Objective (Obj f) where 
  evalAll (Obj  f g h) x = (eval f x,fromList $ map (flip eval x) g,fromLists $ map (map (flip eval x)) h)
    
toObjective f = let g = gradient f in Obj f g (map gradient g)

hessianAt :: (Objective f) => f -> Vector Double -> Matrix Double
hessianAt f x = let (_,_,h) = evalAll f x in h

gradientAt :: (Objective f) =>  f -> Vector Double -> Vector Double
gradientAt f x = let (_,g,_) = evalAll f x in g


directionalDerivative :: (Objective f) =>  f ->  Vector Double -> Vector Double -> Double
directionalDerivative f x p = (gradientAt f x)`dot`p



lineRestrictFunction :: (Evaluable f) => f -> Vector Double -> Vector Double -> Double -> Double
lineRestrictFunction f x p alpha = eval f $ x + (scale alpha p)

lineRestrictPrime :: (Objective f) => f -> Vector Double -> Vector Double -> Double -> Double
lineRestrictPrime f x p alpha = directionalDerivative f (x + (scale alpha p)) p


lineRestrict :: (Objective f) => f -> Vector Double -> Vector Double -> Double -> (Double,Double)
lineRestrict f x p alpha = let (y,g,_) = evalAll f $ x + (scale alpha p) in (y,g`dot`p)
                        
