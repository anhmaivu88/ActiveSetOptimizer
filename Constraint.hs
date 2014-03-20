module Constraint where


import Calculus
import Polynomial
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util
import Control.Monad.State
import Control.Monad.Loops
import System.Random
import Data.Random.Normal
import Data.Maybe
import Utils
import Debug.Trace
import Data.List
import Data.Tuple.HT
                        --     A                  b              Z                x_0
data EqualityConstraint = EQC (Matrix Double) (Vector Double) (Matrix Double) (Vector Double)  deriving (Show)

data InequalityConstraint = IC (Matrix Double) (Vector Double) deriving (Show)

data ActiveSet = AS EqualityConstraint Int InequalityConstraint deriving (Show)

initialPoint (EQC _ _ _ x0) = x0

lagrangeMultipliers :: (Objective f) => f -> EqualityConstraint -> Vector Double
lagrangeMultipliers f (EQC a b z x) = let [lambda] = toColumns $ linearSolve (trans a) 
                                                     (fromColumns [gradientAt f x])
                                      in lambda

longestActiveStep :: InequalityConstraint -> Vector Double -> Vector Double -> Double
longestActiveStep (IC a b) x p = minimum' $ infty:(map (\i -> b_ax_p @> i) $ filter (\i -> ap@>i < 0) $ [0..(dim ap)-1])
  where b_ax_p = (b-a<>x)/ap
        ap = a <> p
        
traceIt x = traceShow x x

fromColumns' a x = if null x then error a else fromColumns x

fullRowRank :: Int -> [Vector Double] -> Matrix Double
fullRowRank 0 _ = error "frr = 0"
fullRowRank ix rows = let trial = fromColumns' "frr1" rows
                      in if rank trial == length rows
                         then (trans trial)
                         else frr (take ix rows) (drop ix rows)
  where frr lis [] = (fromRows lis)
        frr lis (n:ns) 
          | rank (fromColumns' "frr2" $ n:lis) == (length lis) +1 = frr (lis++[n]) ns
          | otherwise                                     = frr lis ns



updateEQC (IC a' b') (EQC a b _ x) = (IC newA' newB',mkEQC newA newB x)
  where (is,notis) = partition (\i -> ax_b@>i <= m_epsilon) [0..(dim b')-1]
        ax_b = a' <> x - b'
        newA = fullRowRank (rows a) $ toRows (a # (a' ? is))
        newA' = a' ? notis
        newB = fromList $ (toList b) ++ (map (b'@>) is)
        newB' = fromList $ map (b'@>) notis

mkEQC :: Matrix Double -> Vector Double -> Vector Double -> EqualityConstraint
mkEQC a b x0 = if (length zlist) + m /= cols a then error $ "mkEQC rank problem " ++ (show $ rank a) ++ "\n" ++ (show z) ++"\n\n" ++ (show a) else EQC a b z x0
  where m = rows a
        zlist = nullspacePrec 1 a
        z = fromColumns' "mkeqc" $ zlist
        
eqcSize (EQC a _ _ _) = rows a

sameA (EQC a _ _ _) (EQC a' _ _ _) = a == a'

updateActiveSet ::  (Objective f) => Double -> f -> ActiveSet -> Either (Vector Double) ActiveSet
updateActiveSet delta f (AS eqc@(EQC a b _ x) ix iqc@(IC a' b')) = if null deadRows then Left x 
                                                                   else Right $ AS (mkEQC newA newB x) ix (IC newA' newB')
  where lambda = lagrangeMultipliers f eqc
        (notDeadRows,deadRows) = partition (\i -> lambda@>i > delta || i < ix) $[0..(dim lambda)-1]
        newA  = a ? notDeadRows
        newA' = a' # (a ? deadRows)
        newB  = fromList $ map (b@>) notDeadRows
        newB' = fromList $ (toList b') ++  (map (b@>) deadRows)
        
        
        