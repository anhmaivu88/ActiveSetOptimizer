module EqualityConstrainedSolve where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Calculus
import Polynomial
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util
import Control.Monad.Loops
import System.Random
import Data.Random.Normal
import Data.Maybe
import Utils
import Debug.Trace
import Constraint
import LineSearch
import ModifiedCholesky

equalityConstrainedSolve :: (Objective f) =>
                            (Double,Double)                                           -- Linesearch params
                            -> Double                                                 -- epsilon
                            -> f                                                      -- objective fun
                            -> InequalityConstraint                                   -- inactive constraints
                            -> EqualityConstraint                                     -- active constraints
                            -> (EqualityConstraint -> Stalk -> Vector Double)         -- Direction Function
                            -> IO (InequalityConstraint,EqualityConstraint)
equalityConstrainedSolve cs eps f iqc eqc searchDirection =  flip evalStateT (iqc,eqc,evalAll f $ initialPoint eqc) $ do
  whileM_ (get >>= (equalityConstrainedSolveStop eps)) $ do
    (iqc',eqc'@(EQC a b z x),s) <- get
    let pz = searchDirection eqc' s
        p = z <> pz
        alpha_m = longestActiveStep iqc' x p
        (lsd,alpha) = strongLineSearch cs f x p alpha_m
        x' = x + (scale alpha p) 
--    liftIO $ putStrLn $ "alpha_m = " ++ (show alpha_m)
--    liftIO $ printLSD (lsd,alpha)
    if alpha_m > alpha 
      then put $ (iqc',EQC a b z x',evalAll f x')
      else do
      let (iqc'',eqc''@(EQC a' _ _ _)) = updateEQC iqc' $ EQC a b z x'
--      liftIO $ putStrLn $ " Adding " ++ (show $ (rows a') - (rows a)) ++ " new constraints.  Total = " ++ (show $ rows a')
      put (iqc'',eqc'',evalAll f x')
  fmap (\(x,y,_) -> (x,y)) get

equalityConstrainedSolveStop eps (_,EQC a b z x,(y,g,h)) = do
  let (m,n) = size a 
  if (m==n) then do
--   liftIO $ putStrLn $ "  Won By Rank = " ++ (show m)
    return False 
    else do
    let gradVal = norm2 ((trans z) <> g)
--    liftIO $ putStrLn $ "   Value of Gradient = " ++ (show gradVal)
--    liftIO $ putStrLn $ "   Value of Objective = " ++ (show $ y)
    return $ gradVal > eps

newtonEqualityConstrainedSolve cs eps f iqc eqc = equalityConstrainedSolve cs eps f iqc eqc (newtonDirection 0.0001)

newtonDirection :: Double -> EqualityConstraint -> Stalk -> Vector Double
newtonDirection epsilon eqc@(EQC a b z x) (y,g,h) = result
  where zt = trans z
        (e,h0) = modifiedCholeskyUnsafe epsilon $ zt <> h <> z
        ztg = fromColumns [scale (-1) $ zt <> g]
        [result] = toColumns $ cholSolve h0 ztg

x2_y2 = Poly [Mon [2,0] 1,Mon [0,2] 1]
eqc = EQC (fromLists [[1,1]]) (fromList [1]) (fromLists [[1],[-1]]) (fromList [3,-2])