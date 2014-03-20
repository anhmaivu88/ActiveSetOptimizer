module LineSearch where

import Calculus
import Polynomial
import Numeric.LinearAlgebra
import Control.Monad.State
import Control.Monad.Loops
import System.Random
import Data.Random.Normal
import Data.Maybe
import Utils
import Debug.Trace
import Text.Printf

lineSearchDefaultParams = (0.1,0.9)

strongLineSearch_ :: (Double,Double) -> (Double -> (Double,Double)) -> Double -> ((Double,Int),Double)
strongLineSearch_ (c_1,c_2) f max_step = let (f0,f'0) = f 0
                                         in flip evalState (0,Nothing,0,f0) $ do
  if (f'0 >= -m_epsilon) then error "Cannot line search up hill, stupid" else do
  whileM (fmap (\(a,b,_,_) -> b /= (Just a)) get) $ do
    (a,b,c,_) <- get
    let x = if isNothing b then min max_step (a + 1) else (a+(fromJust b))/2
    let (fx,f'x) = f x
    if fx < c_1 * f'0 * x + f0 
      then do
      if c_2 * abs f'0 >= abs f'x
        then put (x,Just x,c+1,fx)
        else if f'x < 0 then (if x == max_step then put (x,Just x,c+1,fx) else put (x,b,c+1,fx)) else put (a,Just x,c+1,fx)
      else put (a,Just x,c+1,fx)
  fmap (\(a,b,c,fx) -> ((fx,c),a)) get
  
strongLineSearch :: (Objective f) => (Double,Double) -> f -> Vector Double -> Vector Double -> Double -> ((Double,Int),Double)
strongLineSearch params f x p max_step = strongLineSearch_ params (lineRestrict f x p) max_step

printLSD ((fx,c),alpha) = putStrLn $ "   Linesearch completed in " ++ (show c) ++ " iterations with result " ++ (printf "%.3f" fx) ++ " and alpha = " ++ (printf "%.3f" alpha)

randomBoundedBelowPolynomial :: Int -> StdGen -> (Polynomial,StdGen)
randomBoundedBelowPolynomial n g = (f*f,g')
  where (f,g') = randomPolynomial n g (randomR (1,7)) (randomR (1,3)) normal

testLineSearch :: Int -> Int -> IO ()
testLineSearch d n = do
  forM_ [1..n] $ \_ -> do
    f <- fmap toObjective $ getStdRandom $ randomBoundedBelowPolynomial d
    x <- fmap fromList $ mapM (const $ getStdRandom normal) [1..d]
--    g <- fmap fromList $ mapM (const $ getStdRandom normal) [1..d]    
    let (_,alpha) = strongLineSearch (0.1,0.9) f x g 3
        g0 = directionalDerivative f x g
        f0 = eval f x
        f1 = lineRestrictFunction f x g alpha
        g = scale (-1) $ gradientAt f x
    when (g0 < - sqrt m_epsilon) $ do
      putStrLn $ "   f'(0) = " ++ (show g0)
      putStrLn $ "   f'(alpha) =  " ++ (show $ lineRestrictPrime f x g alpha)
      putStrLn $ "   gain of " ++ (show $ f0 - f1)
      putStrLn $ "   value moved " ++ (show $ alpha)
      when (g0 * alpha * 0.1 + f0 < f1) $ putStrLn "  Not under c_1 line"
      when (g0 *  0.9 > (lineRestrictPrime f x g alpha)) $ putStrLn "   FAILED CURVATURE CONDITION"
      when (abs (g0 *  0.9) < (abs $ lineRestrictPrime f x g alpha)) $ putStrLn "   FAILED STRONG CURVATURE CONDITION"
      when (alpha>3) $ putStrLn "too big"
      when (f0 < f1) $  putStrLn "   NO OPT!!!"
      putStrLn ""

   
