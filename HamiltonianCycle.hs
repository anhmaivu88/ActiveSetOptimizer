module HamiltonianCycle where

import Data.Tuple.HT
import Data.List
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util
import RandomGraph
import ModifiedCholesky
import Calculus
import Utils
import Data.Maybe
import System.Random
import Control.Monad
import LineSearch
import Constraint
import Data.IORef
import ActiveSetSolve
import System.CPUTime

lookupDefault y i x = case lookup i x of
  (Just a) -> a
  Nothing -> y

hessian_ix :: (Int,Int) -> (Int,Int) -> Matrix Double -> Matrix Double -> Double
hessian_ix (i,j) (k,l) p a 
  | i==k || j==l || p@@>(i,j) == 0 || p@@>(k,l) == 0 = 0
  | otherwise = ((-1)^(i+j + khat + lhat+1)) * (det $ (a ? ([0..(rows a)-1]\\[i,k])) ¿ ([0.. (cols a)-1]\\[j,l]))
  where khat = if k > i then k-1 else k
        lhat = if l > j then l-1 else l
       

gradient_ix :: (Int,Int) -> Matrix Double -> Matrix Double -> Double
gradient_ix (i,j) p a 
  | (p@@>(i,j)) == 0 = 0
  | otherwise = ((-1)^(i+j)) * (det $ (a ? ([0..(rows a)-1]\\[i])) ¿ ([0.. (cols a)-1]\\[j]))


aFromCyc (HCO p ls) x = buildMatrix (rows p) (cols p) 
                        $ \(i,j) -> (if i == j then 1 else 0)-(lookupDefault 0 (i,j) coefs) +(1/(fromIntegral $ rows p))
  where coefs = zipWithLength (,) ls $ toList x

hessianHC :: HamCycObj -> Matrix Double -> Matrix Double
hessianHC (HCO p ls) a = buildMatrix (length ls) (length ls)
                         $ \(i,j) -> hessian_ix (ls!!i) (ls!!j) p a
                        
gradientHC :: HamCycObj -> Matrix Double -> Vector Double
gradientHC (HCO p ls) a = fromList $ flip map ls
                          $ \(i,j) -> gradient_ix (i,j) p a


toHamCycObj :: Matrix Double -> HamCycObj
toHamCycObj m = HCO m $ [(i,j) | i <- [0..(rows m)-1], j <- [0..(rows m)-1], (m@@>(i,j)) /= 0]

data HamCycObj = HCO (Matrix Double) [(Int,Int)] deriving (Show)

instance Evaluable HamCycObj where
  domain (HCO _ l) = length l
  eval f x = fst3 $ evalAll f x
  
instance Objective HamCycObj where
  evalAll hco@(HCO g l) x = let a = aFromCyc hco x
                            in (-(det a),gradientHC hco a,hessianHC hco a)


stochasticConstraints_ :: ((Int,Int) -> Int) -> HamCycObj -> [[Double]]
stochasticConstraints_  len hco@(HCO g l) = result
  where rowsIxs = map (\i -> map fst $ filter ((==i).len.snd) $ zip [0..] l) $ [0..(rows g)-1]
        result = map (\ixs -> map (\x -> if x`elem`ixs then 1 else 0) [0..n-1])  rowsIxs
        n = domain hco
stochasticConstraints hco = fromLists $ stochasticConstraints_ fst hco
doubleStochasticConstraints hco = fullRowRank (length sc) $ map fromList $ sc ++ (stochasticConstraints_ snd hco)
  where sc = stochasticConstraints_ fst hco

inequalityConstraints :: Bool -> HamCycObj -> InequalityConstraint
inequalityConstraints dub hco@(HCO g l) = 
  IC ((if (length bads) == n
         then id
         else ((fromRows $ map (\i -> fromList [if j== i then -1 else 0 | j <- [0..n-1]]) $ filter notBad [0..n-1])#))
        $ (diag $ fromList $ replicate (domain hco) 1))
    $ fromList $ (replicate (length $ filter notBad [0..n-1]) $ -1) ++ (replicate (domain hco) 0)
  where bads = (fmap fst $ filter (\(_,(i,j)) -> 1 >= (sum' $ concat $ toLists $ g ? [i])) $ zip [0..] $ l) ++ 
               (if dub then(fmap fst $ filter (\(_,(i,j)) -> 1 >= (sum' $ concat $ toLists $ g ¿ [j])) $ zip [0..] $ l)  else [])
        notBad i = not $ i`elem`bads
        n = domain hco
-- 0 < x + ap < 1
-- a > -x/p 
-- a > (1-x)/p

findInitialPoint :: Matrix Double -> IO (Vector Double)
findInitialPoint a = do
  let [p0] = toColumns $ linearSolveLS a (ones (rows a) 1)
      ns' = nullspacePrec 1 a
  if null ns' then return (p0) else do
    let ns = fromColumns ns'
    p <- fmap fromList $ mapM (const $ randomRIO (-1,1)) [1..cols ns]
    let np = ns <>p
        max_step = minimum' $ map (\i -> if np@>i >= 0 then infty else -(p0@>i)/(np@>i)) [0..(dim np)-1]
        max_step2 = minimum' $ map (\i -> if np@>i <= 0 then infty else (1-(p0@>i))/(np@>i)) [0..(dim np)-1]
    alpha <- randomRIO (0,min max_step2 max_step)
    return $ p0 + (scale alpha np)

testAllRegGraphs :: Bool ->  IO ()
testAllRegGraphs b = do
  xs <- forM [5..20] $ \i -> testRegGraphs b i
  writeFile ("res_" ++ (show b)) $ (unlines $ map (\(i,j,k) -> unwords [show i,show j,show k]) xs)
     
testAllRandGraphs :: IO ()
testAllRandGraphs = do
  putStrLn "True"
--  xs <- forM [5..12] $ \i -> testRandGraphs True i
--  writeFile ("res3_" ++ (show True)) $ (unlines $ map (\(i,j,k) -> unwords [show i,show j,show k]) xs)
--  putStrLn "False"
--  xs <- forM [5..12] $ \i -> testRandGraphs False i
--  writeFile ("res3_" ++ (show False)) $ (unlines $ map (\(i,j,k) -> unwords [show i,show j,show k]) xs)
--  putStrLn "True"
--  xs <- forM [15..20] $ \i -> testRandGraphs True i
 -- writeFile ("res4_" ++ (show True)) $ (unlines $ map (\(i,j,k) -> unwords [show i,show j,show k]) xs)
  putStrLn "False"
  xs <- forM [14..20] $ \i -> testRandGraphs False i
  writeFile ("res4_" ++ (show False)) $ (unlines $ map (\(i,j,k) -> unwords [show i,show j,show k]) xs)
  


testRandGraphs :: Bool -> Int -> IO (Double,Double,Double)
testRandGraphs doubly n = do
  putStrLn $ " TEST n = " ++ (show n)
  gs <- mapM (const $ randomGraph n (9/(fromIntegral $ n)) True) [1..10]
  bs <- fmap (filter (\(i,j) -> i /= -1)) $ mapM (hamiltonianCycle doubly 10) gs
  let wins = (fromIntegral $ length bs) / 10
      avgIts = (sum' $ map (fromIntegral . fst) bs)/(fromIntegral $ length bs)
      avgTime = (sum' $ map ((/10^9) . fromIntegral . snd) bs)/(fromIntegral $ length bs)
  putStrLn $ (show (wins,avgIts,avgTime))
  return (wins,avgIts,avgTime)

                  
testRegGraphs :: Bool -> Int -> IO (Double,Double,Double)
testRegGraphs doubly n = do
  putStrLn $ " TEST n = " ++ (show n)
  gs <- fmap (take 25) $ loadRegGraphs n
  bs <- fmap (filter (\(i,j) -> i /= -1)) $ mapM (hamiltonianCycle doubly 10) gs
  let wins = (fromIntegral $ length bs) / 25
      avgIts = (sum' $ map (fromIntegral . fst) bs)/(fromIntegral $ length bs)
      avgTime = (sum' $ map ((/10^9) . fromIntegral . snd) bs)/(fromIntegral $ length bs)
  putStrLn $ "ANSWER = " ++ (show (wins,avgIts,avgTime))
  return (wins,avgIts,avgTime)

toActiveSet :: Bool -> HamCycObj -> IO ActiveSet
toActiveSet doubly hco = do
  let aMat = if doubly then doubleStochasticConstraints hco else stochasticConstraints hco
      iqc = inequalityConstraints doubly hco
      bVec = fromList $ replicate (rows aMat) 1
  x0 <- findInitialPoint aMat
  return $ AS (mkEQC aMat bVec x0) (rows aMat) iqc
  
hamiltonianCycle :: Bool -> Int -> Matrix Double -> IO (Int,Integer)
hamiltonianCycle doubly numTries g = do
  let hco = toHamCycObj g
  t <- getCPUTime
  found <- newIORef $ -1
  forM [1..numTries] $ \i -> (>>=) (readIORef found) $ \f -> when (f == -1) $ do
--    putStrLn $ "---------BEGINNING ATTEMPT NUMBER " ++ (show i) ++ "--------------"
    as <- toActiveSet doubly hco
    x0 <- activeSetSolve (0.1,0.9) 0.0001 hco as
    let fx0 = eval hco x0
--    putStrLn $ "minimizer found = " ++ (show fx0)
    writeIORef found $ if (round fx0) == -(rows g) then i else -1
  t' <- getCPUTime
  its <- readIORef found
  return (its,t'-t)


h = 0.0000001

testMeG :: Int -> Int -> Double -> Bool -> IO ()
testMeG reps n density b = forM_ [1..reps] $ \_ -> do
  hco <- fmap toHamCycObj $ randomGraph n density b
--  hco <- fmap toObjective $ getStdRandom $ randomBoundedBelowPolynomial 2 
  x <- fmap fromList $ mapM (const $ randomIO) [1..domain hco]
  let (fx,gx,hx) = evalAll hco x      
  forM_ [0..domain hco-1] $ \j -> do
    forM_ [0..domain hco-1] $ \k -> do
      print $ (j,k)
      let (fxe,gxe,hxe) = evalAll hco $ x + (fromList [if i == k then h else 0 | i <- [0..domain hco-1]])
          h_hat = ((gxe@>j) -(gx@>j))/h
          diff =  h_hat - (hx@@>(j,k))
      when (abs diff > 0.0001) $ do
        putStrLn "YELP"
        putStrLn $ " approx diff = " ++ (show $ h_hat - (hx@@>(j,k)))
        putStrLn $ " in abs = " ++ (show $ (h_hat,(hx@@>(j,k))))
