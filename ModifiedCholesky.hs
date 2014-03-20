{-# LANGUAGE RankNTypes #-}
module ModifiedCholesky where

import Numeric.LinearAlgebra hiding (readMatrix)
import Control.Monad.ST
import Control.Monad
import Data.Packed.ST
import Utils

matA = fromLists [[4,2,5,3],[2,2,6,2],[5,6,4,1],[3,2,1,2]] :: Matrix Double

matB = fromLists [[4,12,-16],[12,37,-43],[-16,-43,98]] :: Matrix Double


modifiedCholesky :: Double -> Matrix Double -> (Vector Double,Matrix Double)
modifiedCholesky epsilon a = runST $ do
  a' <- thawMatrix a
  modifiedCholesky_ epsilon (mcBeta a) (rows a) a'

modifiedCholeskyUnsafe :: Double -> Matrix Double -> (Vector Double,Matrix Double)
modifiedCholeskyUnsafe epsilon a = runST $ do
  a' <- unsafeThawMatrix a
  modifiedCholesky_ epsilon (mcBeta a) (rows a) a'


modifiedCholesky_ :: Double -> Double -> Int -> (forall s . (STMatrix s Double -> ST s (Vector Double,Matrix Double)))
modifiedCholesky_ epsilon beta n a = do
  e <- newVector 0 n
  forM_ [0..n-1] $ \k -> do
    mu <- if k == n-1 then return 0 else fmap maximum' $ forM [k+1..n-1] $ \j -> fmap abs $ readMatrix a k j 
    akk <- readMatrix a k k
    let rkk = maximum [epsilon,mu/beta,sqrt $ abs akk]
    writeMatrix a k k $ rkk
    writeVector e k $ rkk^2 - akk
    forM [0..k-1] $ \j -> writeMatrix a k j 0
    forM [k+1..n-1] $ \j -> modifyMatrix a k j (/rkk)
    forM [k+1..n-1] $ \j -> do    
      rkj <- readMatrix a k j
      forM [k+1..n-1] $ \i -> do
        rki <- readMatrix a k i
        modifyMatrix a i j $ (\a -> a - rkj*rki)
  r <- freezeMatrix a
  e' <- freezeVector e
  return (e',r)
  
mcBeta :: Matrix Double -> Double
mcBeta a = if n == 0 then error "wtf" else sqrt $ maximum [gamma,xi/(sqrt (fromIntegral $ n^2-1)),m_epsilon]
  where n = rows a
        gamma = maximum' $ map (\k -> abs $ a@@>(k,k)) [0..n-1]
        xi = maximum' $ 0:[abs $ a@@>(i,j) | i <- [0..n-1], j <- [i+1..n-1]]
             
