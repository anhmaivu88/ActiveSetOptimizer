module RandomGraph where

import Data.Array
import Control.Monad
import Numeric.LinearAlgebra
import Data.Packed.Matrix
import System.Random

randomGraph_ :: Int -> Double -> Bool -> IO (Matrix Double)
randomGraph_ n density force_hamiltonian = do
  elts <- fmap (listArray (0,n^2-1)) $ mapM (const $ randomIO >>= (\x -> return $ if x < density then 1 else 0)) [0..n^2-1]
  return $ buildMatrix n n (\(i,j) -> if force_hamiltonian && (i - ((j+1)`mod`n)) == 0
                                      then 1.0 
                                      else if i == j then 0.0 else fromInteger $ elts!(i * n + j))

checkDegrees a = (all (\i -> 2 <= (sum $ map (\j -> a@@>(i,j)) [0..(rows a)-1])) [0..(rows a)-1])
                 && (all (\i -> 2 <= (sum $ map (\j -> a@@>(j,i)) [0..(rows a)-1])) [0..(rows a)-1])

randomGraph n d f = do
  rg <- randomGraph_ n d f
  fixGraph rg
  
fixGraph g = do
  if (checkDegrees g)
    then return g
    else do
    putStrLn "retry"
    i <- randomRIO (0,(rows g)-1)
    j <- randomRIO (0,(rows g)-1)
    let a = buildMatrix (rows g) (rows g)  $ \ij -> if ij == (i,j) then 1 else 0
    fixGraph $ mapMatrix (min 1) $ g + a


loadRegGraphs :: Int -> IO [Matrix Double]
loadRegGraphs n = do
  gs <- fmap (map (map read) . map words . lines) $ readFile $ "g" ++ (show n)
  return $ wee gs 
  where wee [] = []
        wee h = (fromLists $ take n h):(wee $ drop n h)
