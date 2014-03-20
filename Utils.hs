module Utils where


import Data.List

cim a f l = concat $ intersperse a $ map f l

sum' l = foldl' (+) 0  l
product' l = foldl' (*) 1 l
maximum' (l1:l) = foldl' max l1 l
minimum' (l1:l) = foldl' min l1 l
       
swap (x,y) = (y,x)
zipWithLength _ [] [] = []
zipWithLength f (a:as) (b:bs) = (f a b):(zipWithLength f as bs)
zipWithLength _ _ _ = error "zipWithLength length mismatch"

m_epsilon = 10^^(-15) :: Double

infty = 999999999 :: Double

maybeDefault x Nothing = x
maybeDefault _ (Just a) = a

fromRight (Right a) = a