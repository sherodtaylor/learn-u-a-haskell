
-- Recursion
module Recursion where

  import Debug.Trace (trace)
  -- In haskell think recurse first
  maximum' :: (Ord a) => [a] -> a  
  maximum' [] = error "maximum of empty list"  
  maximum' [x] = x  
  maximum' (x:xs)   
      | x > maxTail = x  
      | otherwise = maxTail  
      where maxTail = maximum' xs  

  -- use max native max
  maximum'' :: (Ord a) => [a] -> a  
  maximum'' [] = error "maximum of empty list"  
  maximum'' [x] = x  
  maximum'' (x:xs) = max x (maximum' xs)  

  -- replicate Int 
  replicate' :: (Num i, Ord i) => i -> a -> [a]  
  replicate' n x  
      | n <= 0    = []  
      | otherwise = x:replicate' (n-1) x  


  -- take n elem from list
  take' :: (Num i, Ord i) => i -> [a] -> [a]  
  -- check negative number
  take' n _               
      | n <= 0   = []  
  -- match on empty list
  take' _ []     = []  
  -- recurse
  take' n (x:xs) = x : take' (n-1) xs 

  -- take a list and reverse using recursion
  reverse' :: [a] -> [a]  
  reverse' [] = []  
  reverse' (x:xs) = reverse' xs ++ [x]  

  -- simple implementation of repeat
  -- this is an infinitine function and haskell supports them
  repeat' :: a -> [a]  
  repeat' x = x:repeat' x  


  -- zip list takes two lists and returns a tuple
  zip' :: [a] -> [b] -> [(a,b)]  
  zip' _ [] = []  
  zip' [] _ = []  
  zip' (x:xs) (y:ys) = (x,y):zip' xs ys 


  elem' :: (Eq a) => a -> [a] -> Bool
  elem' a [] = False
  elem' a (x:xs)
        | a == x = True
        | otherwise = a `elem'` xs -- back ticks are infix operators which reverse args??


  -- quick sort haskell is optimized for recusion
  -- it is tail call optimized
  quicksort' :: (Ord a) => [a] -> [a]
  quicksort' [] = []  
  quicksort' (x:xs) =  
        let smallerSorted = quicksort' [a | a <- xs, a <= x]  
            biggerSorted = quicksort' [a | a <- xs, a > x]  
        in smallerSorted ++ [x] ++ biggerSorted 
  
