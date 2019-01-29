module HighOrder where

  multThree :: (Num a) => a -> a -> a -> a  
  multThree x y z = x * y * z

  -- curried function no need for var declaration
  compareWithHundred :: (Num a, Ord a) => a -> Ordering  
  compareWithHundred = compare 100  


  -- you can also partially apply infix operators like
  divideByTen :: (Floating a) => a -> a
  divideByTen = (/10)

  isUpperAlphanum :: Char -> Bool  
  isUpperAlphanum = (`elem` ['A'..'Z'])  


  -- creating high order functios
  applyTwice :: (a -> a) -> a -> a  
  applyTwice f x = f (f x)  

  -- redefining standard library zipWith
  -- higher funcs are versatile 
  zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
  zipWith' _ [] _ = []  
  zipWith' _ _ [] = []  
  zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

  -- simple library flips args to func
  flip' :: (a -> b -> c) -> b -> a -> c  
  flip' f y x = f x y  

  -- more composed func fun
  map' :: (a -> b) -> [a] -> [b]  
  map' _ [] = []  
  -- apply func and add to arr recursively
  map' f (x:xs) = f x : map f xs  

  quicksort :: (Ord a) => [a] -> [a]    
  quicksort [] = []    
  quicksort (x:xs) =     
      let smallerSorted = quicksort (filter (<=x) xs)  
          biggerSorted = quicksort (filter (>x) xs)   
      in  smallerSorted ++ [x] ++ biggerSorted  


  largestDivisible :: (Integral a) => a  
  largestDivisible = head (filter p [100000,99999..])  
      where p x = x `mod` 3829 == 0  

  -- Collatz sequences 
  chain :: (Integral a) => a -> [a]  
  chain 1 = [1]  
  chain n  
      | even n =  n:chain (n `div` 2)  
      | odd n  =  n:chain (n*3 + 1)  
  

  -- lambdas are anonymous funcs defined like:
  --
  -- (\xs -> length xs > 15)
  numLongChains :: (Integral a) => a -> Int
  numLongChains n = length (filter (\xs -> length xs > 15) (map chain [1..n])) 

  -- you can also pattern match in lambda funcs:
  --
  -- map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]  

  -- These two are identical given funcs are curried by default


  addIdentical :: (Num a) => a -> a -> a -> a  
  addIdentical x y z = x + y + z  

  addIdentical' :: (Num a) => a -> a -> a -> a  
  addIdentical' = \x -> \y -> \z -> x + y + z

  -- sometimes it makes code easier to read when using \x -> \y in lambda's
  flip'' :: (a -> b -> c) -> b -> a -> c  
  flip'' f x y = f y x 

  -- this one is easier to read then ^^
  flip''' :: (a -> b -> c) -> b -> a -> c  
  flip''' f = \x y -> f y x 

  -- intro to folds
  -- foldl folds the list from left to right 
  -- it takes a binary function  with two args 
  -- its like `reduce` func in javascript
  sum' :: (Num a) => [a] -> a  
  -- \acc x -> acc + x is the binary func
  sum' xs = foldl (\acc x -> acc + x) 0 xs

  -- since funcs are curried
  sum'' :: (Num a) => [a] -> a  
  sum'' = foldl (+) 0

  elem' :: (Eq a) => a -> [a] -> Bool  
  elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  
  
  map'' :: (a -> b) -> [a] -> [b]
  map'' f xs = foldr(\x acc -> f x : acc) [] xs

  -- these are some standard libraries and how folds work
  maximum' :: (Ord a) => [a] -> a  
  maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
    
  reverse' :: [a] -> [a]  
  reverse' = foldl (\acc x -> x : acc) []  
    
  product' :: (Num a) => [a] -> a  
  product' = foldr1 (*)  
    
  filter' :: (a -> Bool) -> [a] -> [a]  
  filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
    
  head' :: [a] -> a  
  head' = foldr1 (\x _ -> x)  
    
  last' :: [a] -> a  
  last' = foldl1 (\_ x -> x)  
