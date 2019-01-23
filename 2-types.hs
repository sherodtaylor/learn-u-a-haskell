module Start where
  circumference :: Double -> Double  
  circumference r = 2 * pi * r 

  lucky :: (Integral a) => a -> String  
  lucky 7 = "LUCKY NUMBER SEVEN!"  
  lucky x = "Sorry, you're out of luck, pal!"  

  sayMe :: (Integral a) => a -> String  
  sayMe 1 = "One!"  
  sayMe 2 = "Two!"  
  sayMe 3 = "Three!"  
  sayMe 4 = "Four!"  
  sayMe 5 = "Five!"  
  sayMe x = "Not between 1 and 5"  


  factorial :: (Integral a) => a -> a  
  factorial 0 = 1  
  factorial n = n * factorial (n - 1)  

  -- pattern matching can fail should always define a catch all
  charName :: Char -> String  
  charName 'a' = "Albert"  
  charName 'b' = "Broseph"  
  charName 'c' = "Cecil"  
  
  addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
  addVectors a b = (fst a + fst b, snd a + snd b)  

  -- pattern matching could be used on tuples 
  addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)  
  addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

  -- head implementation pattern match
  head' :: [a] -> a  
  head' [] = error "Can't call head on an empty list, dummy!"  
  head' (x:_) = x

  -- tell pattern match list with colon
  tell :: (Show a) => [a] -> String  
  tell [] = "The list is empty"  
  tell (x:[]) = "The list has one element: " ++ show x  
  tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
  tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

  -- recursive pattern matching
  length' :: (Num b) => [a] -> b  
  length' [] = 0  -- base case
  length' (_:xs) = 1 + length' xs  


  -- recursive sum
  sum' :: (Num a) => [a] -> a  
  sum' [] = 0  
  sum' (x:xs) = x + sum' xs  


  -- capital using @ caputure pattern patching
  capital :: String -> String  
  capital "" = "Empty string, whoops!"  
  capital all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]  


  bmiTell :: (RealFloat a) => a -> String  
  bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"

  bmiTellWeight :: (RealFloat a) => a -> a -> String  
  bmiTellWeight weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!" 
  

  -- Using where after guards to keep dry
  bmiTellWhere :: (RealFloat a) => a -> a -> String  
  bmiTellWhere weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  


  -- get initals and as you can see you can do pattern matching
  initials :: String -> String -> String  
  initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname    


  -- using functions in guard where
  calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
  calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2


  -- let bindings
  cylinder :: (RealFloat a) => a -> a -> a  
  cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 

  -- case expressions are synatactic sugar
  -- syntax:
  -- case expression of pattern -> result  
  --                    pattern -> result  
  --                    pattern -> result  
  --                    ...  
  --
  -- the following two are the same
  head'' :: [a] -> a  
  head'' [] = error "No head for empty lists!"  
  head'' (x:_) = x 


  -- you can also pattern match inside cases
  head''' :: [a] -> a  
  head''' xs = case xs of [] -> error "No head for empty lists!"  
                          (x:_) -> x  


  -- pattern matching on function params can only be done when defining functions
  -- where case exp can be used pretty much anywhere
  describeList :: [a] -> String  
  describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                                 [x] -> "a singleton list."   
                                                 xs -> "a longer list."  

  describeList' :: [a] -> String  
  describeList' xs = "The list is " ++ what xs  
      where what [] = "empty."  
            what [x] = "a singleton list."  
            what xs = "a longer list."  
