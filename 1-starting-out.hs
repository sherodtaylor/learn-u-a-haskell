module Start where
  doubleMe x = x + x
  doubleUs x y = doubleMe x + doubleMe y

  -- in if statements you need to define an else statement
  doubleSmallNumber x = if x > 100
                           then x
                           else x*2
  -- ' is a valid character in a function
  doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

  
