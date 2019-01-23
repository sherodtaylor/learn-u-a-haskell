module Tree where
  data Tree a = EmptyNode
              | FilledNode a (Tree a) (Tree a)
              deriving (Eq,Ord,Show,Read)

  heightOfTree :: (Ord a, Num a) => Tree a -> a
  heightOfTree EmptyNode = 0
  heightOfTree (FilledNode _ left right) =
    let leftHeight  = (heightOfTree left) + 1
        rightHeight = (heightOfTree right) + 1
    in max leftHeight rightHeight

  inOrderArray :: a => Tree a -> a
  inOrderArray EmptyNode = 0
  inOrderArray (FilledNode _ left right) =
    let leftVal  = inOrderArray left
        rightHeight = (heightOfTree right) + 1
    in max leftHeight rightHeight

  

  
  
