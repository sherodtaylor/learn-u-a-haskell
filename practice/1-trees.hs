module Tree where
  data Tree a = EmptyNode
              | FilledNode a (Tree a) (Tree a)
              deriving (Eq,Ord,Show,Read)

  buildLeaf :: (Ord a, Num a) => a -> Tree a
  buildLeaf x = FilledNode x EmptyNode EmptyNode

  heightOfTree :: (Ord a, Num a) => Tree a -> a
  heightOfTree EmptyNode = 0
  heightOfTree (FilledNode _ left right) =
    let leftHeight  = (heightOfTree left) + 1
        rightHeight = (heightOfTree right) + 1
    in max leftHeight rightHeight

  inOrderArray :: (Ord a, Num a) => Tree a -> [a]
  inOrderArray EmptyNode = []
  inOrderArray (FilledNode a left right) =
    let leftArr  = inOrderArray left
        rightArr = inOrderArray right
    in leftArr ++ [a] ++ rightArr

  preOrderArray :: (Ord a, Num a) => Tree a -> [a]
  preOrderArray EmptyNode = []
  preOrderArray (FilledNode a left right) =
    [a] ++ (preOrderArray left) ++ (preOrderArray right)
  
  postOrderArray :: (Ord a, Num a) => Tree a -> [a]
  postOrderArray EmptyNode = []
  postOrderArray (FilledNode a left right) =
    (postOrderArray left) ++ (postOrderArray right) ++ [a] 
  


  
  
