module BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f init Leaf = init
foldTree f init (Node left a right) = 
  foldTree f 
  (foldTree f (f a init) left) 
  right

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 (Node Leaf 4 Leaf)) 2 (Node (Node Leaf 5 Leaf) 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 4, 3, 5]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 4, 2, 5, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [4, 1, 5, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
