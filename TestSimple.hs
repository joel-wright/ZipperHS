module Main (
        main
        ) where

import Zipper

main :: IO()
main = do
	tt <- modifyFocusTreeIO testFocusTree
        t <- return (fst tt)
        printTree t

testTree :: Tree Double
testTree = Node 20
            (Node 34
             (Node 9 
              (Node 8923654
               (Node 6
                (Node 4
                 (Node 98876223456.3463
                  Leaf Leaf)
                 Leaf)
                Leaf)
               Leaf)
              Leaf)
             Leaf)
            (Node 54
             Leaf Leaf)

testFocusTree :: FocusTree Double
testFocusTree = (testTree, [L,L,L,L,L,L])

modifyFocusTreeIO :: Fractional a => FocusTree a -> IO (FocusTree a)
modifyFocusTreeIO ft = return (modifyFocusTree' 10000000 ft)

modifyFocusTree' :: Fractional a => Int -> FocusTree a -> FocusTree a
modifyFocusTree' 0 ft = ft
modifyFocusTree' n ft = modifyFocusTree'' (n-1) (modifyFocus ft div3Node)

modifyFocusTree'' :: Fractional a => Int -> FocusTree a -> FocusTree a
modifyFocusTree'' 0 ft = ft
modifyFocusTree'' n ft = modifyFocusTree' (n-1) (modifyFocus ft doubleNode)

