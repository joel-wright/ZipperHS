module Main (
        main
        ) where

import Zipper

main :: IO()
main = do
	tt <- modifyFocusTreeIO
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

modifyFocusTreeIO :: IO (FocusTree Double)
modifyFocusTreeIO = return (modifyFocusTree 100000 testFocusTree)

modifyFocusTree :: Fractional a => Int -> FocusTree a -> FocusTree a
modifyFocusTree 0 f = f
modifyFocusTree n f
	| (n `mod` 2) == 0 = modifyFocusTree (n-1) (modifyFocus f div3Node)
        | otherwise        = modifyFocusTree (n-1) (modifyFocus f doubleNode)

