module Main (
        main
        ) where

import Zipper

main :: IO()
main = do
	tt <- modifyTestTreeIO
        t <- return (snd $ top tt)
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

testZipTree :: ZipTree Double
testZipTree = (left.left.left.left.left.left) $ mkZipTree testTree

modifyTestTreeIO :: IO (ZipTree Double)
modifyTestTreeIO = return (top $ modifyTestTree 100000 testZipTree)

modifyTestTree :: Fractional a => Int -> ZipTree a -> ZipTree a
modifyTestTree 0 z = z
modifyTestTree n z
	| (n `mod` 2) == 0 = modifyTestTree (n-1) (div3ZipNode z)
        | otherwise        = modifyTestTree (n-1) (doubleZipNode z)

