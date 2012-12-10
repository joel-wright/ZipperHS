module Main (
        main
        ) where

import Zipper

main :: IO()
main = do
	tt <- modifyTestTreeIO testZipTree
        printTree (snd $ top tt)

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

modifyTestTreeIO :: Fractional a => ZipTree a -> IO (ZipTree a)
modifyTestTreeIO zt = return (top $ modifyTestTree' 10000000 zt)

modifyTestTree' :: Fractional a => Int -> ZipTree a -> ZipTree a
modifyTestTree' 0 z = z
modifyTestTree' n z = modifyTestTree'' (n-1) (div3ZipNode z)

modifyTestTree'' :: Fractional a => Int -> ZipTree a -> ZipTree a
modifyTestTree'' 0 z = z
modifyTestTree'' n z = modifyTestTree' (n-1) (doubleZipNode z)

