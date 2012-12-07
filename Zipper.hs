-- The Zipper
--
-- Something to think about if your data is more than just a data structure,
-- but a data structure AND a position...

module Zipper (
    Tree,
    Direction,
    ZipTree,

    mkZipTree,
    left,
    right,
    up,
    top,
    modify,
    delete,
) where

-- ###########################################################################
--
-- Start by making a simple Tree type
--
-- ###########################################################################

data Tree a = Leaf
            | Node a (Tree a) (Tree a)
              deriving (Show,Eq)

-- ###########################################################################
--
-- If we want to modify specific locations of the Tree then we need directions 
--
-- ###########################################################################

data Direction = L | R deriving (Show,Eq)

type Directions = [Direction]

-- ###########################################################################
--
-- Now we make a ZipTree by separating the tree into a context
--
-- ###########################################################################

data Context a = CRoot
               | CLeft (Context a) (a,(Tree a))
               | CRight (Context a) (a,(Tree a))
                 deriving (Show,Eq)

type ZipTree a = (Context a, Tree a)

mkZipTree :: Tree a -> ZipTree a
mkZipTree t = (CRoot, t)

left :: ZipTree a -> ZipTree a
left (c, Node x l r) = (CLeft c (x,r), l)

right :: ZipTree a -> ZipTree a
right (c, Node x l r) = (CRight c (x,l), r)

up :: ZipTree a -> ZipTree a
up (CLeft c (x,r), t) = (c, Node x t r)
up (CRight c (x,l), t) = (c, Node x l t)

top :: ZipTree a -> ZipTree a
top z@(CRoot, t) = z
top z = top.up $ z

modify :: ZipTree a -> (Tree a -> Tree a) -> ZipTree a
modify (c, t) f = (c, f t)

-- ###########################################################################
--
-- Some functions using the ones we've defined
--
-- ###########################################################################

delete :: ZipTree a -> ZipTree a
delete z = modify z (\t -> Leaf)

