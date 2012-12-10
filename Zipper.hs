-- ###########################################################################
--
-- An introduction to The Zipper
--
-- Something to think about if your data is more than just a data structure,
-- but a data structure AND a "focus of interest"...
--
-- ###########################################################################

module Zipper (
    Tree(Node, Leaf),
    Direction(L, R),
    ZipTree,
    FocusTree,
    printTree,

    modifyFocus,

    mkZipTree,
    left,
    right,
    up,
    top,
    modify,

    delete,
    doubleZipNode,
    div3ZipNode,
    doubleNode,
    div3Node
) where

-- ###########################################################################
--
-- Start by making a simple Tree type
--
-- ###########################################################################

data Tree a = Leaf
            | Node a (Tree a) (Tree a)
              deriving (Show,Eq)

showTree :: (Show a) => Tree a -> String -> String
showTree Leaf s = s ++ "Leaf\n"
showTree (Node x l r) s = s ++ "Node " ++ (show x) ++ "\n"
                              ++ (showTree l (s ++ "  ")) 
                              ++ (showTree r (s ++ "  ")) 

printTree :: (Show a) => Tree a -> IO ()
printTree t = putStrLn (showTree t "")

-- ###########################################################################
--
-- If we want to modify specific locations of the Tree then we need directions 
-- and a way of keeping track of our "focus of interest"
--
-- ###########################################################################

data Direction = L | R deriving (Show,Eq)

type Directions = [Direction]

type FocusTree a = (Tree a, Directions)

modifyFocus :: FocusTree a -> (Tree a -> Tree a) -> FocusTree a
modifyFocus (t, []) f = (f t, [])
modifyFocus (Node x l r, L:ds) f = let l' = fst $ modifyFocus (l, ds) f
                                    in l' `seq` ((Node x l' r), L:ds)
modifyFocus (Node x l r, R:ds) f = let r' = fst $ modifyFocus (r, ds) f
                                    in r' `seq` ((Node x l r'), R:ds)
modifyFocus _ _ = error "Can't move down from a Leaf"

-- ###########################################################################
--
-- But each time we want to modify our focus we have to traverse the whole
-- Tree structure, make the change, and return a new Tree.
--
-- We'll try to improve this situation by separating the Tree into a context
-- and the sub-Tree that forms our current focus of interest. In this way we
-- always get O(1) access to the element of the Tree we're interested in :)
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
left _ = error "Can't move down from a Leaf"

right :: ZipTree a -> ZipTree a
right (c, Node x l r) = (CRight c (x,l), r)
right _ = error "Can't move down from a Leaf"

up :: ZipTree a -> ZipTree a
up (CLeft c (x,r), t) = (c, Node x t r)
up (CRight c (x,l), t) = (c, Node x l t)
up _ = error "Can't go up from the root of the Tree"

top :: ZipTree a -> ZipTree a
top z@(CRoot, t) = z
top z = top.up $ z

modify :: ZipTree a -> (Tree a -> Tree a) -> ZipTree a
modify (c, t) f = t' `seq` (c, t')
  where
    t' = f t

-- ###########################################################################
--
-- Some functions using the ones we've defined
--
-- ###########################################################################

delete :: ZipTree a -> ZipTree a
delete z = modify z (\t -> Leaf)

doubleNode :: Num a => Tree a -> Tree a
doubleNode (Node x l r) = x `seq` (Node (2*x) l r)
doubleNode _ = error "No value to double at a Leaf"

doubleZipNode :: Num a => ZipTree a -> ZipTree a
doubleZipNode z = modify z doubleNode

div3Node :: Fractional a => Tree a -> Tree a
div3Node (Node x l r) = x `seq` (Node (x/3) l r)
div3Node _ = error "No value to divide at a Leaf"

div3ZipNode :: Fractional a => ZipTree a -> ZipTree a
div3ZipNode z = modify z div3Node

