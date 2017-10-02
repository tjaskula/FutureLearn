
lng::[a]-> Int
lng [] = 0
lng (x:xs) = 1 + lng xs

let flt::(a->Bool)->[a]->[a]
flt _ [] = []
flt p (x:xs) = if p x then x:(flt p xs) else flt p xs

let flt::(a->Bool)->[a]->[a]
flt _ [] = []
flt p (x:xs) = if p x then x:xs' else xs' where xs' =  flt p xs

let fl::(b->a->b)->b->[a]->b
fl _ acc [] = acc
fl f acc (x:xs) = fl f (f acc x) xs

let fr::(a->b->b)->b->[a]->b
fr f x' xs' = rgo x' xs'
where
   rgo z [] = z
   rgo z (x:xs) = f x (rgo z xs)

   fr (\elt acc -> acc ++ [elt]) "" "Reversing a string" --"gnirts a gnisreveR"
   fr (\elt acc -> elt:acc) "" "Reversing a string" --"Reversing a string"


let treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) =
   1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

let treeSum :: Tree -> Int
     treeSum Leaf = 0
     treeSum (Node value leftSubtree rightSubtree) =
       value + (treeSum leftSubtree) + (treeSum rightSubtree)

let isSortedTree :: Tree -> Int -> Int -> Bool
     isSortedTree Leaf _ _ = True
     isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
       let leftSorted   = isSortedTree leftSubtree minVal x
           rightSorted = isSortedTree rightSubtree x maxVal
       in x >= minVal && x< maxVal && leftSorted && rightSorted

let addNewMax :: Tree -> Tree
     addNewMax Leaf = Node 0 Leaf Leaf
     addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)
     addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2)

-- test submission
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

treezip :: (Tree a) -> (Tree b) -> (Tree (a,b))
treezip _ Leaf = Leaf
treezip Leaf _ = Leaf
treezip (Node x ls1 rs1) (Node y ls2 rs2) = Node (x, y) (treezip ls1 ls2) (treezip rs1 rs2)

treeunzip :: (Tree (a,b)) -> (Tree a, Tree b)
treeunzip Leaf = (Leaf, Leaf)
treeunzip (Node (a, b) l r) = ((Node a l' r'),
                               (Node b l'' r''))
   where (l', l'') = treeunzip l
         (r', r'') = treeunzip r