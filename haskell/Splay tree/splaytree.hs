-- BST tree
-- self-balancing binary search trees
-- all ops O(log n) amortized

-- https://en.wikipedia.org/wiki/Splay_tree

import qualified PrettyT as P

data Btree a = Empty | Node (Btree a ) a (Btree a ) deriving Show
data Pd = L | R | N deriving Show

l_tree = Node (Node Empty 5 Empty) 10 (Node Empty 20 Empty) 
r_tree = Node (Node Empty 50 Empty) 100 (Node Empty 200 Empty) 


tree = Node l_tree 40 r_tree

path :: Ord a => Btree a -> a -> [(Btree a, Pd)]
path Empty _ = error "No element in a tree"
path n fe = go n N []
      where go n@(Node l e r) d path
                | fe < e = go l L ((n,d):path)
                | fe > e = go r R ((n,d):path)
                | otherwise = (n,d):path -- fe == e

splay :: Ord a => Btree a -> a -> Btree a
splay t e = go $ path t e
    where 
          -- zig 
          go [(n,_)] = n
          go (((Node l ce r),L):((Node _ pe pr),N):[]) = Node l ce (Node r pe pr) 
          go (((Node l ce r),R):((Node pl pe _),N):[]) = Node (Node pl pe l) ce r
          -- zig zig 
          go (((Node l ce r),L):((Node _ pe pr),L):((Node _ ge gr),d):ls) = go $ ((Node l ce (Node r pe (Node pr ge gr))),d):ls  
          go (((Node l ce r),R):((Node pl pe _),R):((Node gl ge _),d):ls) = go $ ((Node (Node (Node gl ge pl) pe l) ce r),d):ls            
          -- zig zag 
          go (((Node l ce r),R):((Node pl pe _),L):((Node _ ge gr),d):ls) = go $ ((Node (Node pl pe l) ce (Node r ge gr)),d):ls    
          go (((Node l ce r),L):((Node _ pe pr),R):((Node gl ge _),d):ls) = go $ ((Node (Node gl ge l) ce (Node r pe pr)),d):ls     


convert :: Btree a -> P.Btree a
convert Empty = P.Empty
convert (Node l e r) = P.Node e (convert l) (convert r)

-- P.printt $ convert $ splay (splay (splay tree 5) 200) 100
-- main = P.printt $ convert $ splay (splay (splay tree 5) 200) 100
main = P.printt $ convert $ foldl splay tree [5,200,100]