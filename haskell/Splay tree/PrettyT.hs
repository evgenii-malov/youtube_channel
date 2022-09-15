-- see video https://youtu.be/Ud-1Z0hBlB8
-- pretty binary tree

module PrettyT where
import Data.List

data Btree a = Empty | Node a (Btree a) (Btree a) deriving Show

-- data Btree a = Leaf a | Node a (Btree a) (Btree a) deriving Show
tr_l = Node "b" (Node "d" Empty Empty) (Node "e" Empty Empty)
tr_r = Node "c" (Node "f" Empty Empty) (Node "g" Empty Empty)
tr = Node "a" tr_l tr_r :: Btree String
-- duplicate subtrees
td = Node "a" tr_l tr_l :: Btree String



tr_l1 = Node "h" (Node "i" Empty Empty) (Node "j" Empty Empty)
tr_r1 = Node "k" (Node "l" Empty Empty) (Node "m" Empty Empty)
tr1 = Node "n" tr_l1 tr_r1 :: Btree String

tx = Node "x" tr tr1

trr = Node "1" (Node "2" (Node "3" (Node "4" Empty (Node "5" Empty tx)) Empty) Empty) Empty:: Btree String

data ParentDir = PLeft | PRight | NoParent deriving (Show,Eq)
type ParentPos = Int
type Level = Int

dline = '|'
factor = 4

m c1 c2 = if c1 == dline then c1 else c2
zipWith' f xs [] = xs
zipWith' f [] xs = xs
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

build_line pd a pp level = foldl (zipWith' m) "" (((++"|").(flip replicate ' ') <$> (factor*) <$> pp)++[(replicate (factor*level) ' ')++cn++show a])
                           where cn = case pd of PLeft -> "└──"
                                                 PRight -> "┌──"
                                                 NoParent -> "───"

tprint :: Show a => ParentDir -> [ParentPos] -> Level -> Btree a -> [String]
tprint _ _ _ Empty = []
tprint pd pp level (Node a l r) = tprint PRight new_pp_r (level+1) r ++
                                  [build_line pd a pp level] ++
                                  tprint PLeft new_pp_l (level+1) l
                                  where new_pp_r = case pd of PRight -> pp
                                                              PLeft -> pp++[level]
                                                              NoParent -> pp
                                        new_pp_l = case pd of PRight -> pp++[level]
                                                              PLeft -> pp
                                                              NoParent -> pp

printt t = putStr $ (intercalate "\r\n" (tprint NoParent [] 0 t))++"\r\n"

-- ───"a"
--     └──"b"
--         └──"c"
--             |                   ┌──"g"
--             |               ┌──"c"
--             |               |   └──"f"
--             |           ┌──"a"
--             |           |   |   ┌──"e"
--             |           |   └──"b"
--             |           |       └──"d"
--             |       ┌──"x"
--             |       |   |       ┌──"g"
--             |       |   |   ┌──"c"
--             |       |   |   |   └──"f"
--             |       |   └──"a"
--             |       |       |   ┌──"e"
--             |       |       └──"b"
--             |       |           └──"d"
--             |   ┌──"f"
--             └──"d"