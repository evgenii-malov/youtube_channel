merge [] l2 = l2
merge l1 [] = l1
merge (x:xs) (y:ys) = if x>=y then y:(merge (x:xs) ys) else x:(merge xs (y:ys))
msort l = if length l>1 then merge (msort p1) (msort p2) else l where (p1,p2) = (splitAt.(`div` 2).length) l l
