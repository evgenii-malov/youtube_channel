bup (_,l) = f (False,l) where
f (px,[]) = (px,[])
f (px,[a]) = (px,[a])
f (px, (a:b:t)) = if a<=b then (fst c1, a:(snd c1)) else (True, b:(snd $ f (True,(a:t)) ))
    where c1 = f (px,(b:t))

--start = (False,[4,3,2,1])
bsort l = snd $ last $ takeWhile fst (iterate bup (True,l))
