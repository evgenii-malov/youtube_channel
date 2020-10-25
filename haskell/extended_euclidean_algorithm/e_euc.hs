-- explain why and how extended euclidean algorithm works https://www.youtube.com/watch?v=00osXA62DuI

-- simple 

gcd a b = hgcd a b 1 0 0 1 where hgcd a b x0 y0 x1 y1 = if a `mod` b == 0 then (b,x1,y1) else hgcd b (a `mod` b) x1 y1 (x0-q*x1) (y0-q*y1) where q = a `div` b

-- with trace

(!?) = (!!) . reverse
mgcd a b = hgcd a b [[1,0,a],[0,1,b]] where hgcd a b l = if a `mod` b == 0 then l else hgcd b (a `mod` b) (l ++ [[(x0-q*x1),(y0-q*y1),a `mod` b]]) where q = a `div` b; x0= (l !? 1) !! 0; y0 = (l !? 1) !! 1; x1= (l !? 0) !! 0;y1 = (l !? 0) !! 1;
sh a b r = (\e -> show (e !! 0) ++ "*" ++ show a ++ "+" ++ show (e !! 1) ++ "*" ++ show b ++ "=" ++  show (e !! 2) ) <$> r
mgcd2 a b = sh a b (mgcd a b)
mgcd2 103 22

-- monadic trace

gcd a b = hgcd a b 1 0 0 1 where hgcd a b x0 y0 x1 y1 = if a `mod` b == 0 then d_act k >> return k else d_act k >> hgcd b (a `mod` b) x1 y1 (x0-q*x1) (y0-q*y1) where q = a `div` b; k = (b,x1,y1);d_act = putStrLn.show






