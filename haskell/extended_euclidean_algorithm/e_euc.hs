
gcd a b = hgcd a b 1 0 0 1 where hgcd a b x0 y0 x1 y1 = if a `mod` b == 0 then (b,x1,y1) else hgcd b (a `mod` b) x1 y1 (x0-q*x1) (y0-q*y1) where q = a `div` b

(!?) = (!!) . reverse
Prelude> mgcd a b = hgcd a b [[1,0,a],[0,1,b]] where hgcd a b l = if a `mod` b == 0 then l else hgcd b (a `mod` b) (l ++ [[(x0-q*x1),(y0-q*y1),a `mod` b]]) where q = a `div` b; x0= (l !? 1) !! 0; y0 = (l !? 1) !! 1; x1= (l !? 0) !! 0;y1 = (l !? 0) !! 1;
Prelude> sh 103 22 (mgcd 103 22)

gcd a b = hgcd a b 1 0 0 1 where hgcd a b x0 y0 x1 y1 = if a `mod` b == 0 then d_act k >> return k else d_act k >> hgcd b (a `mod` b) x1 y1 (x0-q*x1) (y0-q*y1) where q = a `div` b; k = (b,x1,y1);d_act = putStrLn.show

sh a b r = (\e -> show (e !! 0) ++ "*" ++ show a ++ "+" ++ show (e !! 1) ++ "*" ++ show b ++ "=" ++  show (e !! 2) ) <$> r

mgcd2 a b = sh a b (mgcd a b)


-----------------------


gcd a b p = if a `mod` b == 0 then (b, snd p) else gcd b (a `mod` b) (snd p, (xj2-q*xj1, yj2-q*yj1)) where xj2= fst $ fst p; xj1 = fst $ snd p; yj2 = snd $ fst p; yj1 = snd $ snd p; q = fst $ quotRem a b


(!?) = (!!) . reverse
mgcd a b p = if a `mod` b == 0 then p else mgcd b (a `mod` b) (p++[(xj2-q*xj1, yj2-q*yj1)]) where xj2= fst $ p !? 1; xj1 = fst $ p !? 0; yj2 = snd $ p !? 1; yj1 = snd $ p !? 1; q = a `div` b

gcd 103 22 ((1,0),(0,1))

mgcd a b p = if a `mod` b == 0 then p else mgcd b (a `mod` b) (p++[[xj2-q*xj1, yj2-q*yj1,q,xj1,xj2,yj1,yj2]]) where xj2= (!! 0) $ p !? 1; xj1 = (!! 0) $ p !? 0; yj2 = (!! 1) $ p !? 1; yj1 = (!! 1) $ p !? 0; q = a `div` b
mgcd 103 22 [[1,0,0,0,0,0,0],[0,1,0,0,0,0,0]]


mgcd 103 22 [(1,0),(0,1)]

gcd 33 22 ((0,1),(1,0))
gcd 103 22 ((0,1),(1,0))




gcd f a b p = if a `mod` b == 0 then (b, snd pp) else gcd False b (a `mod` b) pp where xj2= fst $ fst p; xj1 = snd $ fst p; yj2 = fst $ snd p; yj1 = snd $ snd p; q = fst $ quotRem a b; pp = if not f then (snd p, (xj2-q*xj1, yj2-q*yj1)) else p
 gcd True 103 11 ((0,1),(1,0))


eGCD 0 b = (b, 0, 1)
eGCD a b = let (g, s, t) = eGCD (b `mod` a) a
           in (g, t - (b `div` a) * s, s)
