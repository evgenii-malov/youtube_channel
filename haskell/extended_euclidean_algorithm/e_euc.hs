gcd a b p = if a `mod` b == 0 then (b, snd p) else gcd b (a `mod` b) (snd p, (xj2-q*xj1, yj2-q*yj1)) where xj2= fst $ fst p; xj1 = snd $ fst p; yj2 = fst $ snd p; yj1 = snd $ snd p; q = fst $ quotRem a b

gcd 33 22 ((0,1),(1,0))
