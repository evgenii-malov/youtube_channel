import Data.List

p :: Eq a => [a] -> [[a]]
p [] = [[]]
p xs = xs >>= (\x -> (x:) <$> p (delete x xs) )