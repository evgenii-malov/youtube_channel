import qualified Data.Set as Set(fromList,difference)
kr n l = (*n) <$> [2..l `div` n]
g n = difference (fromList [2..n]) (fromList $ concat $ ((flip kr) n) <$> [2..n])

