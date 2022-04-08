{-# LANGUAGE FunctionalDependencies #-}

-- :set -XFlexibleContexts

data Boolean = T | F deriving Show
data Bit = One | Zero deriving Show

class Logical a b c | a b -> c where
  (|||) :: a -> b -> c  -- logical or

instance Logical Boolean Boolean Boolean where
  (|||) T _ = T
  (|||) _ T = T
  (|||) _ _ = F

instance Logical Boolean Bit Boolean where
  (|||) T _ = T
  (|||) _ One = T
  (|||) _ _ = F

instance Logical Bit Boolean Boolean where
  (|||) One _ = T
  (|||) _ T = T
  (|||) _ _ = F

instance Logical Bit Bit Bit where
  (|||) One _ = One
  (|||) _ One = One
  (|||) _ _ = Zero








