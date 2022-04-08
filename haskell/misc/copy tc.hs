 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE FunctionalDependencies #-}
 -- {-# LANGUAGE FlexibleContexts #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
 -- {-# FlexibleContexts #-}

data Boolean = T | F deriving Show
data Bit = One | Zero deriving Show

class Logical a where
  (|||) :: a -> a -> a  -- logical or

class Logical2 a b c where
  (|||) :: a -> b -> c  -- logical or

instance Logical2 Bit Boolean Boolean where
  (|||) One _ = T
  (|||) _ T = T
  (|||) _ _ = F

instance Logical Bit  where
  (|||) One _ = One
  (|||) _ One = One
  (|||) _ _ = Zero

instance Logical Boolean  where
  (|||) T _ = T
  (|||) _ T = T
  (|||) _ _ = F


-- main = print $ (T or' T)

class Logical a b c | a b -> c where
  (|||) :: a -> b -> c  -- logical or

instance Logical Boolean Boolean Boolean where
  (|||) T _ = T
  (|||) _ T = T
  (|||) _ _ = F

instance Logical Bit Bit Boolean where
  (|||) One _ = T
  (|||) _ One = T
  (|||) _ _ = F

instance Logical Bit Boolean Boolean where
  (|||) One _ = T
  (|||) _ T = T
  (|||) _ _ = F

instance Logical Boolean Bit Boolean where
  (|||) T _ = T
  (|||) _ One = T
  (|||) _ _ = F



-- main = print $ (T or' T)

