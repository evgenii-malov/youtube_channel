{-# LANGUAGE InstanceSigs #-}
import Control.Monad
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

--class MyMonad m where
--    (>>==)       ::  m a -> (a -> m b) -> m b
--    (>>)        ::  m a -> m b -> m b
--    m >> k = m >>== \_ -> k
--    return      :: a -> m a

-- https://stackoverflow.com/questions/19635265/is-it-better-to-define-functor-in-terms-of-applicative-in-terms-of-monad-or-vic


instance Applicative (Cont r) where
  pure = return
  (<*>) = ap

instance Functor (Cont r) where
  fmap = liftM

instance Monad (Cont r) where
  return :: a -> Cont r a
  -- a -> ((a -> r) -> r)
  return x = Cont $ \c -> c x

  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  -- ((a -> r) -> r) -> ( a -> ((b -> r) -> r)) -> ((b -> r) -> r)
  (>>=) (Cont v) k = Cont $ \c -> v (\a -> (runCont $ k a) c)

main = putStrLn $ runCont sumIt id

--sumIt :: Cont [r] String
--sumIt = do
--  -- a <- return 3
--  a1 <- Cont $ \c -> c "1" ++ c "2"
--  a2 <- Cont $ \c -> c "3" ++ c "4"
--  a3 <- Cont $ \c -> c "5" ++ c "6" ++ c "7"
--  return $ a1 ++ a2 ++ a3


sumIt :: Cont [r] String
sumIt = do
  -- a <- return 3
  a1 <- Cont $ \c -> c "1" ++ c "2"
  return $ a1 ++ "3"

-- (>>=) (Cont v) k = Cont $ \x -> v (\a -> (runCont $ k a) x)
-- Cont $ \c -> c "1" ++ c "2" >>= (\a1 -> return $ a1 ++ "3") =
-- Cont $ \x -> (\c -> c "1" ++ c "2") (\a -> (runCont $ k a) x)
-- Cont $ \x -> (runCont $ (return $ "1" ++ "3")) x)  ++ (runCont $ (return $ "2" ++ "3")) x)
-- Cont $ \x -> (runCont $ (return $ "13")) x)  ++ (runCont $ (return $ "23")) x)
-- Cont $ \x -> (runCont $ (Cont $ \c -> c "13")) x)  ++ (runCont $ (Cont $ \c -> c "23")) x)
-- Cont $ \x -> ( \c -> c "13" x)  ++ ( \c -> c "23" x)
-- runCont $ Cont $ \x -> ( \c -> c "13" x)  ++ ( \c -> c "23" x) id
--  id "13"  ++  id "23"
--  "13" ++ "23"
--  "1323"
