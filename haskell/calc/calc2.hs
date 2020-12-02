--https://github.com/system-f/validation/blob/master/examples/src/Email.hs
-- http://hackage.haskell.org/package/either-5.0.1.1/docs/Data-Either-Validation.html
-- http://hackage.haskell.org/package/either-5.0.1.1/docs/Data-Either-Validation.html
-- cabal install either
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Text.Read
import Data.Either.Validation
import Data.Monoid
newtype NonZeroInt = NonZeroInt Integer deriving (Show)

data VError = MustBeNonZero
            | MustBePositive
            | InvalidNumber
            deriving (Show)

--Just ["er1","er2"] `mappend` Just ["er3"]

--isNumber :: String -> Validation [VError] Double
--isNumber s = if isJust v then Success (fromMaybe 0.0 v) else Failure [InvalidNumber] where v = readMaybe s :: Maybe Double

--nonZero s = ((/=)0) <$> (isNumber s)

fe_validNumber s = if isJust v then Nothing else Just [InvalidNumber] where v = readMaybe s :: Maybe Double
fe_non_zero s = if isNothing $ fe_validNumber s then if (0 == fromMaybe 0 (readMaybe s :: Maybe Double) ) then Just [MustBeNonZero] else Nothing
            else fe_validNumber s

find_errors s = (fe_non_zero s) <> (fe_validNumber s)


get_double = do v<-getLine;if isJust (find_errors v) then (putStrLn $ show (fromJust (find_errors v))) >> get_double else return (fromJust $ readMaybe v)


-- validate s = if isNothing $ find_errors s then Success (readMaybe s :: Maybe Double) else


binary_op name f = do
    putStrLn name;
    putStrLn "first n";
    v1<-get_double
    putStrLn "second n";
    v2<-get_double
    --x<-getLine;
    --y<-getLine;
    --let v2 = readMaybe y :: Maybe Double;
    putStrLn $ show (f v1 v2)


plus = binary_op "plus" (+)
minus = binary_op "minus" (-)
mul = binary_op "mul" (*)
div_op = binary_op "div" (/)
acts = M.fromList [("plus",plus),("minus",minus),("mul",mul),("div",div_op)]

m = do
       putStrLn "Input command"
       putStrLn $ intercalate " | " (M.keys acts)
       x<-getLine;
       (fromMaybe (putStrLn "404") (M.lookup x acts))

main = m
