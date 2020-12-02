import qualified Data.Map as M
import Data.Maybe

binary_op name f = do 
    putStrLn name;
    putStrLn "first n";
    x<-getLine;
    putStrLn "second n";
    y<-getLine;
    putStrLn $ show (f (read x) (read y))
    
m = do x<-getLine;(fromMaybe (putStrLn "404") (M.lookup x acts))

plus = binary_op "plus" (+)
minus = binary_op "minus" (-)
acts = M.fromList [("plus",plus),("minus",minus)]


main = m 
