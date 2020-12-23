import Network.HTTP.Conduit
import Text.JSON.JPath
import Data.ByteString.Lazy.Char8 (unpack)
import Control.Concurrent
import Control.Exception
-- import Data.Functor.Compose
import Control.Applicative

u = "https://api.bittrex.com/api/v1.1/public/getticker?market=USD-BTC"

print_or_not pp p = if pp /= p then print p else return ()

parsed d = head $ (jPath "result/Ask" d) <|> ["parse_price_error"]


act pp = (unpack <$> (simpleHttp u))
      >>= (\d -> print_or_not pp (parsed d)
                 >> threadDelay 1000000
                 >> act (parsed d)
      )

onerr :: String -> HttpException -> IO ()
onerr pp e = (print $ "error: " ++ show e) >> threadDelay 1000000 >> go pp

go pp = catch (act pp) (onerr pp)
