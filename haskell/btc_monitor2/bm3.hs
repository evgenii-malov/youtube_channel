import Network.HTTP.Conduit
import Text.JSON.JPath
import Data.ByteString.Lazy.Char8 (unpack)
import Control.Concurrent
import Control.Exception
import Control.Applicative
import Control.Monad.Loops

u = "https://api.bittrex.com/api/v1.1/public/getticker?market=USD-BTC"
parsed d = head $ (jPath "result/Ask" d) <|> ["parse_price_error"]
act = try $ (simpleHttp u) >>= (\d -> threadDelay 1000000 >> return (parsed $ unpack d)) :: IO (Either HttpException [Char])
pe pp (Right p) = if pp==p then return p else print p >> return p
pe pp (Left e) = print er >> threadDelay 1000000 >> return er where er = show e
main = iterateUntilM (\_->False) (\pp -> act >>= (pe pp)) "start"