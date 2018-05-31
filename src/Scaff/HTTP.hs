module Scaff.HTTP
where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import Network.HTTP.Types.Status
import System.IO
import Text.Printf

mkHttpClient :: IO Manager
mkHttpClient = newTlsManager

readFileHttp :: Manager -> String -> IO (Maybe String)
readFileHttp client url = do
  rq <- parseRequest url
  rp <- httpLbs rq client
  case statusCode . responseStatus $ rp of
    200 ->
      return . Just . LUTF8.toString $ responseBody rp
    c -> do
      hPutStrLn stderr $
        printf "Error: GET %s - HTTP %i"
          url c
      return Nothing

