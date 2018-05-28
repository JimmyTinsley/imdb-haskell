module DownloadHTML where

import Network.URI
import Network.HTTP.Conduit
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Lazy as B
-- | This function is used to download the .html file from the IMDB TOP 250 website
downloadHTML :: String -> String -> IO()
downloadHTML filename source = do
    htmlcode <- simpleHttp source
    B.writeFile filename htmlcode
