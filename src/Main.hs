{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class
import Data.Monoid
import Data.IORef
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Network.Wai as Wai
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT
import qualified Data.Map as Map
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Zopfli as Zopfli

zopfliResponse :: HC.Response BSL.ByteString -> Wai.Response
zopfliResponse response = Wai.responseLBS
    (HC.responseStatus response)
    (newHeaders <> filter (headerNameIsNot "Content-Length") (HC.responseHeaders response))
    body
  where
    headerNameIsNot name = (/= name) . fst
    body = Zopfli.zopfliLBS $ HC.responseBody response
    newHeaders = [
        ("Content-Encoding", "gzip"),
        ("Content-Length", BSC.pack $ show $ BSL.length body)]

data CacheKey = CacheKey BS.ByteString HT.RequestHeaders
type Cache = Map.Map CacheKey BSL.ByteString

makeBackendRequest :: Wai.Request -> IO (HC.Response BSL.ByteString)
makeBackendRequest request = do
    let backendPath = Wai.rawPathInfo request
    HC.withManager $ \manager -> do
        requestp <- HC.parseUrl $ "http://localhost:8000" <> (BSC.unpack backendPath)
        requestBody <- liftIO $ Wai.lazyRequestBody request
        let backendRequest = requestp {
                HC.method = Wai.requestMethod request,
                HC.requestHeaders = Wai.requestHeaders request,
                HC.requestBody = HC.RequestBodyLBS requestBody,
                HC.checkStatus = \_r _s _h -> Nothing }
    
        HC.httpLbs backendRequest {HC.checkStatus = \_r _s _h -> Nothing} manager

convertResponse :: HC.Response BSL.ByteString -> Wai.Response
convertResponse response =
    Wai.responseLBS (HC.responseStatus response)
                    (HC.responseHeaders response)
                    (HC.responseBody response)

app :: IORef Cache -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
app cache request respond = do
    let method = Wai.requestMethod request
    if (method == HT.methodGet || method == HT.methodHead) then do
        -- check cache

        response <- makeBackendRequest request

        -- store into cache
        let responseHeaders = HC.responseHeaders response
        -- hacky hack, should check specifically for gzip
        let hasContentType = any ((== "Content-Encoding") . fst) responseHeaders

        if hasContentType then
            respond $ zopfliResponse response
        else
            respond $ convertResponse response
    else do
        response <- makeBackendRequest request
        respond $ convertResponse response
    
main :: IO ()
main = do
    cache <- newIORef Map.empty
    Warp.run 3000 $ app cache
