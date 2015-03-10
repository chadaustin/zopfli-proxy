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
                deriving (Show, Eq, Ord)
type Cache = Map.Map CacheKey Wai.Response

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

cacheKeyFromRequestAndResponse :: Wai.Request -> HC.Response BSL.ByteString -> CacheKey
cacheKeyFromRequestAndResponse = error "hi"

findCacheHit :: Cache -> Wai.Request -> Maybe (Wai.Response)
findCacheHit = error "hi"

app :: IORef Cache -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
app cache request respond = do
    let method = Wai.requestMethod request
    if (method == HT.methodGet || method == HT.methodHead) then do
        c <- readIORef cache
        fr <- case findCacheHit c request of
            Just r -> return r
            Nothing -> do
                response <- makeBackendRequest request

                let responseHeaders = HC.responseHeaders response
                -- hacky hack, should support gzip decoding
                let hasContentType = any ((== "Content-Encoding") . fst) responseHeaders

                let cr = if hasContentType then
                             convertResponse response
                         else
                             -- only compress if the backend is uncompressed
                             zopfliResponse response

                atomicModifyIORef cache $ \m ->
                    (Map.insert (cacheKeyFromRequestAndResponse request response) cr m, ())
                return (cr :: Wai.Response)

        respond (fr :: Wai.Response)
    else do
        -- TODO: implement lightweight passthru
        response <- makeBackendRequest request
        respond $ convertResponse response
    
main :: IO ()
main = do
    cache <- newIORef Map.empty
    Warp.run 3000 $ app cache
