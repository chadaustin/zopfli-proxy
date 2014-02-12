{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Data.Monoid
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Network.Wai as Wai
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT
import qualified Network.Wai.Handler.Warp as Warp

foreign import ccall "zopfli_compress" zopfli_compress
  :: CSize -> CString -> Ptr CSize -> CString

-- zopfli compression should be middleware
-- resp <- alloca $ \output_size -> return $ BSL.pack $ show $ zopfli_compress (CSize 0) nullPtr output_size

convertResponse response =
  Wai.responseLBS
    (HC.responseStatus response)
    (HC.responseHeaders response)
    (HC.responseBody response)

app request = do
  -- TODO: disallow non-GET-or-HEAD
  --let method = Wai.requestMethod request
  let backendPath = Wai.rawPathInfo request
  response <- HC.withManager $ \manager -> do
    requestp <- HC.parseUrl $ "http://localhost:8000" <> (BS.unpack backendPath)
    requestBody <- liftIO $ Wai.lazyRequestBody request
    let backendRequest = requestp {
          HC.method = Wai.requestMethod request,
          HC.requestHeaders = Wai.requestHeaders request,
          HC.requestBody = HC.RequestBodyLBS requestBody,
          HC.checkStatus = \_r _s _h -> Nothing }
                                        
    HC.httpLbs backendRequest {HC.checkStatus = \_r _s _h -> Nothing} manager
  --backend <- HC.httpLbs backendRequest manager -- simpleHTTP (getRequest "http://localhost:4000")
  
  return $ convertResponse response

main :: IO ()
main = do
  --config <- HC.parseUrl $ "http://localhost:8000"
  Warp.run 3000 $ app
