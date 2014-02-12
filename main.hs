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
import Network.HTTP
import qualified Network.HTTP.Conduit as HC
import Network.HTTP.Base
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

foreign import ccall "zopfli_compress" zopfli_compress
  :: CSize -> CString -> Ptr CSize -> CString

app config request = do
  let method = Wai.requestMethod request
  let backendPath = Wai.rawPathInfo request
  backendRequest <- HC.parseUrl $ config <> (BS.unpack backendPath)
  putStrLn $ show backendRequest
  -- TODO: disallow non-GET-or-HEAD
  response <- HC.withManager $ \manager ->
    HC.httpLbs backendRequest manager
  --backend <- HC.httpLbs backendRequest manager -- simpleHTTP (getRequest "http://localhost:4000")
  
  resp <- alloca $ \output_size -> return $ BSL.pack $ show $ zopfli_compress (CSize 0) nullPtr output_size
  return $ Wai.responseLBS HT.status200 [] $ HC.responseBody response

main :: IO ()
main = do
  let config = "http://localhost:8000"
  Warp.run 3000 $ app config

  return ()
