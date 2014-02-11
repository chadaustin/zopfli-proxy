{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Network.Wai as Wai
import Network.HTTP
import Network.HTTP.Base
import qualified Network.HTTP.Types as HT
import qualified Network.Wai.Handler.Warp as Warp

foreign import ccall "zopfli_compress" zopfli_compress
  :: CSize -> CString -> Ptr CSize -> CString

app config _request = do
  backend <- simpleHTTP (getRequest "http://localhost:4000")
  
  resp <- alloca $ \output_size -> return $ BSL.pack $ show $ zopfli_compress (CSize 0) nullPtr output_size
  return $ Wai.responseLBS HT.status200 [] resp

main :: IO ()
main = do
  let config = "http://localhost:4000"
  Warp.run 3000 $ app config

  return ()
