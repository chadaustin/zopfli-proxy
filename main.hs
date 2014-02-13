{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Data.Monoid
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Data.ByteString.Unsafe
import Data.ByteString.Internal (c_free_finalizer, ByteString(PS))

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Network.Wai as Wai
import qualified Network.HTTP.Base as HB
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT
import qualified Network.Wai.Handler.Warp as Warp

foreign import ccall "zopfli_compress" zopfli_compress
  :: CSize -> CString -> Ptr CSize -> IO CString

-- copied from https://github.com/ghc/packages-bytestring/blob/bytestring-0.10.4.0-release/Data/ByteString/Unsafe.hs#L264
unsafePackMallocCStringLen :: CStringLen -> IO BS.ByteString
unsafePackMallocCStringLen (cstr, len) = do
    fp <- newForeignPtr c_free_finalizer (castPtr cstr)
    return $! PS fp 0 len

zopfli :: BS.ByteString -> BS.ByteString
zopfli input = unsafePerformIO $ do
  alloca $ \output_length_ptr ->
    unsafeUseAsCStringLen input $ \(input_str, input_length) -> do
      output_str <- zopfli_compress (toEnum input_length) input_str output_length_ptr
      output_length <- peek output_length_ptr
      unsafePackMallocCStringLen (output_str, fromEnum output_length)

zopfliLazy :: BSL.ByteString -> BSL.ByteString
zopfliLazy = BSL.fromStrict . zopfli . BSL.toStrict

zopfliResponse :: HC.Response BSL.ByteString -> HC.Response BSL.ByteString
zopfliResponse response =
  response { HC.responseBody = zopfliLazy $ HC.responseBody response }

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
    
  let responseHeaders = HC.responseHeaders response
  let hasContentType = any ((== "Content-Encoding") . fst) responseHeaders
  return $ convertResponse $ if hasContentType
    then response
    else zopfliResponse response

main :: IO ()
main = do
  Warp.run 3000 $ app
