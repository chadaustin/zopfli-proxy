module Data.Zopfli
    ( zopfli
    , zopfliLBS
    ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Data.ByteString.Unsafe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

foreign import ccall "zopfli_compress" zopfli_compress
    :: CSize -> CString -> Ptr CSize -> IO CString

zopfli :: BS.ByteString -> BS.ByteString
zopfli input = unsafePerformIO $ do
    alloca $ \output_length_ptr ->
        unsafeUseAsCStringLen input $ \(input_str, input_length) -> do
            output_str <- zopfli_compress (toEnum input_length) input_str output_length_ptr
            output_length <- peek output_length_ptr
            unsafePackMallocCStringLen (output_str, fromEnum output_length)

zopfliLBS :: BSL.ByteString -> BSL.ByteString
zopfliLBS = BSL.fromStrict . zopfli . BSL.toStrict

