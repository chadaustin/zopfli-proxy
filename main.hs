{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HT
import qualified Network.Wai.Handler.Warp as Warp

foreign import 

app _request = do
    let resp = "PONG"
    return $ Wai.responseLBS HT.status200 [] resp

main :: IO ()
main = do
    Warp.run 3000 app

    return ()
