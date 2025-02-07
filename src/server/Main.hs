{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LB
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.IO

main :: IO ()
main = do
  mapM_ (`hSetBuffering` NoBuffering) [stdin, stdout]
  hSetBuffering stderr LineBuffering

  forever $ do
    sendBS "\"Hello, from Haskell!\""
    threadDelay 1000_000
    hPutStrLn stderr "Hello, from Haskell!"
    threadDelay 1000_000

-- | send json-rpc message on stdout
sendBS :: LB.ByteString -> IO ()
sendBS = BS.putStr . BS.toStrict . toRpc
 where
  toRpc bs =
    LB.concat
      [ "Content-Length: " <> showLB (LB.length bs)
      , "\r\n\r\n"
      , bs
      ]

  showLB = LB.fromStrict . Text.encodeUtf8 . Text.pack . show
