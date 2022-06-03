

module Lib
    ( startPeer
    ) where

import Network.Simple.TCP
import RIO 
import qualified Data.ByteString as BS
import qualified Data.String as BSI


startPeer :: Int -> Int -> Int -> IO ()
startPeer remotePort localPort delay =   concurrently_ (serveFunc $ show localPort) $ do 
                    threadDelay delay
                    clientFunc $ show remotePort

serveFunc :: String -> IO ()
serveFunc port = serve (Host "127.0.0.1") port $ \(connectionSocket, remoteAddr) -> do
  putStrLn $ "TCP connection established from " ++ show remoteAddr
  send connectionSocket $ BSI.fromString "test"
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.


clientFunc :: String -> IO ()
clientFunc port = connect "127.0.0.1" port $ \(connectionSocket, remoteAddr) -> do
  putStrLn $ "Connection established to " ++ show remoteAddr
  recvd <- recv connectionSocket 4
  print recvd
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.