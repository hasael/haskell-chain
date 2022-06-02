module Lib
    ( serveFunc,
    clientFunc
    ) where

import Network.Simple.TCP

serveFunc :: String -> IO ()
serveFunc port = serve (Host "127.0.0.1") port $ \(connectionSocket, remoteAddr) -> do
  putStrLn $ "TCP connection established from " ++ show remoteAddr
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.


clientFunc :: String -> IO ()
clientFunc port = connect "127.0.0.1" port $ \(connectionSocket, remoteAddr) -> do
  putStrLn $ "Connection established to " ++ show remoteAddr
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.