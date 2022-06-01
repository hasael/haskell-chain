module Lib
    ( someFunc,
    clientFunc
    ) where

import Network.Simple.TCP

someFunc :: IO ()
someFunc = serve (Host "127.0.0.1") "3080" $ \(connectionSocket, remoteAddr) -> do
  putStrLn $ "TCP connection established from " ++ show remoteAddr
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.


clientFunc :: IO ()
clientFunc = connect "127.0.0.1" "3080" $ \(connectionSocket, remoteAddr) -> do
  putStrLn $ "Connection established to " ++ show remoteAddr
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.