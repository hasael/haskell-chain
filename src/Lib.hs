

module Lib
    ( startPeer
    ) where

import Network.Simple.TCP
import RIO 
import Models
import Data.Aeson 
import Data.ByteString.Lazy (toStrict)


startPeer :: Int -> Int -> Int -> IO ()
startPeer remotePort localPort delay =   concurrently_ (serveFunc $ show localPort) $ do 
                    threadDelay delay
                    clientFunc $ show remotePort

serveFunc :: String -> IO ()
serveFunc port = serve (Host "127.0.0.1") port $ \(connectionSocket, remoteAddr) -> do
  putStrLn $ "TCP connection established from " ++ show remoteAddr
  let block = Block "hash" "merkle" "timestamp" 10 ["trx a","trx b"]
  send connectionSocket $ toStrict $ encode block
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.


clientFunc :: String -> IO ()
clientFunc port = connect "127.0.0.1" port $ \(connectionSocket, remoteAddr) -> do
  putStrLn $ "Connection established to " ++ show remoteAddr
  recvd <- recv connectionSocket 400
  case recvd of 
    Just val ->  case readBlock val of
                  Right a -> print a
                  Left e -> print e
    _ -> print "no value received"
 
readBlock :: ByteString -> Either String Block
readBlock =  eitherDecodeStrict 

  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.