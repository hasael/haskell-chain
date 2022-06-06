
module Lib
    ( startPeer
    ) where

import Network.Simple.TCP
import RIO 
import Models
import Data.Aeson 
import Data.ByteString.Lazy (toStrict)
import AppState 
import RIO.State (StateT(runStateT))
import RIO.Time (hoursToTimeZone)


startPeer :: Int -> [Peer] -> Int -> IO ()
startPeer localPort peers delay =   concurrently_ (runReaderT (serveFunc localPort) $ AppState peers localPort) $ do 
                    threadDelay delay
                    testFunc $ head peers

serveFunc :: Int -> AppHandler ()
serveFunc port = do 
   appState <- ask
   serve (Host "127.0.0.1") (show port) $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ "TCP connection established from " ++ show remoteAddr
  -- let block = Block "hash" "merkle" "timestamp" 10 ["trx a","trx b"]
  -- send connectionSocket $ toStrict $ encode block
        recvd <- recv connectionSocket 4000
        case recvd of 
            Just val ->  case readMsg val of
                     Right msg -> putStrLn ("Received " ++ show msg) >> runReaderT  (handleMessage msg (findPeer appState (ipFromSocketAddress remoteAddr) port) ) appState
                     Left e -> return ()
            _ -> print "no value received"
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.


testFunc :: Peer -> IO ()
testFunc peer =  sendMessage peer $ Message RequestPeers "timeStamp"  RequestPeersData 

handleMessage :: Message -> Peer -> AppHandler ()
handleMessage msg peer = case msgData msg of
  RequestPeersData -> do
               appState <- ask
               liftIO $ sendMessage peer $ Message NewPeer "timeStamp"  $ NewPeerData $ getStrPeers appState 
  NewBlockData block -> liftIO $ print $ "Received " ++ show msg
  NewPeerData peers -> return ()

  
readBlock :: ByteString -> Either String Block
readBlock =  eitherDecodeStrict 

readMsg :: ByteString -> Either String Message
readMsg =  eitherDecodeStrict 

sendMessage :: Peer -> Message -> IO ()
sendMessage peer msg = do
     putStrLn $ "Sending message" ++ show msg ++ " to " ++ show peer 
     connect (getIpAddr $ ipAddress peer) (show $ getPort $ peerPort peer) $ \(connectionSocket, remoteAddr) -> do
       send connectionSocket $ toStrict $ encode msg
       closeSock connectionSocket

ipFromSocketAddress :: SockAddr -> String
ipFromSocketAddress sockAddr =  head $ wordsWhen (==':') $ show sockAddr


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.