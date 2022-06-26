{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startPeer
    ) where

import Network.Simple.TCP
import RIO
import Models
import Data.Aeson
import Data.ByteString.Lazy  as BSL (toStrict, pack)
import Data.ByteString as BS (unpack)
import AppState
import RIO.Time (hoursToTimeZone)
import MessageHandler
import Block
import Messages
import Wallet
import Crypto.Random (MonadRandom)
import BlockChain
import Database.LevelDB.Higher
import Data.Maybe
import qualified Codec.Binary.UTF8.String as Utf8
import DbRepository

startPeer :: Int -> Int -> Int -> [Peer] -> String -> Int -> IO ()
startPeer mineFrequency difficulty localPort peers dbFilePath delay  = do
  appState <- newAppState difficulty localPort peers dbFilePath
  runReaderT loadBlocksFromDb appState
  concurrently_ (concurrently_ (runReaderT (mineBlockProcess mineFrequency) appState) (runReaderT (serveFunc localPort) appState)) $ do
                    threadDelay delay
                    testFunc $ head peers

newAppState :: (MonadIO m, MonadRandom m) => Int -> Int -> [Peer] -> String  -> m AppState
newAppState diff localPort peers dbFilePath = do
    appPeers <- newTVarIO peers
    keys <- liftIO $ getOrUpdateWalletKeys dbFilePath
    chain <- newTVarIO []
    let difficulty = Difficulty diff
    return $ AppState appPeers localPort chain difficulty (fst keys) (snd keys) dbFilePath

mineBlockProcess :: Int -> AppHandler ()
mineBlockProcess frequency = do
  mineNewBlock
  appState <- ask
  chain <- readTVarIO $ blockChain appState
  liftIO $ print chain
  threadDelay $ frequency * 1000000
  mineBlockProcess frequency

loadBlocksFromDb :: AppHandler ()
loadBlocksFromDb = do
  appState <- ask
  let filePath = dbFilePath appState
  blocks <- liftIO $ loadDbBlocks filePath
  chain <- readTVarIO $ blockChain appState
  let newChain = loadBlocks blocks chain
  atomically $
    writeTVar (blockChain appState) newChain    

mineNewBlock ::  AppHandler ()
mineNewBlock = do
  appState <- ask
  let filePath = dbFilePath appState
  chain <- readTVarIO $ blockChain appState
  let diff = mineDifficulty appState
  let pubKey = publicKey appState
  newBlock <- liftIO $ mineBlock pubKey diff chain $ Nonce 1
  saveBlock filePath newBlock
  let newChain = addBlock newBlock chain
  atomically $
    writeTVar (blockChain appState) newChain
  return ()


serveFunc :: Int -> AppHandler ()
serveFunc port = do
   appState <- ask
   liftIO $ listen (Host "127.0.0.1") (show port) $ \(connectionSocket, remoteAddr) -> do
       putStrLn $ "Listening for TCP connections at " ++ show remoteAddr
       forever . acceptFork connectionSocket $ \(csock, caddr) -> do
         putStrLn $ "Accepted incoming connection from " ++ show caddr
         recvd <- recv csock 4096
         peer <- findPeer appState (ipFromSocketAddress caddr) port
         case recvd of
            Just val ->  case readMsg val of
                     Right msg -> putStrLn ("Received " ++ show msg) >> runReaderT (handleMessage msg peer sendMessage) appState
                     Left e -> return ()
            _ -> print "no value received"

testFunc :: Peer -> IO ()
testFunc peer = return ()-- sendMessage peer $ Message RequestPeers "timeStamp"  RequestPeersData 

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
