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
import Crypto.Random (MonadRandom)
import BlockChain
import Database.LevelDB.Higher
import qualified Codec.Binary.UTF8.String as Utf8
import DbRepository
import Data.Map
import HttpAPI
import qualified Transaction as T

startPeer :: Int -> Int -> Int -> [(String, Int)] -> String -> Int -> IO ()
startPeer mineFrequency difficulty localPort peersData dbFilePath delay  = do
  let peers = uncurry peerFromData <$> peersData
  appState <- newAppState difficulty localPort peers dbFilePath
  startApp 3080 appState
  runReaderT (startUp mineFrequency) appState

startUp :: Int -> AppHandler ()
startUp mineFrequency = do
  appState <- ask
  loadBlocksFromDb
  concurrently_ (mineBlockProcess mineFrequency) $ concurrently_ (serveFunc $ appLocalPort appState) $ concurrently_ checkPeersBlocksProcess pingPeersProcess

pingPeersProcess :: AppHandler ()
pingPeersProcess = pingPeers sendMessage >> threadDelay 5000000 >> pingPeersProcess

checkPeersBlocksProcess :: AppHandler ()
checkPeersBlocksProcess = do
  appState <- ask
  liftIO $ checkPeersBlocks sendMessage appState
  threadDelay 6000000
  checkPeersBlocksProcess

newAppState :: (MonadIO m, MonadRandom m) => Int -> Int -> [Peer] -> String -> m AppState
newAppState diff localPort peers dbFilePath = do
    appPeers <- newTVarIO peers
    keys <- liftIO $ getOrUpdateWalletKeys dbFilePath
    chain <- newTVarIO []
    trxPool <- newTVarIO []
    peerState <- newTVarIO empty
    let difficulty = Difficulty diff
    return $ AppState appPeers trxPool peerState localPort chain difficulty (fst keys) (snd keys) dbFilePath

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
  newBlock <- liftIO $ mineCoinbaseBlock pubKey diff chain $ Nonce 1
  saveBlock filePath newBlock
  let newChain = addBlock newBlock chain
  atomically $
    writeTVar (blockChain appState) newChain
  return ()

mineNewBlockTrxs :: [T.Transaction] -> AppHandler ()
mineNewBlockTrxs trxs = do
  appState <- ask
  let filePath = dbFilePath appState
  chain <- readTVarIO $ blockChain appState
  let diff = mineDifficulty appState
  let pubKey = publicKey appState
  newBlock <- liftIO $ mineTrxsBlock pubKey  trxs diff chain $ Nonce 1
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
                     Left e -> void (putStrLn ("Error pasrsing message " ++ show e))
            _ -> print "no value received"

readBlock :: ByteString -> Either String Block
readBlock =  eitherDecodeStrict

readMsg :: ByteString -> Either String Message
readMsg =  eitherDecodeStrict

sendMessage :: Peer -> Message -> IO (Either IOException ())
sendMessage peer msg = tryIO $ do
     putStrLn $ "Sending message" ++ show msg ++ " to " ++ show peer
     connect (getIpAddr $ ipAddress peer) (show $ getPort $ peerPort peer) $ \(connectionSocket, remoteAddr) -> do
       send connectionSocket $ toStrict $ encode msg
       closeSock connectionSocket

checkTrxPool :: AppHandler ()
checkTrxPool = do
  appState <- ask
  trxs <- getTrxPool appState
  let trxLen = length trxs
  if trxLen >= 10 then mineNewBlockTrxs trxs else return ()

ipFromSocketAddress :: SockAddr -> String
ipFromSocketAddress sockAddr =  head $ wordsWhen (==':') $ show sockAddr

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
