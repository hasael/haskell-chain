{-# LANGUAGE DeriveGeneric #-}

module AppState where
import RIO (ReaderT, TVar, atomically, readTVar, MonadIO, writeTVar)
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, ToJSON )
import RIO.List (headMaybe)
import Models
import BlockChain (BlockChain)

data AppState = AppState { appPeers :: TVar [Peer], appLocalPort :: Int, 
  blockChain :: TVar BlockChain, 
  mineDifficulty :: Difficulty, 
  publicKey :: PublicAddress,
  privateKey :: PrivateKeyValue}   deriving (Generic)

type AppHandler = ReaderT AppState IO

getStrPeers :: MonadIO m => AppState -> m [String]
getStrPeers appState = do
   p <- atomically $ do
    readTVar $ appPeers appState
   return $ getIpAddr . ipAddress <$> p

getPeers :: MonadIO m => AppState -> m [Peer]
getPeers appState = do
   atomically $ do
    readTVar $ appPeers appState

findPeer :: MonadIO m => AppState -> String -> Int -> m Peer
findPeer appState addr defaultPort = do
    peers <- atomically $ do
        readTVar $ appPeers appState
    let found = headMaybe $ filter (\peer -> ipAddress peer == IPAddress addr && Port defaultPort /= peerPort peer ) peers
                    in
                        case found of
                            Just val -> return val
                            _ -> return $ Peer (IPAddress addr) $ Port defaultPort

mergePeers :: [Peer] -> [Peer] -> [Peer] -> [Peer]
mergePeers localPeers existingPeers peers =
    let newPeers = filter (\a -> notElem a existingPeers && notElem a localPeers) peers in
        newPeers ++ existingPeers


addPeer :: MonadIO m => AppState -> [Peer] -> m ()
addPeer appState peers = atomically $ do
   exPeers <- readTVar $ appPeers appState
   let localPeers = [Peer (IPAddress "127.0.0.1") (Port $ appLocalPort appState), Peer (IPAddress "localhost") (Port $ appLocalPort appState)]
   let newPeers = mergePeers localPeers exPeers peers
   writeTVar (appPeers appState) newPeers