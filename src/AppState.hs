{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module AppState where
import RIO (ReaderT, TVar, atomically, readTVar, MonadIO, writeTVar)
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, ToJSON )
import RIO.List (headMaybe)

newtype IPAddress = IPAddress {getIpAddr :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype Port = Port {getPort :: Int}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
data Peer = Peer {ipAddress :: IPAddress, peerPort :: Port}   deriving (Show, Generic, Eq)
data AppState = AppState { appPeers :: TVar [Peer], appLocalPort :: Int}   deriving (Generic)

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
    let found = headMaybe $ filter (\peer -> ipAddress peer == IPAddress addr ) peers
                    in
                        case found of
                            Just val -> return val
                            _ -> return $ Peer (IPAddress addr) $ Port defaultPort

mergePeers :: [Peer] -> [Peer] -> [Peer]
mergePeers existingPeers peers =
    let newPeers = filter (\a -> not (elem a existingPeers)) peers in
        newPeers ++ existingPeers


addPeer :: MonadIO m => AppState -> [Peer] -> m ()
addPeer appState peers = atomically $ do
   exPeers <- readTVar $ appPeers appState
   let newPeers = mergePeers exPeers peers
   writeTVar (appPeers appState) newPeers

instance FromJSON Peer

instance ToJSON Peer