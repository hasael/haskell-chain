{-# LANGUAGE DeriveGeneric #-}

module AppState where
import RIO (ReaderT, TVar, atomically, readTVar, MonadIO, writeTVar)
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, ToJSON )
import RIO.List (headMaybe)
import Models
import BlockChain (BlockChain, currentIndex, addBlock)
import Block
import Control.Monad
import Data.Map (Map, insert)
import Messages (MessageData(chainSize))

data AppState = AppState {
  appPeers :: TVar [Peer],
  peersState :: TVar (Map Peer Int),
  appLocalPort :: Int,
  blockChain :: TVar BlockChain,
  mineDifficulty :: Difficulty,
  publicKey :: PublicAddress,
  privateKey :: PrivateKeyValue,
  dbFilePath :: String} deriving (Generic)

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

getHealthyPeers :: MonadIO m => AppState -> m [Peer]
getHealthyPeers appState = do
   peers <- atomically $ do
    readTVar $ appPeers appState
   return $ filter healthy peers

findPeer :: MonadIO m => AppState -> String -> Int -> m Peer
findPeer appState addr defaultPort = do
    peers <- atomically $ do
        readTVar $ appPeers appState
    let found = headMaybe $ filter (\peer -> ipAddress peer == IPAddress addr && Port defaultPort /= peerPort peer ) peers
                    in
                        case found of
                            Just val -> return val
                            _ -> return $ Peer (IPAddress addr) (Port defaultPort) True

mergePeers :: [Peer] -> [Peer] -> [Peer] -> [Peer]
mergePeers localPeers existingPeers peers =
    let newPeers = filter (\a -> notElem a existingPeers && notElem a localPeers) peers in
        newPeers ++ existingPeers


addPeer :: MonadIO m => AppState -> [Peer] -> m ()
addPeer appState peers = atomically $ do
   exPeers <- readTVar $ appPeers appState
   let localPeers = [Peer (IPAddress "127.0.0.1") (Port $ appLocalPort appState) True, Peer (IPAddress "localhost") (Port $ appLocalPort appState) True]
   let newPeers = mergePeers localPeers exPeers peers
   writeTVar (appPeers appState) newPeers

setPeerHealthy :: MonadIO m => AppState -> Peer -> m ()
setPeerHealthy appState peer = atomically $ do
   exPeers <- readTVar $ appPeers appState
   let tempPeers = dropWhile (\p -> (ipAddress p == ipAddress peer) && (peerPort p == peerPort peer)) exPeers
   let newPeer = Peer (ipAddress peer) (peerPort peer) True
   let newPeers = tempPeers ++ [newPeer]
   writeTVar (appPeers appState) newPeers

setPeerUnhealthy :: MonadIO m => AppState -> Peer -> m ()
setPeerUnhealthy appState peer = atomically $ do
   exPeers <- readTVar $ appPeers appState
   let tempPeers = dropWhile (\p -> (ipAddress p == ipAddress peer) && (peerPort p == peerPort peer)) exPeers
   let newPeer = Peer (ipAddress peer) (peerPort peer) False
   let newPeers = tempPeers ++ [newPeer]
   writeTVar (appPeers appState) newPeers

getChainSize :: MonadIO m => AppState -> m BlockIndex
getChainSize appState = do
    chain <- atomically $ do
        readTVar $ blockChain appState
    return $ currentIndex chain

getChain :: MonadIO m => AppState -> m BlockChain
getChain appState = do
    atomically $ do
        readTVar $ blockChain appState

getBlock :: MonadIO m => BlockIndex -> AppState -> m (Maybe Block)
getBlock idx appState = do
    chain <- atomically $ do
        readTVar $ blockChain appState
    return $ headMaybe $ filter (\b -> index b == idx) chain

addNewBlock :: MonadIO m => Block -> AppState -> m ()
addNewBlock block appState = atomically $ do
        chain <- readTVar $ blockChain appState
        let newChain = addBlock block chain
        let existsPeer = not (any (\b -> index b == index block) chain)
        when existsPeer $
          writeTVar (blockChain appState) newChain

setPeerState :: MonadIO m => AppState -> Peer -> Int -> m ()
setPeerState appState peer chainSize =  atomically $ do
        peerState <- readTVar $ peersState appState
        let newState = insert peer chainSize peerState
        writeTVar (peersState appState) newState

getPeersState :: MonadIO m => AppState -> m (Map Peer Int)
getPeersState appState = do
    atomically $ do
        readTVar $ peersState appState