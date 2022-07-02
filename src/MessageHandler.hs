module MessageHandler where
import Models
import AppState
import RIO
import Control.Monad.Trans.Reader (withReaderT)
import GHC.IO.Handle.Types (HandleType(AppendHandle))
import Messages
import TimeService

handleMessage :: Message -> Peer -> (Peer -> Message -> IO ()) -> AppHandler ()
handleMessage msg peer sendMessage = case msgData msg of
  RequestPeersData -> do
               appState <- ask
               peers <- getHealthyPeers appState
               timeStamp <- liftIO getTimeStamp
               liftIO $ sendMessage peer $ Message NewPeer timeStamp $ NewPeerData peers 
  NewBlockData block -> liftIO $ print $ "Received " ++ show msg
  NewPeerData peers -> do 
      appState <- ask
      addPeer appState peers
      checkPeers <- getPeers appState
      void $ liftIO $ print "new peers: " >> traverse (print . show) checkPeers
  ResponseChainData chainSize -> do
      appState <- ask
      setPeerHealthy appState peer
  RequestChainData -> do
      appState <- ask
      chainSize <- getChainSize appState
      timeStamp <- liftIO getTimeStamp
      liftIO $ sendMessage peer $ Message ResponseChain timeStamp $ ResponseChainData $ blockIndex chainSize
  RequestBlockData idx -> do
      appState <- ask
      block <- getBlock (BlockIndex idx) appState
      timeStamp <- liftIO getTimeStamp
      liftIO $ sendMessage peer $ Message ResponseBlock timeStamp $ ResponseBlockData block
  ResponseBlockData block -> do
      appState <- ask
      addNewBlock block appState

      