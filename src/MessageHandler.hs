module MessageHandler where
import Models
import AppState
import RIO
import GHC.IO.Handle.Types (HandleType(AppendHandle))
import Messages
import TimeService

handleMessage :: Exception e => Message -> Peer -> (Peer -> Message -> IO (Either e ())) -> AppHandler ()
handleMessage msg peer sendMessage = case msgData msg of
  RequestPeersData -> do
      appState <- ask
      peers <- getHealthyPeers appState
      timeStamp <- liftIO getTimeStamp
      let newMsg = Message NewPeer timeStamp $ NewPeerData peers
      liftIO $ handleSendMessage sendMessage newMsg peer appState
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
      let newMsg = Message ResponseChain timeStamp $ ResponseChainData $ blockIndex chainSize
      liftIO $ handleSendMessage sendMessage newMsg peer appState
  RequestBlockData idx -> do
      appState <- ask
      maybeBlock <- getBlock (BlockIndex idx) appState
      timeStamp <- liftIO getTimeStamp
      case maybeBlock of
        Just block -> liftIO $ handleSendMessage sendMessage (Message ResponseBlock timeStamp $ ResponseBlockData block) peer appState 
        _ -> return ()
  ResponseBlockData block -> do
      appState <- ask
      addNewBlock block appState

pingPeers ::  (Peer -> Message -> IO (Either e ())) -> AppHandler ()
pingPeers sendMessage = do
    appState <- ask
    peers <- getPeers appState
    timeStamp <- liftIO getTimeStamp
    let msg = Message RequestChain timeStamp RequestChainData
    liftIO $ sequence_ $ (`sendMessage` msg) <$> peers

handleSendMessage :: (Peer -> Message -> IO (Either e ())) -> Message -> Peer -> AppState -> IO ()
handleSendMessage sendMessage msg peer appState = do
    msgResult <-  sendMessage peer msg
    case msgResult of
        Right a -> return a
        Left e -> setPeerUnhealthy appState peer