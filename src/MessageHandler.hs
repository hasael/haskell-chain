module MessageHandler where
import Models
import AppState
import RIO
import Control.Monad.Trans.Reader (withReaderT)
import GHC.IO.Handle.Types (HandleType(AppendHandle))

handleMessage :: Message -> Peer -> (Peer -> Message -> IO ()) -> AppHandler ()
handleMessage msg peer sendMessage = case msgData msg of
  RequestPeersData -> do
               appState <- ask
               peers <- getPeers appState
               liftIO $ sendMessage peer $ Message NewPeer "timeStamp"  $ NewPeerData peers 
  NewBlockData block -> liftIO $ print $ "Received " ++ show msg
  NewPeerData peers -> do 
      appState <- ask
      addPeer appState peers
      checkPeers <- getPeers appState
      void $ liftIO $ print "new peers: " >> traverse (print . show) checkPeers

      