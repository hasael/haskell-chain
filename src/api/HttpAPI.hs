{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HttpAPI where
import Servant
import AppAPI
import Server
import Network.Wai.Handler.Warp
import Control.Monad.Reader (ReaderT(runReaderT), MonadIO (liftIO))
import AppState (AppHandler, AppState)

startApp :: Int -> AppState -> IO ()
startApp port state = run port $ app state

nt :: AppState -> AppHandler a -> Handler a
nt s x = liftIO $ runReaderT x s

app :: AppState -> Application
app env = serve api $ hoistServer api (nt env) server