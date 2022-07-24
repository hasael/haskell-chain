{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HttpAPI where
import Servant
import AppAPI
import Server
import Network.Wai.Handler.Warp

startApp :: Int -> IO ()
startApp port = run port app

app :: Application
app = serve api server
