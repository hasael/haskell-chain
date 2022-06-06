{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module AppState where
import RIO (ReaderT)
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON )
import RIO.List (headMaybe)

newtype IPAddress = IPAddress {getIpAddr :: String}   deriving (Show, Generic, FromJSON, Eq)
newtype Port = Port {getPort :: Int}   deriving (Show, Generic, FromJSON)
data Peer = Peer {ipAddress :: IPAddress, peerPort :: Port}   deriving (Show, Generic)
data AppState = AppState { appPeers :: [Peer], appLocalPort :: Int}   deriving (Show, Generic)

type AppHandler = ReaderT AppState IO

getStrPeers :: AppState -> [String]
getStrPeers appState =  getIpAddr . ipAddress <$> appPeers appState

findPeer :: AppState -> String -> Int -> Peer
findPeer appState addr defaultPort = let found = headMaybe $ filter (\peer -> ipAddress peer == IPAddress addr ) $ appPeers appState
                    in
                        case found of
                            Just val -> val
                            _ -> Peer (IPAddress addr) $ Port defaultPort