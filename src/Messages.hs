{-# LANGUAGE DeriveGeneric #-}
module Messages where

import Models
import GHC.Generics
import Data.Aeson
import Block

data MessageType = NewPeer | NewBlock | RequestPeers deriving (Show , Generic)

data MessageData = NewPeerData {
    peerAddrs :: [Peer]
} | NewBlockData {
    block :: Block
} 
  | RequestPeersData {} 
  deriving (Show , Generic)

data Message = Message {
    messageType :: MessageType,
    msgTimeStamp :: String,
    msgData :: MessageData
} deriving (Show , Generic)

instance FromJSON MessageType 

instance FromJSON MessageData

instance FromJSON Message 

instance ToJSON MessageType 

instance ToJSON MessageData

instance ToJSON Message 