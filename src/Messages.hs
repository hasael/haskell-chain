{-# LANGUAGE DeriveGeneric #-}
module Messages where

import Models
import GHC.Generics
import Data.Aeson
import Block
import Data.Int (Int64)

data MessageType = NewPeer | NewBlock | RequestPeers | RequestChain | ResponseChain | RequestBlock | ResponseBlock deriving (Show , Generic)

data MessageData = NewPeerData {
    peerAddrs :: [Peer]
} | NewBlockData {
    block :: Block
} 
  | RequestPeersData {} 
  | RequestChainData {}
  | ResponseChainData {
    chainSize :: Int
  }
  | RequestBlockData {
    requestBlockIndex :: Int
  } 
  | ResponseBlockData{
    block :: Block
  }
  deriving (Show , Generic)

data Message = Message {
    messageType :: MessageType,
    msgTimeStamp :: Int64,
    msgData :: MessageData
} deriving (Show , Generic)


instance FromJSON MessageType 

instance FromJSON MessageData

instance FromJSON Message 

instance ToJSON MessageType 

instance ToJSON MessageData

instance ToJSON Message 