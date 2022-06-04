{-# LANGUAGE DeriveGeneric #-}

module Models where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data Block = Block {
    hash :: String,
    merkleRoot :: String,
    timeStamp :: String,
    nonce :: Int,
    transactions :: [String]
} deriving (Show , Generic)

instance FromJSON Block 

instance ToJSON Block

data MessageType = NewPeer | NewBlock deriving (Show , Generic)

data MessageData = NewPeerData {
    peerAddr :: String
} | NewBlockData {
    block :: Block
} deriving (Show , Generic)

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