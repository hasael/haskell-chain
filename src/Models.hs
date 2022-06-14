{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Models where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

newtype Timestamp = Timestamp {timeStamp :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype IPAddress = IPAddress {getIpAddr :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype Port = Port {getPort :: Int}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype PublicKey = PublicKey {publicKey :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype HashValue = HashValue {hashValue :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype Signature = Signature {signature :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)

data Peer = Peer {ipAddress :: IPAddress, peerPort :: Port}   deriving (Show, Generic, Eq)

instance FromJSON Peer
instance ToJSON Peer