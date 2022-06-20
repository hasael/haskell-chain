{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Models where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Data.Int (Int64)

newtype Timestamp = Timestamp {timeStamp :: Int64}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype IPAddress = IPAddress {getIpAddr :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype Port = Port {getPort :: Int}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype PublicAddress = PublicAddress {publicAddress :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype PrivateKeyValue = PrivateKeyValue {privateKeyValue :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
data SignatureValue = SignatureValue {signatureR :: Integer, signatureS :: Integer}   deriving (Show, Generic, Eq)
newtype HashValue = HashValue {hashValue :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)

data Peer = Peer {ipAddress :: IPAddress, peerPort :: Port}   deriving (Show, Generic, Eq)

instance FromJSON Peer
instance ToJSON Peer
instance FromJSON SignatureValue
instance ToJSON SignatureValue