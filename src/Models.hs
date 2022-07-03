{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Models where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Data.Int (Int64)

newtype Timestamp = Timestamp {timeStamp :: Int64}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype IPAddress = IPAddress {getIpAddr :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq, Ord)
newtype Port = Port {getPort :: Int}   deriving (Show, Generic, FromJSON, ToJSON, Eq, Ord)
newtype PublicAddress = PublicAddress {publicAddress :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype PrivateKeyValue = PrivateKeyValue {privateKeyValue :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
data SignatureValue = SignatureValue {signatureR :: Integer, signatureS :: Integer}   deriving (Show, Generic, Eq)
newtype HashValue = HashValue {hashValue :: String}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype Nonce = Nonce {nonce :: Int}   deriving (Show, Generic, FromJSON, ToJSON, Eq, Num)
newtype Difficulty = Difficulty {difficulty :: Int}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype BlockIndex = BlockIndex {blockIndex :: Int}   deriving (Show, Generic, FromJSON, ToJSON, Eq)
newtype BlockVersion = BlockVersion {blockVersion :: Int}   deriving (Show, Generic, FromJSON, ToJSON, Eq)

data Peer = Peer {ipAddress :: IPAddress, peerPort :: Port, healthy :: Bool}   deriving (Show, Generic, Eq, Ord)

peerFromData :: String -> Int -> Peer
peerFromData ipAddr port = Peer (IPAddress ipAddr) (Port port) False

instance FromJSON Peer
instance ToJSON Peer
instance FromJSON SignatureValue
instance ToJSON SignatureValue