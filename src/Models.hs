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