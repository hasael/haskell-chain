{-# LANGUAGE DeriveGeneric #-}

module Block where

import Data.Aeson
import GHC.Generics
import Models
import Transaction

data Block = Block {
    index :: Int,
    hash :: HashValue,
    merkleRoot :: String,
    timeStamp :: Timestamp,
    nonce :: Int,
    transactions :: [Transaction],
    version :: Int
} deriving (Show , Generic)

instance FromJSON Block 
instance ToJSON Block
