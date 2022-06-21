{-# LANGUAGE DeriveGeneric #-}

module Block where

import Data.Aeson
import GHC.Generics
import Models
import Transaction

data Block = Block {
    index :: BlockIndex,
    difficulty :: Difficulty,
    hash :: HashValue,
    merkleRoot :: HashValue,
    timeStamp :: Timestamp,
    nonce :: Nonce,
    transactions :: [Transaction],
    version :: BlockVersion
} deriving (Show , Generic)

instance FromJSON Block 
instance ToJSON Block
