{-# LANGUAGE DeriveGeneric #-}

module Transaction where
import Models
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

data TrxInput = TrxInput {
    signature :: String,
    publicKey :: PublicKey,
    previousTrx :: HashValue
} deriving (Show , Generic)

data TrxOutput = TrxOutput {
    receiver :: PublicKey,
    value :: Double
} deriving (Show , Generic)

data Transaction = Transaction {
    timeStamp :: Timestamp,
    version :: Int,
    input :: TrxInput,
    output :: TrxOutput,
    hash :: HashValue
} | CoinbaseTransaction {
    timeStamp :: Timestamp,
    version :: Int,
    output :: TrxOutput,
    hash :: HashValue
} deriving (Show , Generic)

instance FromJSON TrxInput
instance FromJSON TrxOutput
instance FromJSON Transaction

instance ToJSON TrxInput
instance ToJSON TrxOutput
instance ToJSON Transaction