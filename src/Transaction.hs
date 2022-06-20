{-# LANGUAGE DeriveGeneric #-}

module Transaction where
import Models
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString
import Data.String (IsString(fromString))

data TrxInput = TrxInput {
    signature :: SignatureValue,
    publicKey :: PublicAddress,
    previousTrx :: HashValue
} deriving (Show , Generic)

data TrxOutput = TrxOutput {
    receiver :: PublicAddress,
    value :: Double
} deriving (Show , Generic)

data Transaction = Transaction {
    timeStamp :: Timestamp,
    version :: Integer,
    input :: TrxInput,
    output :: [TrxOutput],
    hash :: HashValue
} | CoinbaseTransaction {
    timeStamp :: Timestamp,
    version :: Integer,
    output :: [TrxOutput],
    hash :: HashValue
} deriving (Show , Generic)

trxToByteStr :: Transaction -> ByteString
trxToByteStr trx = fromString $ show trx

instance FromJSON TrxInput
instance FromJSON TrxOutput
instance FromJSON Transaction

instance ToJSON TrxInput
instance ToJSON TrxOutput
instance ToJSON Transaction