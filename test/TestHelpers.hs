module TestHelpers where

import Models
import Transaction 

createTestTrx :: Double -> Transaction
createTestTrx value = CoinbaseTransaction (Timestamp "Timestamp") 1 ( [TrxOutput (PublicAddress "publicAddress") value] ) ( HashValue "hashValue")