module TestHelpers where

import Models
import Transaction 

createTestTrx :: Double -> Transaction
createTestTrx value = CoinbaseTransaction (Timestamp 1234) 1 ( [TrxOutput (PublicAddress "publicAddress") value] ) ( HashValue "hashValue")