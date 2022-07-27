module TestHelpers where

import Models
import Transaction 
import AppState (AppState (AppState))
import RIO
import Data.Map

createTestTrx :: Double -> Transaction
createTestTrx value = CoinbaseTransaction (Timestamp 1234) 1 ( [TrxOutput (PublicAddress "publicAddress") value] ) ( HashValue "hashValue")

createTestAppState :: (MonadIO m) => m AppState
createTestAppState = do
    appPeers <- newTVarIO []
    let keys = (PublicAddress "PublicAddress", PrivateKeyValue "PublicAddress")
    chain <- newTVarIO []
    trxPool <- newTVarIO []
    peerState <- newTVarIO empty
    let difficulty = Difficulty 1
    return $ AppState appPeers trxPool peerState 123 chain difficulty (fst keys) (snd keys) "dbFilePath"