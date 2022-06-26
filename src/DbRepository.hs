{-#LANGUAGE OverloadedStrings #-}

module DbRepository where

import AppState
import Block
import Models
import Database.LevelDB.Higher 
import Data.ByteString
import qualified Codec.Binary.UTF8.String as Utf8
import RIO 
import Data.Aeson
import Data.Maybe
import BlockChain
import Wallet
import Data.ByteString.Lazy (toStrict)

saveBlock :: Block -> AppHandler [Item]
saveBlock block = do
    appState <- ask
    let filePath = dbFilePath appState
    runCreateLevelDB filePath "HaskellChain" $ do
        let key = fromString $ hashValue $ hash block
        let value = encode block
        put key $ toStrict value
        scan "" queryItems

loadBlock ::  AppHandler ()
loadBlock = do
    appState <- ask
    let filePath = dbFilePath appState
    runCreateLevelDB filePath "HaskellChain" $ do
        items <- scan "" queryItems
        let values = (fmap snd items) :: [ByteString]
        let maybeBlocks =  ((\item -> decodeStrict' item) <$> values) :: [Maybe Block]
        let blocks =  fromJust <$> maybeBlocks
        chain <- readTVarIO $ blockChain appState
        let newChain = loadBlocks blocks chain
        atomically $
          writeTVar (blockChain appState) newChain

saveWalletKeys :: String -> PublicAddress -> PrivateKeyValue -> IO ()
saveWalletKeys dbFilePath pubKey privateKey = do
    runCreateLevelDB dbFilePath "Wallet" $ do
        let pubKeyValue = pack $ Utf8.encode $ publicAddress pubKey
        let privKeyValue = pack $ Utf8.encode $ privateKeyValue  privateKey
        put "publicKey" pubKeyValue
        put "privateKey" privKeyValue

getOrUpdateWalletKeys :: String -> IO (PublicAddress, PrivateKeyValue)
getOrUpdateWalletKeys dbFilePath = do
    runCreateLevelDB dbFilePath "Wallet" $ do
        pubKeyValue <- get "publicKey"
        privKeyValue <- get "privateKey"
        case (pubKeyValue, privKeyValue) of
          (Just a, Just b) -> return (PublicAddress $ Utf8.decode $ unpack a, PrivateKeyValue $ Utf8.decode $ unpack b)
          _ -> do
            keys <- liftIO generateKeyPair
            let pubKeyValue = pack $ Utf8.encode $ publicAddress (fst keys)
            let privKeyValue = pack $ Utf8.encode $ privateKeyValue (snd keys)
            put "publicKey" pubKeyValue
            put "privateKey" privKeyValue
            return keys
