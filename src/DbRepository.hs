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

saveBlock :: (MonadUnliftIO m, MonadThrow m) => String -> Block -> m ()
saveBlock dbFilePath block = do
    runCreateLevelDB dbFilePath "HaskellChain" $ do
        let key = fromString $ hashValue $ hash block
        let value = encode block
        put key $ toStrict value

loadDbBlocks :: (MonadUnliftIO m, MonadThrow m) => String -> m [Block]
loadDbBlocks dbFilePath = do
    runCreateLevelDB dbFilePath "HaskellChain" $ do
        items <- scan "" queryItems
        let values = fmap snd items :: [ByteString]
        let maybeBlocks =  (decodeStrict' <$> values) :: [Maybe Block]
        return $ fromJust <$> maybeBlocks

saveWalletKeys :: (MonadUnliftIO m, MonadThrow m) => String -> PublicAddress -> PrivateKeyValue -> m ()
saveWalletKeys dbFilePath pubKey privateKey = do
    runCreateLevelDB dbFilePath "Wallet" $ do
        let pubKeyValue = pack $ Utf8.encode $ publicAddress pubKey
        let privKeyValue = pack $ Utf8.encode $ privateKeyValue  privateKey
        put "publicKey" pubKeyValue
        put "privateKey" privKeyValue

getOrUpdateWalletKeys :: (MonadUnliftIO m, MonadThrow m) => String -> m (PublicAddress, PrivateKeyValue)
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
