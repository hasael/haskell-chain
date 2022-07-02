module Wallet (
    generateKeyPair,
    signMsg,
    verifyMsg,
    signTransaction,
    verifyTransaction,
    createCoinbaseTrx
) where

import RIO
import Crypto.PubKey.ECDSA
import Crypto.Error
import Crypto.Random (MonadRandom)
import Crypto.Hash.Algorithms
import Data.ByteArray as BA
import Transaction
import qualified Models as M
import Data.ByteString.Base64 as B
import Crypto.ECC as E
import Crypto.PubKey.ECDSA
import Data.Proxy (asProxyTypeOf)
import Models
import Codec.Binary.UTF8.String as Utf8
import Data.ByteString as BS
import Crypto.Hash
import System.Posix (epochTime)
import Foreign.C (CTime (CTime))
import TimeService


generateKeyPair :: (MonadRandom m) => m (M.PublicAddress, M.PrivateKeyValue )
generateKeyPair = do
   keyPair <- curveGenerateKeyPair $ asProxyTypeOf Curve_P256R1
   let encodedPublicKey = encodePublicKey (keypairGetPublic keyPair)
   let encodedPrivateKey = encodePrivateKey (keypairGetPrivate keyPair)
   return (encodedPublicKey, encodedPrivateKey)

signMsg :: (ByteArrayAccess msg, MonadRandom m) => M.PrivateKeyValue -> msg -> m M.SignatureValue
signMsg privateKey msg = do
    let privateKeyDecoded = decodePrivateKey privateKey
    signature <- sign (asProxyTypeOf Curve_P256R1) privateKeyDecoded SHA256 msg
    let ints = signatureToIntegers (asProxyTypeOf Curve_P256R1) signature
    return (uncurry M.SignatureValue ints)

signTransaction :: (MonadRandom m) => M.PrivateKeyValue -> Transaction -> m M.SignatureValue
signTransaction privateKey trx = signMsg privateKey $ trxToByteStr trx

verifyTransaction :: M.PublicAddress -> M.SignatureValue -> Transaction -> Bool
verifyTransaction publicKey signature trx = verifyMsg publicKey signature $ trxToByteStr trx

createCoinbaseTrx :: MonadIO m => M.PublicAddress -> Double -> m Transaction
createCoinbaseTrx publicKey amout = do
     timeStamp <- liftIO getTimeStamp
     return $ CoinbaseTransaction (Timestamp timeStamp) 1 [TrxOutput publicKey amout] (HashValue (hashContent ((show timeStamp ++ show [TrxOutput publicKey amout] ++ show 1 ))))

cTimeValue :: CTime -> Int64
cTimeValue ctime = case ctime of
    CTime val -> val

hashContent :: String -> String
hashContent bs = show $ hashWith SHA3_512 $ BS.pack $ Utf8.encode bs

verifyMsg :: ByteArrayAccess msg => M.PublicAddress -> M.SignatureValue -> msg -> Bool
verifyMsg publicKey signature msg = verify (asProxyTypeOf Curve_P256R1) SHA256 (decodePublicKey publicKey) (decodeSignature signature) msg

encodePrivateKey :: PrivateKey Curve_P256R1 -> M.PrivateKeyValue
encodePrivateKey privateKey = M.PrivateKeyValue $ Utf8.decode $ BS.unpack $ B.encode $ encodeScalar (asProxyTypeOf Curve_P256R1) privateKey

encodePublicKey :: PublicKey Curve_P256R1 -> M.PublicAddress
encodePublicKey pubKey = M.PublicAddress $ Utf8.decode $ BS.unpack $ B.encode $ encodePoint (asProxyTypeOf Curve_P256R1) pubKey

decodePrivateKey :: M.PrivateKeyValue -> PrivateKey Curve_P256R1
decodePrivateKey privateKey = throwCryptoError $ decodeScalar (asProxyTypeOf Curve_P256R1) ((fromRightThrow $ B.decode $ fromString $ M.privateKeyValue privateKey) :: ByteString)

decodePublicKey :: M.PublicAddress -> PublicKey Curve_P256R1
decodePublicKey pubAddress = throwCryptoError $ decodePublic (asProxyTypeOf Curve_P256R1)  (fromRightThrow $ B.decode $ fromString $ M.publicAddress pubAddress)

fromRightThrow :: Either String b -> b
fromRightThrow e = case e of
    Right a -> a
    Left e -> error e

decodeSignature :: M.SignatureValue -> Signature Curve_P256R1
decodeSignature signatureV = throwCryptoError $ signatureFromIntegers (asProxyTypeOf Curve_P256R1) (M.signatureR signatureV, M.signatureS signatureV)
