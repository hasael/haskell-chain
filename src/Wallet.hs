module Wallet where

import RIO
import Crypto.PubKey.ECDSA 
import Crypto.Random (MonadRandom)
import Crypto.Hash.Algorithms
import Data.ByteArray
import Transaction
import qualified Models as M
import Crypto.ECC as E
import Crypto.PubKey.ECDSA
import Data.Proxy (asProxyTypeOf)


generateKeyPair :: (MonadRandom m) => m (E.KeyPair Curve_P256R1)
generateKeyPair = curveGenerateKeyPair $ asProxyTypeOf Curve_P256R1 

signMsg :: (ByteArrayAccess msg, MonadRandom m) => PrivateKey Curve_P256R1 -> msg -> m (Signature Curve_P256R1)
signMsg privateKey msg = sign (asProxyTypeOf Curve_P256R1) privateKey SHA256 msg

verifyMsg :: ByteArrayAccess msg => PublicKey Curve_P256R1 -> Signature Curve_P256R1 -> msg -> Bool
verifyMsg publicKey signature msg = verify (asProxyTypeOf Curve_P256R1) SHA256 publicKey signature msg

signTrx :: (MonadRandom m) => PrivateKey Curve_P256R1 -> Transaction -> m (Signature Curve_P256R1)
signTrx privateKey trx = sign (asProxyTypeOf Curve_P256R1) privateKey SHA256 $ toByteStr trx

--verifyTrxSignature :: PublicKey Curve_P256R1 -> Transaction -> Transaction -> Bool
--verifyTrxSignature publicKey prevTrx trx = (hash prevTrx == previousTrx (input trx))
--  && verifyMsg publicKey (fromSig $ signature $ input trx) (toByteStr prevTrx)

--fromSig :: M.Signature -> Signature Curve_P256R1
--fromSig sign = Signature (M.sign_r sign) (M.sign_s sign)