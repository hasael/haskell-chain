module WalletOld where

import RIO
import Crypto.PubKey.ECC.ECDSA (PublicKey(PublicKey), PrivateKey, sign, Signature (Signature), verify, KeyPair)
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.PubKey.ECC.Types (getCurveByName, CurveName(SEC_p224r1))
import Crypto.Random (MonadRandom)
import Crypto.Hash.Algorithms
import Data.ByteArray
import Transaction
import qualified Models as M
import Crypto.ECC as E
import Data.Proxy (asProxyTypeOf)

generateKeyPair :: (MonadRandom m)=> m (PublicKey, PrivateKey)
generateKeyPair = generate $ getCurveByName SEC_p224r1

generateKeyPair2 :: (MonadRandom m) => m (E.KeyPair Curve_P256R1)
generateKeyPair2 = curveGenerateKeyPair $ asProxyTypeOf Curve_P256R1 

signMsg :: (ByteArrayAccess msg, MonadRandom m) => PrivateKey -> msg -> m Signature
signMsg privateKey msg = sign privateKey SHA256 msg

verifyMsg :: ByteArrayAccess msg => PublicKey -> Signature -> msg -> Bool
verifyMsg publicKey signature msg = verify SHA256 publicKey signature msg

signTrx :: (MonadRandom m) => PrivateKey -> Transaction -> m Signature
signTrx privateKey trx = sign privateKey SHA256 $ toByteStr trx

verifyTrxSignature :: PublicKey -> Transaction -> Transaction -> Bool
verifyTrxSignature publicKey prevTrx trx = (hash prevTrx == previousTrx (input trx))
  && verifyMsg publicKey (fromSig $ signature $ input trx) (toByteStr prevTrx)

fromSig :: M.Signature -> Signature
fromSig sign = Signature (M.sign_r sign) (M.sign_s sign)