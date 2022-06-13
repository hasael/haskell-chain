module Wallet where

import RIO
import Crypto.PubKey.ECC.ECDSA (PublicKey(PublicKey), PrivateKey, sign, Signature, verify)
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.PubKey.ECC.Types (getCurveByName, CurveName(SEC_p224r1))
import Crypto.Random (MonadRandom)
import Crypto.Hash.Algorithms
import Data.ByteArray

generateKeyPair :: (MonadRandom m)=> m (PublicKey, PrivateKey)
generateKeyPair = generate $ getCurveByName SEC_p224r1

signMsg :: (ByteArrayAccess msg, MonadRandom m) => PrivateKey -> msg -> m Signature
signMsg privateKey msg = sign privateKey SHA256 msg

verifyMsg :: ByteArrayAccess msg => PublicKey -> Signature -> msg -> Bool
verifyMsg publicKey signature msg = verify SHA256 publicKey signature msg