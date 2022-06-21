module HashService (hashContent) where

import Data.ByteString as BS
import Data.ByteArray as BA
import Codec.Binary.UTF8.String as Utf8
import Crypto.Hash

hashContent :: String -> String
hashContent bs = show $ hashWith SHA3_512 $ BS.pack $ Utf8.encode bs