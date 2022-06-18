{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Wallet
import Data.ByteString as BS
import Data.ByteArray
import Crypto.Random
import Crypto.PubKey.ECC.ECDSA (toPublicKey)
import Data.ByteString.Base64 as B

import Data.String (IsString(fromString))
import Crypto.PubKey.ECDSA (encodePublic)
import Crypto.ECC
import Crypto.Error
import Data.Proxy (asProxyTypeOf)
import Models
import qualified Data.ByteArray as B
import Codec.Binary.UTF8.String as Utf8
import TestHelpers
import Wallet (signTransaction)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Wallet" $ do
    it "verifies correctly signed message" $ do
        let message = ("msgText" :: ByteString) 
        res <- generateKeyPair
        signature <- signMsg (snd res) message
        let verifyRes = verifyMsg (fst res) signature message
        verifyRes `shouldBe` True
    it "refuses tampered message" $ do
        let message = ("msgText" :: ByteString) 
        let tampered = "tampered" :: ByteString
        res <- generateKeyPair
        signature <- signMsg (snd res) message
        let verifyRes = verifyMsg (fst res) signature tampered
        verifyRes `shouldBe` False   
    it "verifies correctly signed transaction" $ do
        let testTrx = createTestTrx 10
        res <- generateKeyPair
        signature <- signTransaction (snd res) testTrx
        let verifyRes = verifyTransaction (fst res) signature testTrx
        verifyRes `shouldBe` True
    it "refuses tampered transaction" $ do
        let testTrx = createTestTrx 10
        let tamperedTrx = createTestTrx 8
        res <- generateKeyPair
        signature <- signTransaction (snd res) testTrx
        let verifyRes = verifyTransaction (fst res) signature tamperedTrx
        verifyRes `shouldBe` False   