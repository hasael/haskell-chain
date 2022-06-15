{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Wallet
import Data.ByteString
import Data.ByteArray
import Crypto.Random
import Crypto.PubKey.ECC.ECDSA (toPublicKey)
import Data.ByteString.Base64

import Data.String (IsString(fromString))
import Crypto.PubKey.ECDSA (encodePublic)
import Crypto.ECC
    ( KeyPair(keypairGetPublic, keypairGetPrivate),
      Curve_P256R1(Curve_P256R1),
      EllipticCurve(decodePoint, encodePoint) )
import Crypto.Error
import Data.Proxy (asProxyTypeOf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Wallet" $ do
    it "verifies correctly signed message" $ do
        let message = ("msgText" :: ByteString) 
        res <- generateKeyPair
        signature <- signMsg (keypairGetPrivate res) message
        let verifyRes = verifyMsg (keypairGetPublic res) signature message
        verifyRes `shouldBe` True
    it "refuses tampered message" $ do
        let message = ("msgText" :: ByteString) 
        let tampered = "tampered" :: ByteString
        res <- generateKeyPair
        signature <- signMsg (keypairGetPrivate res) message
        let verifyRes = verifyMsg (keypairGetPublic res) signature tampered
        verifyRes `shouldBe` False   


  describe "PublicKey" $ do
    it "check printed version" $ do
        let message = ("msgText" :: ByteString) 
        res2 <- generateKeyPair
        let pubK = keypairGetPublic res2
        let enc2 = encodePoint (asProxyTypeOf Curve_P256R1) pubK :: ByteString
        print pubK
        print $ show enc2
        let decoded = decodePoint (asProxyTypeOf Curve_P256R1) enc2
        case decoded of
          CryptoPassed a -> print a
          CryptoFailed _ -> return ()
        print decoded
    
