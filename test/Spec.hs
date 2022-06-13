{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Wallet
import Data.ByteString
import Data.ByteArray
import Crypto.Random

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


--instance MonadRandom Spec where
--  getRandomBytes :: (ByteArray byteArray, MonadRandom IO) => Int -> Spec byteArray
--  getRandomBytes = liftIO getRandomBytes