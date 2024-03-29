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
import Transaction
import Data.Aeson (ToJSON(toJSON))
import RIO as R (concurrently, threadDelay, length)
import BlockChain
import qualified Block as B
import Models (Difficulty(Difficulty), Nonce (Nonce))
import HashService (hashContent)
import AppState
import Data.Foldable as F


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

    it "same transactions have same hash" $ do
        let createTrxAction = (createCoinbaseTrx (PublicAddress "Addr") 10)
        trxs <- concurrently  createTrxAction  createTrxAction
        (hash (fst trxs)) `shouldBe` (hash (snd trxs))

    it "different transactions have different hash" $ do
        testTrx <- createCoinbaseTrx (PublicAddress "Addr") 10
        secondTrx <- createCoinbaseTrx (PublicAddress "Addr") 9
        (hash testTrx) `shouldNotBe` (hash secondTrx)

    it "transactions of different times have different hash" $ do
        testTrx <- createCoinbaseTrx (PublicAddress "Addr") 10
        secondTrx <- threadDelay 1000000 >> createCoinbaseTrx (PublicAddress "Addr") 10
        (hash testTrx) `shouldNotBe` (hash secondTrx)

  describe "BlockChain" $ do
    it "adds block correctly" $ do
        let chain = [] :: BlockChain
        let pubAddr = PublicAddress "Addr"
        let diff = Difficulty 0
        let nonce = Nonce 1
        newBlock <- mineCoinbaseBlock pubAddr diff chain nonce
        let newChain = addBlock newBlock chain
        (R.length newChain) `shouldBe` 1

    it "gets size correctly" $ do
        let chain = [] :: BlockChain
        let pubAddr = PublicAddress "Addr"
        let diff = Difficulty 0
        let nonce = Nonce 1
        newBlock <- mineCoinbaseBlock pubAddr diff chain nonce
        let newChain = addBlock newBlock chain
        (currentIndex newChain) `shouldBe` BlockIndex 1

    it "mines block correctly" $ do
        let chain = [] :: BlockChain
        let pubAddr = PublicAddress "Addr"
        let diff = Difficulty 2
        let nonce = Nonce 1
        newBlock <- mineCoinbaseBlock pubAddr diff chain nonce
        let newChain = addBlock newBlock chain
        checkValidHashDifficulty (B.hash newBlock) diff `shouldBe` True

    it "same blocks have same hash" $ do
        let chain = [] :: BlockChain
        let pubAddr = PublicAddress "Addr"
        let diff = Difficulty 0
        let nonce = Nonce 1
        let testTrx = createTestTrx 10
        let testTrx2 = createTestTrx 20
        blocks <- concurrently (mineTrxsBlock pubAddr [testTrx, testTrx2] diff chain nonce) (mineTrxsBlock pubAddr [testTrx, testTrx2] diff chain nonce)
        B.hash (fst blocks) `shouldBe` B.hash (snd blocks)

    it "different blocks have different hash" $ do
        let chain = [] :: BlockChain
        let pubAddr = PublicAddress "Addr"
        let diff = Difficulty 0
        let nonce = Nonce 1
        let testTrx = createTestTrx 10
        let testTrx2 = createTestTrx 20
        let testTrx3 = createTestTrx 21
        blocks <- concurrently (mineTrxsBlock pubAddr [testTrx, testTrx2] diff chain nonce) (mineTrxsBlock pubAddr [testTrx, testTrx3] diff chain nonce)
        B.hash (fst blocks) `shouldNotBe` B.hash (snd blocks)

  describe "MessageHandler" $ do
    it "adds block correctly" $ do
        let chain = [] :: BlockChain
        let pubAddr = PublicAddress "Addr"
        let diff = Difficulty 0
        let nonce = Nonce 1
        newBlock <- mineCoinbaseBlock pubAddr diff chain nonce
        let newChain = addBlock newBlock chain
        (R.length newChain) `shouldBe` 1

    it "mines block correctly" $ do
        let chain = [] :: BlockChain
        let pubAddr = PublicAddress "Addr"
        let diff = Difficulty 2
        let nonce = Nonce 1
        newBlock <- mineCoinbaseBlock pubAddr diff chain nonce
        let newChain = addBlock newBlock chain
        checkValidHashDifficulty (B.hash newBlock) diff `shouldBe` True

  describe "AppState" $ do
    it "adds transaction to pool correctly" $ do
        let testTrx = createTestTrx 11
        appState <- createTestAppState
        addToTrxPool appState testTrx
        newTrxs <- getTrxPool appState
        F.length newTrxs `shouldBe` 1

    it "get size of chain correctly" $ do
        let testTrx = createTestTrx 11
        let chain = [] :: BlockChain
        let pubAddr = PublicAddress "Addr"
        let diff = Difficulty 2
        let nonce = Nonce 1
        appState <- createTestAppState
        newBlock <- mineCoinbaseBlock pubAddr diff chain nonce
        addNewBlock newBlock appState
        length <- getChainSize appState
        length `shouldBe` (BlockIndex 1)
 