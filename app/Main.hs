#!/usr/bin/env stack
-- stack --resolver lts-19.6 script
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Main where

import Lib
import RIO 
import Data.Yaml
import System.Environment
import AppConfig
import Prelude (print, head)
import Models (IPAddress(getIpAddr), Port (getPort))
import Data.Yaml.Builder (toByteString)

main :: IO ()
main = do 
  args <- getArgs
  print $ "Args: " ++ show args
  let initArg = if not $ null args then Just $ head args else Nothing 
  print initArg
  case initArg of
    Just "1" -> readConfig "./config/peer1.yaml" >>= start
    Just "2" -> readConfig "./config/peer2.yaml" >>= start
    Just "3" -> readConfig "./config/test.yaml" >>= start
    _ -> getConfigFromEnv >>= start 
    --_ -> readConfig "./config/default.yaml" >>= start

readConfig :: FilePath -> IO AppConfig
readConfig = decodeFileThrow 

getConfigFromEnv:: IO AppConfig
getConfigFromEnv = do
   envValue <- getEnv "CHAIN_CONFIG" 
   decodeThrow $ fromString envValue

start :: AppConfig -> IO ()
start appConfig = do
                let p = peers $ tcpConfig appConfig 
                let ips = getIpAddr . ipAddress  <$> p
                let ports = getPort. peerPort  <$> p
                startPeer (mineFrequency $ mineConfig appConfig) (mineDifficulty $ mineConfig appConfig) (localPort $ tcpConfig appConfig) (zip ips ports) (dbFilePath $ dbConfig appConfig) (pingPeersFrequency $ pollPeersConfig appConfig) (checkPeersBlocksFrequency $ pollPeersConfig appConfig) (serverPort $ httpConfig appConfig)

