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

main :: IO ()
main = do 
  args <- getArgs
  print $ "Args: " ++ show args
  let initArg = if not $ null args then Just $ head args else Nothing 
  print initArg
  case initArg of
    Just "1" -> start "./config/peer1.yaml" 
    Just "2" -> start "./config/peer2.yaml" 
    _ -> start "./config/default.yaml"

readConfig :: FilePath -> IO AppConfig
readConfig = decodeFileThrow 

start :: FilePath -> IO ()
start config = do
                appConfig <- readConfig config
                let p = peers $ tcpConfig appConfig 
                let ips = getIpAddr . ipAddress  <$> p
                let ports = getPort. peerPort  <$> p
                startPeer (mineFrequency $ mineConfig appConfig) (mineDifficulty $ mineConfig appConfig) (localPort $ tcpConfig appConfig) (zip ips ports) (dbFilePath $ dbConfig appConfig) 10000000

