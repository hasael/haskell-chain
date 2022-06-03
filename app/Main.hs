#!/usr/bin/env stack
-- stack --resolver lts-19.6 script
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import RIO 
import Data.Yaml
import System.Environment
import AppConfig
import Prelude (print, head)

main :: IO ()
main = do 
  args <- getArgs
  print $ "Args: " ++ show args
  let initArg = if not $ null args then Just $ head args else Nothing 
  print initArg
  case initArg of
    Just "def" -> start "./config/default.yaml" 
    _ -> start "./config/peer.yaml"

readConfig :: FilePath -> IO AppConfig
readConfig = decodeFileThrow 

start :: FilePath -> IO ()
start config = do
                appConfig <- readConfig config
                startPeer (remotePort $ tcpConfig appConfig) (localPort $ tcpConfig appConfig) 10000000

