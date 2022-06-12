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
                startPeer (localPort $ tcpConfig appConfig) (peers $ tcpConfig appConfig) 10000000

