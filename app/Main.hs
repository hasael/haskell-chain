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

--main :: IO ()
--main = void $ concurrently someFunc $ do 
--    threadDelay 1000
--    clientFunc

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
                concurrently_ (serveFunc ( show $ localPort $ tcpConfig appConfig)) $ do 
                    threadDelay 10000000
                    clientFunc (show $ remotePort $ tcpConfig appConfig)

