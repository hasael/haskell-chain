#!/usr/bin/env stack
-- stack --resolver lts-19.6 script
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Main where

import RIO 
import Prelude (print, head, putStrLn)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.Process

main :: IO ()
main =  do
  res <- async $ callCommand "stack run 3" 
  threadDelay 15000000
  manager <- newManager defaultManagerSettings

  request <- parseRequest "http://localhost:3085/blocks/length"
  response <- httpLbs request manager

  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ "Length was" ++ (show (responseBody response))
  res1 <- wait res
  print res1
