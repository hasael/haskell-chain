#!/usr/bin/env stack
-- stack --resolver lts-19.6 script
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import RIO 


main :: IO ()
main = void $ concurrently someFunc $ do 
    threadDelay 1000
    clientFunc