{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module AppConfig where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Models
import AppState

data AppConfig = AppConfig
  { tcpConfig :: TCPConfig,
    mineConfig :: MineConfig,
    dbConfig :: DbConfig
  }
  deriving (Show, Generic)

data TCPConfig = TCPConfig
  { localPort :: Int,
    peers :: [Peer]
  }
  deriving (Show, Generic)

data MineConfig = MineConfig {
  mineDifficulty :: Int,
  mineFrequency :: Int
} deriving(Show, Generic)

data DbConfig = DbConfig {
  dbFilePath :: String
} deriving(Show, Generic)

instance FromJSON AppConfig
instance FromJSON TCPConfig
instance FromJSON MineConfig
instance FromJSON DbConfig