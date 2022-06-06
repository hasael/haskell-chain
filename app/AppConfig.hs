{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module AppConfig where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Models
import AppState

newtype AppConfig = AppConfig
  { tcpConfig :: TCPConfig
  }
  deriving (Show, Generic)

data TCPConfig = TCPConfig
  { localPort :: Int,
    peers :: [Peer]
  }
  deriving (Show, Generic)

instance FromJSON AppConfig

instance FromJSON TCPConfig