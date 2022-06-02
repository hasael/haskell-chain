{-# LANGUAGE DeriveGeneric #-}

module AppConfig where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

newtype AppConfig = AppConfig
  { tcpConfig :: TCPConfig
  }
  deriving (Show, Generic)

data TCPConfig = TCPConfig
  { remotePort :: Int,
    localPort :: Int
  }
  deriving (Show, Generic)

instance FromJSON AppConfig

instance FromJSON TCPConfig
