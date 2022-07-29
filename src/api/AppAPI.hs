{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AppAPI where
import Transaction 
import Servant
import Block (Block)
import Models (BlockIndex, HashValue)


type TransactionsAPI =
  "trx"
    :> ( 
        "receive" :> ReqBody '[JSON] Transaction :> Post '[JSON] Transaction
       )

type BlockchainAPI =
  "blocks"
    :> ( 
        Capture "index" BlockIndex :> Get '[JSON] Block
        :<|> "length" :> Get '[JSON] Int
        :<|> "hash" :> Capture "hashValue" HashValue :> Get '[JSON] Block
       )

type API = TransactionsAPI :<|> BlockchainAPI

api :: Proxy API
api = Proxy