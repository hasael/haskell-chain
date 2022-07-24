{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AppAPI where
import Transaction 
import Servant


type TransactionsAPI =
  "trx"
    :> ( 
        "receive" :> ReqBody '[JSON] Transaction :> Post '[JSON] Transaction
       )
type API = TransactionsAPI

api :: Proxy API
api = Proxy