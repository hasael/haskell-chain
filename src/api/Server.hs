{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Servant

import Transaction (Transaction)
import AppAPI 
import Control.Monad.Except
import AppState (AppHandler)

server :: ServerT API AppHandler
server = transactionServer

transactionServer :: ServerT TransactionsAPI AppHandler
transactionServer = receiveTransaction

receiveTransaction :: Transaction -> AppHandler Transaction
receiveTransaction trx = return trx