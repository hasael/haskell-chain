{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Servant

import Transaction (Transaction)
import AppAPI 
import Control.Monad.Except

server :: (MonadError ServerError m)  =>  ServerT API m
server = transactionServer

transactionServer :: (MonadError ServerError m)  => ServerT TransactionsAPI m
transactionServer = receiveTransaction

receiveTransaction :: (MonadError ServerError m) => Transaction -> m Transaction
receiveTransaction trx = return trx