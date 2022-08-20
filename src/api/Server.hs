{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Servant
import Transaction (Transaction)
import AppAPI 
import Control.Monad.Except (MonadError, MonadIO(liftIO))
import AppState 
import RIO (MonadReader(ask), IOException, throwIO, try, Exception (toException, fromException))
import Models
import Block (Block)

server :: ServerT API HttpAppHandler
server = transactionServer :<|> blockChainServer

transactionServer :: ServerT TransactionsAPI HttpAppHandler
transactionServer = receiveTransaction

blockChainServer :: ServerT BlockchainAPI HttpAppHandler
blockChainServer = getBlockByIndex :<|> chainLength :<|> getBlockByHash

receiveTransaction :: Transaction -> HttpAppHandler Transaction
receiveTransaction trx = do
    appState <- ask
    liftIO $ addToTrxPool appState trx
    return trx

chainLength :: HttpAppHandler Int
chainLength = do
    appState <- ask
    size <- getChainSize appState
    return $ blockIndex size

getBlockByIndex :: BlockIndex -> HttpAppHandler Block
getBlockByIndex blockIndex = do
    appState <- ask
    getBlock blockIndex appState  >>= notFoundResponse

getBlockByHash :: HashValue -> HttpAppHandler Block
getBlockByHash hash = do
    appState <- ask
    getBlock (BlockIndex 1) appState  >>= notFoundResponse

notFoundResponse :: Maybe a -> HttpAppHandler a
notFoundResponse Nothing = throwError err404
notFoundResponse (Just r) = return r