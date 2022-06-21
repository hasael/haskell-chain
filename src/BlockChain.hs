module BlockChain where

import Block
import TimeService
import Models
import HashService

type BlockChain = [Block]

addBlock :: Block -> BlockChain -> BlockChain
addBlock block chain = chain ++ [block]

getLastBlock :: BlockChain -> Block
getLastBlock = last

nextIndex :: BlockChain -> Int
nextIndex chain = (+1) $ length chain

mineBlock :: Int -> BlockChain -> IO Block
mineBlock difficulty chain = do
    timeStamp <- getTimeStamp
    let lastIdx = nextIndex chain
    let nonce = 1
    let version = 1
    let hashValue = hashContent $ show lastIdx ++ show difficulty ++ show timeStamp ++ show nonce
    return $ Block lastIdx difficulty (HashValue hashValue) (HashValue hashValue) (Timestamp timeStamp) nonce [] version
