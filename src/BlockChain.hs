module BlockChain where

import RIO
import Block
import TimeService
import Models as M
import HashService

type BlockChain = [Block]

addBlock :: Block -> BlockChain -> BlockChain
addBlock block chain = chain ++ [block]

getLastBlock :: BlockChain -> Block
getLastBlock = last

nextIndex :: BlockChain -> Int
nextIndex chain = (+1) $ length chain

mineBlock :: Int -> BlockChain -> Int -> IO Block
mineBlock difficulty chain nonce = do
    timeStamp <- getTimeStamp
    let lastIdx = nextIndex chain
    let version = 1
    let hashValue = calculateHash lastIdx difficulty timeStamp nonce
    let valid = checkValidHashDifficulty hashValue difficulty 
    if valid then 
        return $ Block lastIdx difficulty hashValue hashValue (Timestamp timeStamp) nonce [] version
    else
        mineBlock difficulty chain (nonce + 1)


calculateHash :: Int -> Int -> Int64 -> Int -> HashValue
calculateHash lastIdx difficulty timeStamp nonce = HashValue $  hashContent $ show lastIdx ++ show difficulty ++ show timeStamp ++ show nonce

checkValidHashDifficulty :: HashValue -> Int -> Bool
checkValidHashDifficulty hashValue difficulty = take difficulty (M.hashValue hashValue) == replicate difficulty '0'