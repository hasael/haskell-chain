module BlockChain where

import RIO
import Block as B
import TimeService
import Models as M
import HashService
import Wallet
import Transaction (Transaction)

type BlockChain = [Block]

addBlock :: Block -> BlockChain -> BlockChain
addBlock block chain = chain ++ [block]

loadBlocks :: [Block] -> BlockChain -> BlockChain
loadBlocks blocks chain = chain ++ blocks

getLastBlock :: BlockChain -> Block
getLastBlock = last

nextIndex :: BlockChain -> BlockIndex
nextIndex chain = BlockIndex $ (+1) $ length chain

currentIndex :: BlockChain -> BlockIndex
currentIndex chain = BlockIndex $ length chain

mineBlock :: PublicAddress -> Difficulty -> BlockChain -> Nonce -> IO Block
mineBlock publicAddress diff chain nonce = do
    timeStamp <- getTimeStamp
    let lastIdx = nextIndex chain
    let version = BlockVersion 1
    coinbaseTrx <- createCoinbaseTrx publicAddress 10
    let hashValue = calculateHash lastIdx diff timeStamp nonce coinbaseTrx
    let valid = checkValidHashDifficulty hashValue diff 
    if valid then 
        return $ Block lastIdx diff hashValue hashValue (Timestamp timeStamp) nonce [coinbaseTrx] version
    else
        mineBlock publicAddress diff chain (nonce + 1)


calculateHash :: BlockIndex -> Difficulty -> Int64 -> Nonce -> Transaction -> HashValue
calculateHash lastIdx difficulty timeStamp nonce trx = HashValue $  hashContent $ show lastIdx ++ show difficulty ++ show timeStamp ++ show nonce ++ show trx

checkValidHashDifficulty :: HashValue -> Difficulty -> Bool
checkValidHashDifficulty hashValue difficulty = take (M.difficulty difficulty) (M.hashValue hashValue) == replicate (M.difficulty difficulty) '0'