{-# LANGUAGE OverloadedStrings #-}
module Blockchain where

import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Maybe(fromMaybe)

type Index = Int

data Transaction = Transaction { sender :: B.ByteString
                               , recipient :: B.ByteString
                               , text :: B.ByteString } deriving Show

data Block = Block  { index :: Index
                    , timestamp :: POSIXTime
                    , transactions :: [Transaction]
                    , proof :: Int
                    , previousHash :: B.ByteString} deriving Show


data Blockchain = Blockchain  { chain :: [Block]
                              , currentTransactions :: [Transaction]}

genesis = addBlock (Blockchain [] []) 1 (Just B.empty)

hash :: Block -> B.ByteString
hash block = SHA256.hash (BC.pack (show block))

addBlock :: Blockchain -> Int -> Maybe B.ByteString -> POSIXTime -> Blockchain
addBlock (Blockchain chain currentTransactions) proof previousHash time = result
  where
    nb = Block  { index = length currentTransactions + 1
                , timestamp = time
                , transactions = currentTransactions
                , proof = proof
                , previousHash =  fromMaybe (hash (head chain)) previousHash }
    result = Blockchain (nb:chain) []

addTransaction :: Blockchain -> Transaction -> (Index, Blockchain)
addTransaction (Blockchain chain currentTransactions) transaction = result
  where
    result = (index (head chain), Blockchain chain (transaction:currentTransactions))

lastBlock :: Blockchain -> Block
lastBlock (Blockchain (block:_) _ ) = block

