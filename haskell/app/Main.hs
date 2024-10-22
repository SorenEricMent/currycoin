module Main where

import Lib
import Crypto.Hash.SHA256
import qualified Data.ByteString as B

type Hash = B.ByteString
type Version = B.ByteString
type Flag = B.ByteString    -- Constant of 0000 because we don't support SegWit
type InCounter = Integer
type OutCounter = Integer
type Amount = Integer

class Hashable a where
    hash :: a -> Hash

data TxInput = TxInput Hash    -- Previous spendable output
               
data TxOutput = TxOutput Hash Amount

data Transaction = Transaction
                   Version
                   Flag
                   InCounter
                   [TxInput]
                   OutCounter
                   [TxOutput]


data MerkleTree a = INode Hash (MerkleTree a) (MerkleTree a) |
                    LeafNode Hash a


main :: IO ()
main = someFunc
