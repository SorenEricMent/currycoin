module Main where

import Lib
import Crypto.Hash.SHA256
import qualified Data.ByteString as B
import System.Entropy (getEntropy)
import Crypto.Secp256k1

import Data.Word (Word8)
import Numeric (showHex)
import Data.List (intercalate)


type Hash = B.ByteString
type Version = B.ByteString
type Flag = B.ByteString    
type InCounter = Integer
type OutCounter = Integer
type Amount = Integer

-- Constant section
flagConst = B.pack [0, 0, 0, 1] -- Constant of 0000 because we don't support SegWit

-- Helper function section
byteStringToHex :: B.ByteString -> String
byteStringToHex bs = intercalate "" $ map word8ToHex (B.unpack bs)
word8ToHex :: Word8 -> String
word8ToHex w = let hex = showHex w ""
               in if length hex == 1 then '0' : hex else hex
	       -- About helper is by ChatGPT, to create human readable hexdigit strings for better visuals.
	       -- Note: Do NOT use those two function internally beside formatting output! All internal structures should be in ByteString
	       
-- Class definition section
class Hashable a where
    hash :: a -> Hash

instance Hashable B.ByteString where
    hash = Crypto.Hash.SHA256.hash

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

createMerkleTreeFromList :: (Hashable a) => [a] -> a
    -- INode construct

createMerkleTreeFromList [] = undefined
    -- Leaf node construct
createMerkleTreeFromList (x:y:xs) = undefined
createMerkleTreeFromList (x:xs) = undefined    -- Need to duplicate tx


addToMerkleTree :: (Hashable a) => (MerkleTree a) -> a -> (MerkleTree a)
addToMerkleTree = undefined

-- Example section
data MerkleTreeExample = MerkleTreeExample B.ByteString


main :: IO ()
main = someFunc
