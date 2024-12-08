{-# LANGUAGE FlexibleInstances #-}

module Currycoin.Data.Transaction where

import Currycoin.Data.MerkleTree
import Crypto.Hash.SHA256
import Currycoin.Common
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU

data Transaction = Transaction
                   [TxInput]
                   [(TxOutput, Hash)] -- Technically, in the original bitcoin format,
                   [B.ByteString]     -- There should be only two output:
                                      -- The spend and the change
                                      -- But it wouldn't hurt to do a simple extension here
                   -- Last array is signatures, every input address need to have sigs
                   -- Hash should not take the signatures as signature is on the tx hash
getOutput :: Transaction -> [(TxOutput, Hash)]
getOutput (Transaction _ r _) = r

getTXID :: TxOutput -> Hash -> Hash
getTXID (TxOutput addr amount) inHash = Crypto.Hash.SHA256.hash (B.concat [inHash, BSU.fromString addr, intToByteString $ fromIntegral amount])

instance Hashable Transaction where
    serialize (Transaction inputs outputTuples _) = B.concat (inputs ++ (map snd outputTuples))  -- Signature shoule be added after the hash is computed

instance Hashable (Maybe (MerkleTree Transaction)) where
    serialize (Just x) = serialize x
    serialize Nothing = B.pack [0x0]

countInput :: Transaction -> Int
countInput (Transaction inputs _ _) = length inputs
countOutput :: Transaction -> Int
countOutput (Transaction _ outputs _) = length outputs

countTxMerkleTree :: MerkleTree Transaction -> (Int, Int) -- InputCounter, OutputCounter
countTxMerkleTree txm = accumulator f j txm
                        where
                          f :: Hash -> Transaction -> (Int, Int)
                          f _ tx = ((countInput tx), (countOutput tx))
                          j :: (Int, Int) -> (Int, Int) -> (Int, Int)
                          j (x, y) (z, w) = (x + z, y + w)

amountMatch :: [Int] -> [TxOutput] -> Bool -- Need to retrieve input amount list manually
amountMatch ins outs = fromIntegral (foldr (+) 0 ins) == fromIntegral (foldr f 0 outs)
    where
      f (TxOutput _ amount) acc = acc + amount
