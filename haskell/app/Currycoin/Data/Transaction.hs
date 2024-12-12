{-# LANGUAGE FlexibleInstances #-}

module Currycoin.Data.Transaction where

import Currycoin.Data.MerkleTree
import Crypto.Hash.SHA256
import Crypto.Secp256k1
import Currycoin.Common
import Currycoin.Data.Key
import Data.Maybe (fromJust)
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString)
import Data.List (intersperse)

data UTXO = UTXO TxOutput Hash
    deriving (Eq)

instance {-# OVERLAPS #-} Show UTXO where
    show (UTXO out hash) = show out ++ ", hash: " ++ (byteStringToHex hash)

data Transaction = Transaction
                   [TxInput]
                   [UTXO] -- Technically, in the original bitcoin format,
                   [B.ByteString]     -- There should be only two output:
                                      -- The spend and the change
                                      -- But it wouldn't hurt to do a simple extension here
                   -- Last array is signatures, every input address need to have sigs
                   -- Hash should not take the signatures as signature is on the tx hash
    deriving (Eq)                   
getOutput :: Transaction -> [UTXO]
getOutput (Transaction _ r _) = r

getTXID :: TxOutput -> Hash -> Hash
getTXID (TxOutput addr amount) inHash = Crypto.Hash.SHA256.hash (B.concat [inHash, fromString addr, intToByteString $ fromIntegral amount])

instance Hashable Transaction where
    serialize (Transaction inputs outputTuples _) = B.concat (inputs ++ (map (\(UTXO _ hash) -> hash) outputTuples))  -- Signature shoule be added after the hash is computed

instance Hashable (Maybe (MerkleTree Transaction)) where
    serialize (Just x) = serialize x
    serialize Nothing = B.pack [0x0]

instance {-# OVERLAPS #-} Show [Transaction] where
    show txs = concat (intersperse "\n" (map show txs))

instance Show Transaction where
    show (Transaction ins outs sigs) =
        "Inputs: \n" ++ (inputsToString ins) ++ "\n" ++
        "Outputs: \n" ++ (outputsToString outs) ++ "\n" ++
        "Signatures: \n" ++ (sigsToString sigs) ++ "\n" ++
        "Transaction Hash: " ++ (hashToString (Transaction ins outs sigs)) ++ "\n\n"
        where inputsToString [] = "- No inputs."
              inputsToString ins = concat (intersperse "\n" (map (\h -> "- " ++ byteStringToHex h) ins))
              outputsToString outs = concat (intersperse "\n" (map (\o -> "- " ++ show o) outs))
              sigsToString [] = "- No signatures."
              sigsToString ins = concat (intersperse "\n" (map (\h -> "- " ++ byteStringToHex h) sigs))
              hashToString tx = (byteStringToHex . takeHash) tx

instance Show (MerkleTree Transaction) where
    show t = concat (map show (convertMerkleTreeToList t))
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

verifyTX :: Crypto.Secp256k1.Ctx -> Transaction -> [MyPubKey] -> Bool
verifyTX ctx tx pks =
    verifyTXi tx pks
    where
        m = (fromJust . msg . takeHash) tx
        verifyTXi (Transaction [] outputs []) [] = True
        verifyTXi (Transaction (i:ins) outputs (s:sigs)) (p:pks) =
            verifySig ctx pk si m && verifyTXi (Transaction ins outputs sigs) pks
            where si = Sig s
                  pk = processPubKeyToLib p
