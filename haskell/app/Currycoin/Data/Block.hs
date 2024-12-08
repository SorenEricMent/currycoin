module Currycoin.Data.Block where

import Crypto.Hash.SHA256
import Currycoin.Common
import Currycoin.Data.MerkleTree
import Currycoin.Data.Transaction
import Data.List (isPrefixOf)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU

type Version = B.ByteString
type Flag = B.ByteString
type InCounter = Integer
type OutCounter = Integer

hashEmptyTree :: Hash
hashEmptyTree = B.pack (replicate 32 0)

-- Constant section
flagConst = B.pack [0, 0, 0, 0] -- Constant of 0000 because we don't support SegWit
-- Block definition
data BlockTemplate = BlockTemplate Version
                                   Flag
                                   InCounter
                                   OutCounter
                                   Transaction -- coinBase
                                   (Maybe (MerkleTree Transaction))
                                   -- AdditionalData, Hash is previous hash
                                   B.ByteString

instance Hashable BlockTemplate where
    serialize (BlockTemplate version flag incr oucr coinbase txs additional) =
        B.concat [version,
                  flag,
                  intToByteString (fromIntegral incr),
                  intToByteString (fromIntegral oucr),
                  (serialize coinbase),
                  -- No MerkleTree hash
                  additional]

-- Block: BlockTemplate hash without MerkleTree
--        MerkleTree root hash
--        Previous hash
--        Nonce
--        PoW hash
--        BlockTemplate


data Block = PrunedBlock Hash Hash Hash B.ByteString Hash |
             FullBlock   Hash Hash Hash B.ByteString Hash BlockTemplate

getCoinbase :: Block -> Maybe Transaction
getCoinbase (PrunedBlock _ _ _ _ _) = Nothing
getCoinbase (FullBlock _ _ _ _ _ (BlockTemplate _ _ _ _ coinbase _ _)) = Just coinbase

instance Show Block where
    show (PrunedBlock templateHash rootHash prevHash nonce powHash) = "Pruned block, Block hash"
    show (FullBlock   templateHash rootHash prevHash nonce powHash (BlockTemplate version flag incr oucr coinbase txs additional)) =
        "Locally Stored Block\n" ++
        "Block Template Hash:\t" ++ (byteStringToHex templateHash) ++ "\n" ++
        "Block Root Hash:\t" ++ (byteStringToHex rootHash) ++ "\n" ++
        "Included previous hash:\t" ++ (byteStringToHex prevHash) ++ "\n" ++
        "Block POW Hash:\t\t" ++ (byteStringToHex powHash) ++ "\n" ++
        "Block Nonce:\t\t" ++ (show nonce) ++ "\n"
        
-- Block is either its merkle root(pruned), or fully stored with its version, flag and transactions
difficulty :: Integer -> Integer
difficulty height = floor (logBase 8 (fromIntegral height)) + 2

mining :: Hash -> Integer -> Integer -> (Hash, Integer)
mining target nonce diff =
  (if (verifyDiff currentHash diff)
    then (currentHash, nonce)
    else (mining target (nonce + 1) diff))
  where
    currentHash = hash (B.concat [target, intToByteString (fromIntegral nonce)])

mintBlock :: Version -> Flag -> String -> (Maybe (MerkleTree Transaction)) -> Integer -> Hash -> BSU.ByteString -> Block -- Address, tx to be included, diff, previous hash
mintBlock v f addr txs diff prevhash additional = undefined

verifyPOW :: Hash -> Hash -> Integer -> Bool -- Does not check difficulty
verifyPOW target result nonce = result == (Crypto.Hash.SHA256.hash (B.concat [target, intToByteString (fromIntegral nonce)]))
-- e.g. verifyPOW (hexToByteString "1a2b3d") (hexToByteString "0538c0b1223aad095feefecb31ebb517fd2f730e97a3cfbf01c3d235a3edb387") 9 -> True

verifyDiff :: Hash -> Integer -> Bool
verifyDiff target diff = (Data.List.isPrefixOf (replicate (fromIntegral diff) '0') (byteStringToHex target))

-- We use a simple algorithm for block PoW difficulty(instead of Bitcoin considering 2 week average):
-- Per 8^n block, increase difficulty requirement by requesting one more head zero
-- e.g. at the 1-8 block, require zero difficulty
-- at 9-64 block, require one zero at the head
-- Max difficulty is 16 zeros

generateGenesis :: Block
generateGenesis =
        (FullBlock templateHash
                   rootHash
                   prevHash
                   nonce
                   powHash
                   template)
        where
          genesisOutput = (TxOutput "1Curry58bkekKypHUv6wm82XDqnNzgsZNy" 100)
          genesisTX = Transaction [genesisCoinbase] [(genesisOutput, getTXID genesisOutput genesisCoinbase)] [] -- No sig for coinbase
          genesisCoinbase = (B.pack [0x0])
          template = BlockTemplate (B.pack [0x1]) flagConst 0 0 genesisTX Nothing (BSU.fromString "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks")
          templateHash = takeHash template
          rootHash = hashEmptyTree
          prevHash = hashEmptyTree
          miningHash = (hash . B.concat) [templateHash, rootHash, prevHash]
          genesisBlockTuple = (mining miningHash 0 (difficulty 1))
          nonce = (intToByteString . fromIntegral . snd) genesisBlockTuple
          powHash = (fst genesisBlockTuple)
