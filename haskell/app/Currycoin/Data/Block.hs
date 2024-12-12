module Currycoin.Data.Block where

import Crypto.Hash.SHA256
import Currycoin.Common
import Currycoin.Data.MerkleTree
import Currycoin.Data.Transaction
import Data.List (isPrefixOf)
import Data.Maybe (fromJust, isNothing)
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (toString, fromString)

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

instance Hashable Block where
    serialize (PrunedBlock a b c _ _) = (hash . B.concat) [a, b, c]
    serialize (FullBlock a b c _ _ _) = (hash . B.concat) [a, b, c]
    
instance Hashable BlockTemplate where
    serialize (BlockTemplate version flag incr oucr coinbase txs additional) =
        B.concat [version,
                  flag,
                  intToByteString (fromIntegral incr),
                  intToByteString (fromIntegral oucr),
                  (serialize coinbase),
                  (serialize txs),
                  additional]

-- Block: BlockTemplate hash without MerkleTree
--        MerkleTree root hash
--        Previous hash
--        Nonce
--        PoW hash
--        BlockTemplate


data Block = PrunedBlock Hash Hash Hash B.ByteString Hash |
             FullBlock   Hash Hash Hash B.ByteString Hash BlockTemplate

pruneBlock :: Block -> Block
pruneBlock (FullBlock a b c d e _) = (PrunedBlock a b c d e)
pruneBlock (PrunedBlock a b c d e) = (PrunedBlock a b c d e)

getCoinbase :: Block -> Maybe Transaction
getCoinbase (PrunedBlock _ _ _ _ _) = Nothing
getCoinbase (FullBlock _ _ _ _ _ (BlockTemplate _ _ _ _ coinbase _ _)) = Just coinbase

getTxTree :: Block -> Maybe (MerkleTree Transaction)
getTxTree (FullBlock _ _ _ _ _ (BlockTemplate _ _ _ _ _ (Just mt) _)) = Just mt
getTxTree (PrunedBlock _ _ _ _ _) = Nothing

findTX :: Block -> Hash -> Maybe Transaction
findTX (PrunedBlock _ _ _ _ _) _ = Nothing
findTX (FullBlock _ _ _ _ _ (BlockTemplate _ _ _ _ _ Nothing _)) _ = Nothing
findTX (FullBlock _ _ _ _ _ (BlockTemplate _ _ _ _ _ (Just mt) _)) hash =
    lookup (convertMerkleTreeToList mt)
    where
        lookup [] = Nothing
        lookup (t:txs) | hash == (takeHash t) = Just t
                       | otherwise = lookup txs

instance Show Block where
    show (PrunedBlock templateHash rootHash prevHash nonce powHash) =
        "\ESC[36mPruned block, Block hash\ESC[0m\n\n" ++
        "Block Template Hash:\t\t" ++ (byteStringToHex templateHash) ++ "\n" ++
        "Block Root Hash:\t\t" ++ (byteStringToHex rootHash) ++ "\n" ++
        "Included previous hash:\t\t" ++ (byteStringToHex prevHash) ++ "\n" ++
        "Block POW Hash:\t\t\t" ++ (byteStringToHex powHash) ++ "\n" ++
        "Block Nonce:\t\t\t\t" ++ (toString nonce) ++ "\n" ++
        "Final Block Hash for chaining:\t" ++ byteStringToHex (takeHash (PrunedBlock templateHash rootHash prevHash nonce powHash)) ++ "\n"
    show (FullBlock   templateHash rootHash prevHash nonce powHash (BlockTemplate version flag incr oucr coinbase txs additional)) =
        "\ESC[36mLocally Stored Block\ESC[0m\n" ++
        "Block Template Hash:\t\t" ++ (byteStringToHex templateHash) ++ "\n" ++
        "Block Root Hash:\t\t" ++ (byteStringToHex rootHash) ++ "\n" ++
        "Included previous hash:\t\t" ++ (byteStringToHex prevHash) ++ "\n" ++
        "Block POW Hash:\t\t\t" ++ (byteStringToHex powHash) ++ "\n" ++
        "Final Block Hash for chaining:\t" ++ byteStringToHex (takeHash (FullBlock   templateHash rootHash prevHash nonce powHash (BlockTemplate version flag incr oucr coinbase txs additional))) ++ "\n" ++
        "Block Nonce:\t\t\t" ++ (toString nonce) ++ "\n" ++
        "Block Version:\t\t" ++ (byteStringToHex version) ++ "\n" ++
        "Block Flags:\t\t" ++ (lastFourBinaryDigits flag) ++ "\n" ++
        "Block Input Counter:\t" ++ (show incr) ++ "\n" ++
        "Block Output Counter:\t" ++ (show oucr) ++ "\n" ++
        "\ESC[36mTransactions\ESC[0m\n" ++
        "\ESC[36mCoinbase:\ESC[0m\n" ++ (show coinbase) ++ "\n" ++
        "\ESC[36mRegular:\ESC[0m\n" ++ (show txs) ++ "\n" ++
        "\ESC[36mAdditional Data\ESC[0m\n" ++
        "Hex form: " ++ (byteStringToHex additional) ++ "\nConverted: \n" ++
        (toString additional)
        
-- Block is either its merkle root(pruned), or fully stored with its version, flag and transactions
difficulty :: Integer -> Integer
difficulty height = min 10 (floor (logBase 4 (fromIntegral height)) + 2)

mining :: Hash -> Integer -> Integer -> (Hash, Integer)
mining target nonce diff =
  (if (verifyDiff currentHash diff)
    then (currentHash, nonce)
    else (mining target (nonce + 1) diff))
  where
    currentHash = hash (B.concat [target, intToByteString (fromIntegral nonce)])

mintBlock :: Version -> Flag -> String -> (Maybe (MerkleTree Transaction)) -> Integer -> Hash -> B.ByteString -> Block -- Version, Flag, Address, tx to be included, height(for diff and coinbase), previous hash additional data
mintBlock v f addr txs height prevHash additional =
    (FullBlock templateHash rootHash prevHash nonce powHash template)
    where
      amountTuple = (if (isNothing txs) then (0, 0) else (countTxMerkleTree (fromJust txs)))
      coinbaseOutput = TxOutput addr 100
      coinbase = Transaction [] [(UTXO coinbaseOutput (getTXID coinbaseOutput (intToByteString $ fromInteger height)))] []
      template = (BlockTemplate v f (fromIntegral (fst amountTuple)) (fromIntegral (snd amountTuple)) coinbase txs additional)
      templateHash = takeHash template
      rootHash = (if (isNothing txs) then hashEmptyTree else (takeHash (fromJust txs)))
      miningHash = (hash . B.concat) [templateHash, rootHash, prevHash]
      powResult = mining miningHash 0 (difficulty $ fromIntegral height)
      powHash = (fst powResult)
      nonce = (intToByteString . fromIntegral . snd) powResult
      
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
          genesisTX = Transaction [] [(UTXO genesisOutput (getTXID genesisOutput (B.pack [0x0])))] [] -- No sig for coinbase
          template = BlockTemplate (B.pack [0x1]) flagConst 0 1 genesisTX Nothing (fromString "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks") 
          templateHash = takeHash template
          rootHash = hashEmptyTree
          prevHash = hashEmptyTree
          miningHash = (hash . B.concat) [templateHash, rootHash, prevHash]
          genesisBlockTuple = (mining miningHash 0 (difficulty 1))
          nonce = (intToByteString . fromIntegral . snd) genesisBlockTuple
          powHash = (fst genesisBlockTuple)
