{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Main where

import Lib
import Crypto.Hash.SHA256
import qualified Data.ByteString as B
import Crypto.Hash.RIPEMD160
import System.Entropy
import Crypto.Secp256k1
import Crypto.Secp256k1.Internal.Base
import Data.ByteString.Base58
import Data.Char (digitToInt)
import Numeric (readHex)
import Data.Map (Map)
import qualified Data.Map as Map
-- https://stackoverflow.com/a/40491001
import qualified Data.ByteString.UTF8 as BSU

import Data.Word (Word8)
import Numeric (showHex)
import Data.List (intercalate)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import System.IO (hFlush, stdout)
import System.Console.Haskeline
import Data.Maybe (listToMaybe)

type Hash = B.ByteString
type Version = B.ByteString
type Flag = B.ByteString
type InCounter = Integer
type OutCounter = Integer
type Amount = Integer

data GlobalState = GlobalState {
    -- Todo
    block :: [Block],
    txPool :: [Transaction],
    utxo :: [(Hash, Integer)]
}

-- The Times 03/Jan/2009 Chancellor on brink of second bailout for banks
initialState = GlobalState {
    block = [(generateGenesis)],
    txPool = [],
    utxo = []
}

generateGenesis :: Block
generateGenesis =
        (FullBlock blockHash (B.pack [0x0]) (mining blockHash 0 0) template)
        where
          template = BlockTemplate (B.pack [0x1]) flagConst 0 0 [] (BSU.fromString "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks")
          blockHash = (Crypto.Hash.SHA256.hash (serialize template))

type AppState = StateT GlobalState IO
-- Constant section
flagConst = B.pack [0, 0, 0, 0] -- Constant of 0000 because we don't support SegWit

-- Helper function section
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise = listToMaybe (drop n xs)
-- Provided by haskell-base >= 4.20, can't get it installed on Archlinux

-- Function to convert a hex string to a ByteString
hexToByteString :: String -> B.ByteString
hexToByteString hexStr
  | odd (length hexStr) = error "Invalid hex string: odd length"
  | otherwise = B.pack $ map (fst . head . readHex) (chunksOf 2 hexStr)

-- Helper function to split a string into chunks of the given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
byteStringToHex :: B.ByteString -> String
byteStringToHex bs = intercalate "" $ map word8ToHex (B.unpack bs)
word8ToHex :: Word8 -> String
word8ToHex w = let hex = showHex w ""
	       in if Prelude.length hex == 1 then '0' : hex else hex
	       -- About helper is by ChatGPT, to create human readable hexdigit strings for better visuals.
	       -- Note: Do NOT use those two function internally beside formatting output! All internal structures should be in ByteString

-- Class definition section
class Hashable a where
    serialize :: a -> B.ByteString
    takeHash :: a -> Hash
    takeHash = Crypto.Hash.SHA256.hash . serialize

data TxInput = TxInput Hash    -- Previous spendable output

data TxOutput = TxOutput Hash Amount

data Transaction = Transaction
		   [TxInput]
		   [TxOutput]	 -- Technically, in the original bitcoin format,
				 -- There should be only two output:
				 -- The spend and the change
				 -- But it wouldn't hurt to do a simple extension here
-- Block definition
data BlockTemplate = BlockTemplate Version Flag InCounter OutCounter [Transaction] B.ByteString -- AdditionalData
data Block = PrunedBlock Hash B.ByteString Hash | -- BlockHash(MerkleRoot) Nonce PoWHash
	     FullBlock Hash B.ByteString Hash BlockTemplate

instance Hashable BlockTemplate where
    serialize (BlockTemplate _ _ _ _ _ _) = undefined

instance Show Block where
    show (PrunedBlock root nonce pow) = "Pruned block, Block hash"
    show (FullBlock root nonce pow (BlockTemplate version flag incr oucr [tx] additional)) = "Locally Stored Block"
-- Block is either its merkle root(pruned), or fully stored with its version, flag and transactions
difficulty :: Integer -> Integer
difficulty height = floor (logBase 8 (fromIntegral height))

mining :: Hash -> Integer -> Integer-> Hash
mining target nonce difficulty = undefined

-- We use a simple algorithm for block PoW difficulty(instead of Bitcoin considering 2 week average):
-- Per 8^n block, increase difficulty requirement by requesting one more head zero
-- e.g. at the 1-8 block, require zero difficulty
-- at 9-64 block, require one zero at the head
-- Max difficulty is 16 zeros

-- https://wiki.haskell.org/Data_declaration_with_constraint
-- https://wiki.haskell.org/Generalised_algebraic_datatype
-- https://stackoverflow.com/a/40825913: Not a good idea?
data MerkleTree a where
    INode :: Hash -> MerkleTree a -> MerkleTree a -> MerkleTree a
    LeafNode :: Hashable a => Hash -> a -> MerkleTree a

instance Show (MerkleTree String) where
    show = drawMerkleTree

-- https://hackage.haskell.org/package/containers-0.5.7.1/docs/src/Data.Tree.html#drawTree
-- Had to limit it to MerkleTree String instead of MerkleTree a here
drawMerkleTree :: MerkleTree String -> String
drawMerkleTree	= unlines . drawMT
drawMT :: MerkleTree String -> [String]
drawMT (LeafNode h a) = [show a] ++ [" \\ "] ++ [byteStringToHex h]
drawMT (INode h l r) = ("" ++ byteStringToHex h) : drawSubTrees [l, r]
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
	"|" : shift "`- " "   " (drawMT t)
    drawSubTrees (t:ts) =
	"|" : shift "+- " "|  " (drawMT t) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)

-- 'MerkleTree a' requires 'FlexibleInstances' extension
-- See https://stackoverflow.com/a/25768967
instance Hashable (MerkleTree a) where
    serialize (INode h a b) = B.append (takeHash a) (takeHash b)
    -- Not possible? Without GADT
    serialize (LeafNode h a) = serialize a
    takeHash (INode h a b) = h
    takeHash (LeafNode h a) = h

instance Hashable String where
    serialize = BSU.fromString

instance Hashable B.ByteString where
    serialize a = a

-- | Construct a Merkle Tree from a list of hashes.
-- | [1] => 1|1
-- |	   1   1
-- | [1, 2] => 1|2
-- |	      1	  2
-- | [1, 2, 3] => 1|2|3|3
-- |		1|2	3|3
-- |	       1   2   3dup3
-- | [1, 2, 3, 4] => 1|2|3|4
-- |		   1|2	   3|4
-- |		  1   2	  3   4
-- | [1, 2, 3, 4, 5] => 1|2|3|4|5|5|5|5
-- |		  1|2|3|4	      5|5|5|5
-- |		1|2	3|4	    5|5 dup 5|5
-- |	       1   2   3   4	   5   5
-- | [1, 2, 3, 4, 5, 6] => 1|2|3|4|5|6|5|6
-- |		     1|2|3|4		 5|6|5|6
-- |		   1|2	   3|4	       5|6 dup 5|6
-- |		  1   2	  3   4	      5	  6
-- | [1, 2, 3, 4, 5, 6, 7] => 1|2|3|4|5|6|7|7
-- |			1|2|3|4		    5|6|7|7
-- |		      1|2     3|4	  5|6	  7|7
-- |		     1	 2   3	 4	 5   6	 7dup7
-- | [1, 2, 3, 4, 5, 6, 7, 8] => 1|2|3|4|5|6|7|8
-- |			   1|2|3|4	       5|6|7|8
-- |			 1|2	 3|4	     5|6     7|8
-- |			1   2	3   4	    5	6   7	8

groupBy2 :: [a] -> [(a, a)]
groupBy2 [] = []
groupBy2 [a] = [(a, a)]
groupBy2 [a, b] = [(a, b)]
groupBy2 (a:b:xs) = (a, b) : groupBy2 xs

createMerkleTreeFromListInternal :: [MerkleTree a] -> MerkleTree a
createMerkleTreeFromListInternal [a] = a
createMerkleTreeFromListInternal [a, b] = INode h a b
    where h = takeHash (B.append (takeHash a) (takeHash b))
createMerkleTreeFromListInternal as = w
    where w = createMerkleTreeFromListInternal p
	  p = map (\x -> INode (takeHash (B.append (takeHash (fst x)) (takeHash (snd x)))) (fst x) (snd x)) g
	  g = groupBy2 as

createMerkleTreeFromList :: (Hashable a) => [a] -> MerkleTree a
createMerkleTreeFromList = createMerkleTreeFromListInternal . map (\x -> LeafNode (takeHash x) x)

-- Manually verify the integrity of a MerkleTree
verifyMerkleTree :: (Hashable a) =>
		    MerkleTree a ->
		    Bool

verifyMerkleTree (LeafNode nodeHash dataVal) =
        nodeHash == takeHash dataVal -- leaf node hash should be the hash of the data

verifyMerkleTree (INode nodeHash left right) =
        -- inode hash should be the hash of the concatenation of the left and right children
        -- and the left and right children should also be valid Merkle trees
        nodeHash == takeHash (B.append (takeHash left) (takeHash right)) &&
        verifyMerkleTree left && verifyMerkleTree right

-- Credit: ChatGPT added skewness in generateInclusionProof and proveHashableInclusion for consistent order
generateInclusionProof :: (Hashable a) =>
			  MerkleTree a ->
			  a ->		  -- Element to prove inclusion
			  Maybe [(Bool, Hash)]	-- (IsRightSibling, SiblingHash)
generateInclusionProof (LeafNode hsh hble) y =
    if (takeHash y) == hsh
    then Just []  -- Element found, return empty proof path
    else Nothing  -- Element not found

generateInclusionProof (INode hsh lft rht) y =
    case generateInclusionProof lft y of
	Just path -> Just (path ++ [(True, takeHash rht)])  -- Right sibling
	Nothing -> case generateInclusionProof rht y of
	    Just path -> Just (path ++ [(False, takeHash lft)])	 -- Left sibling
	    Nothing -> Nothing

-- Verify the inclusion proof
-- (Bool, Hash): Bool = True if the sibling is on the right, False if on the left
proveHashableInclusion :: (Hashable a) =>
			  Hash ->	-- Root hash
			  a ->		-- Element to prove
			  [(Bool, Hash)] ->  -- Sibling hashes with left/right info
			  Bool
proveHashableInclusion rootHash y proofPath =
    let initialHash = takeHash y
	finalHash = foldl combineHashes initialHash proofPath
    in finalHash == rootHash
  where
    -- Combine the current hash with the sibling hash based on sibling position
    combineHashes acc (isRight, siblingHash) =
	if isRight
	then takeHash (B.append acc siblingHash)  -- Right sibling: append current hash first
	else takeHash (B.append siblingHash acc)  -- Left sibling: append sibling hash first


addToMerkleTree :: (Hashable a) => (MerkleTree a) -> a -> (MerkleTree a)
addToMerkleTree originalTree newElement =
    -- take original merkle tree and new element and create a new merkle tree with the new element added
    let newLeaf = LeafNode (takeHash newElement) newElement
    in createMerkleTreeFromListInternal [originalTree, newLeaf]

-- Example section
data MerkleTreeExample = MerkleTreeExample B.ByteString

appendPubPrefix :: PubKey -> BSU.ByteString
appendPubPrefix (PubKey bs) = B.cons 0x4 bs

pubkeyToAddress :: PubKey -> String
pubkeyToAddress pubkey =
    show ((encodeBase58 bitcoinAlphabet (addChecksum (B.append (B.pack [0x00]) (Crypto.Hash.RIPEMD160.hash (Crypto.Hash.SHA256.hash (appendPubPrefix pubkey)))))))
    where
      addChecksum :: B.ByteString -> B.ByteString
      addChecksum bs = B.append bs (B.take 4 (Crypto.Hash.SHA256.hash (Crypto.Hash.SHA256.hash bs)))


shell :: AppState ()
shell = do
    currentState <- get
    liftIO $ runInputT defaultSettings $ do
	outputStr "CurryCoin> "
	input <- getInputLine ""
	ctx <- liftIO Crypto.Secp256k1.createContext
	case input of
	    Just "exit" -> do
		liftIO $ putStrLn "Exiting..."
	    Just "init" -> do
		liftIO $ putStrLn "Reset all state to initial"
		liftIO $ evalStateT shell initialState
	    Just "help" -> do
		liftIO $ putStrLn "exit, help, new_address"
		liftIO $ evalStateT shell currentState
	    Just "new_address" -> do
		private_key <- liftIO $ (Crypto.Hash.SHA256.hash <$> getEntropy 32)
		let pubKey = derivePubKey ctx (Crypto.Secp256k1.SecKey private_key)
		liftIO $ putStrLn $ "New public key: " ++ (byteStringToHex . appendPubPrefix) pubKey
		liftIO $ putStrLn $ "New public address: " ++ (pubkeyToAddress pubKey)
		liftIO $ putStrLn $ "New private key: " ++ (show . byteStringToHex) private_key
		liftIO $ evalStateT shell currentState
	    Just "height" -> do
		case (block currentState)!?0 of
		    Just n -> do
			liftIO $ putStrLn $ show $ length $ block currentState
		    Nothing -> do
			liftIO $ putStrLn "No block exists in the database."
		liftIO $ evalStateT shell currentState
	    Just "transact" -> do
		liftIO $ putStrLn "placehold"
		liftIO $ evalStateT shell currentState
	    Just "show_tx_pool" -> do
		liftIO $ putStrLn "placehold"
		liftIO $ evalStateT shell currentState
	    Just "show_tx" -> do
		liftIO $ putStrLn "placehold"
		liftIO $ evalStateT shell currentState
	    Just "show_utxo" -> do
		liftIO $ putStrLn "placehold"
		liftIO $ evalStateT shell currentState
	    Just "show_utxo_addr" -> do
		liftIO $ putStrLn "placehold"
		liftIO $ evalStateT shell currentState
	    Just "mint_block" -> do
		liftIO $ putStrLn "placehold"
		liftIO $ evalStateT shell currentState
	    Just _	-> do
		liftIO $ putStrLn "Unknown command"
		liftIO $ evalStateT shell currentState	-- Continue shell
	    Nothing -> return ()  -- If no input is provided
    -- Credit: ChatGPT replaced readline with haskeline

main :: IO ()
main = evalStateT shell initialState
-- Origial test by ChatGPT (modified)
testMerkleTree :: [String] -> String -> IO ()
testMerkleTree elements elementToProve = do
    let merkleTree = createMerkleTreeFromList elements
    putStrLn "Merkle Tree Structure:"
    print merkleTree

    let inclusionProof = generateInclusionProof merkleTree elementToProve
    case inclusionProof of
	Nothing -> putStrLn "Element not found in the tree."
	Just proofPath -> do
	    putStrLn $ "Inclusion Proof for element '" ++ elementToProve ++ "':"
	    mapM_ (\(isRight, hash) -> putStrLn $ (if isRight then "Right: " else "Left:  ") ++ byteStringToHex hash) proofPath
	    let elementToProveBS = BSU.fromString elementToProve
	    let rootHash = takeHash merkleTree
	    let isValid = proveHashableInclusion rootHash elementToProveBS proofPath
	    putStrLn $ "Proof is valid: " ++ show isValid
