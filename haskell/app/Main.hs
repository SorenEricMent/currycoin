{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Main where

import Lib
import Crypto.Hash.SHA256
import qualified Data.ByteString as B
import System.Entropy (getEntropy)
import Crypto.Secp256k1
-- https://stackoverflow.com/a/40491001
import qualified Data.ByteString.UTF8 as BSU

import Data.Word (Word8)
import Numeric (showHex)
import Data.List (intercalate)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import System.IO (hFlush, stdout)
import System.Console.Haskeline

type Hash = B.ByteString
type Version = B.ByteString
type Flag = B.ByteString    
type InCounter = Integer
type OutCounter = Integer
type Amount = Integer

data GlobalState = GlobalState {
    -- Todo
    txPool :: [Transaction]
    
}

-- The Times 03/Jan/2009 Chancellor on brink of second bailout for banks
initialState = GlobalState {
    txPool = []
}

type AppState = StateT GlobalState IO

-- Constant section
flagConst = B.pack [0, 0, 0, 1] -- Constant of 0000 because we don't support SegWit

-- Helper function section
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
                   TxInput       -- Coinbase
                   [TxInput]
                   [TxOutput]    -- Technically, in the origial bitcoin format,
                                 -- There should be only two output:
                                 -- The spend and the change
                                 -- But it wouldn't hurt to do a simple extension here
data Block = Block
                   Version
                   Flag
                   InCounter
                   OutCounter
                   [Transaction]                   

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
drawMerkleTree  = unlines . drawMT
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
-- |       1   1
-- | [1, 2] => 1|2
-- |          1   2
-- | [1, 2, 3] => 1|2|3|3
-- |            1|2     3|3
-- |           1   2   3dup3
-- | [1, 2, 3, 4] => 1|2|3|4
-- |               1|2     3|4
-- |              1   2   3   4
-- | [1, 2, 3, 4, 5] => 1|2|3|4|5|5|5|5
-- |              1|2|3|4             5|5|5|5
-- |            1|2     3|4         5|5 dup 5|5
-- |           1   2   3   4       5   5
-- | [1, 2, 3, 4, 5, 6] => 1|2|3|4|5|6|5|6
-- |                 1|2|3|4             5|6|5|6
-- |               1|2     3|4         5|6 dup 5|6
-- |              1   2   3   4       5   6
-- | [1, 2, 3, 4, 5, 6, 7] => 1|2|3|4|5|6|7|7
-- |                    1|2|3|4             5|6|7|7
-- |                  1|2     3|4         5|6     7|7
-- |                 1   2   3   4       5   6   7dup7
-- | [1, 2, 3, 4, 5, 6, 7, 8] => 1|2|3|4|5|6|7|8
-- |                       1|2|3|4             5|6|7|8
-- |                     1|2     3|4         5|6     7|8
-- |                    1   2   3   4       5   6   7   8

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

-- Manuall verify the integrity of a MerkleTree
verifyMerkleTree :: (Hashable a) =>
                    MerkleTree a ->
                    Bool
verifyMerkleTree = undefined

-- Credit: ChatGPT added skewness in generateInclusionProof and proveHashableInclusion for consistent order
generateInclusionProof :: (Hashable a) =>
                          MerkleTree a -> 
                          a ->            -- Element to prove inclusion
                          Maybe [(Bool, Hash)]  -- (IsRightSibling, SiblingHash)
generateInclusionProof (LeafNode hsh hble) y = 
    if (takeHash y) == hsh 
    then Just []  -- Element found, return empty proof path
    else Nothing  -- Element not found
    
generateInclusionProof (INode hsh lft rht) y =
    case generateInclusionProof lft y of
        Just path -> Just (path ++ [(True, takeHash rht)])  -- Right sibling
        Nothing -> case generateInclusionProof rht y of
            Just path -> Just (path ++ [(False, takeHash lft)])  -- Left sibling
            Nothing -> Nothing

-- Verify the inclusion proof
-- (Bool, Hash): Bool = True if the sibling is on the right, False if on the left
proveHashableInclusion :: (Hashable a) =>
                          Hash ->       -- Root hash
                          a ->          -- Element to prove
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
addToMerkleTree = undefined

-- Example section
data MerkleTreeExample = MerkleTreeExample B.ByteString

shell :: AppState ()
shell = do
    liftIO $ runInputT defaultSettings $ do
        outputStr "CurryCoin> (This shell is not yet used!!! Use GHCi for PoC testing)"
        input <- getInputLine ""
        case input of
            Just "exit" -> liftIO $ putStrLn "Exiting..."
            Just _      -> do
                liftIO $ putStrLn "Unknown command"
                liftIO $ evalStateT shell initialState  -- Continue shell
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
