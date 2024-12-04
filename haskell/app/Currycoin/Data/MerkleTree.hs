{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Currycoin.Data.MerkleTree where

import Currycoin.Common
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU

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
drawMerkleTree = unlines . drawMT
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

-- | Construct a Merkle Tree from a list of hashes.
-- | [1] => 1|1
-- |           1   1
-- | [1, 2] => 1|2
-- |              1          2
-- | [1, 2, 3] => 1|2|3|3
-- |                1|2        3|3
-- |               1   2   3dup3
-- | [1, 2, 3, 4] => 1|2|3|4
-- |                   1|2           3|4
-- |                  1   2          3   4
-- | [1, 2, 3, 4, 5] => 1|2|3|4|5|5|5|5
-- |                  1|2|3|4              5|5|5|5
-- |                1|2        3|4            5|5 dup 5|5
-- |               1   2   3   4           5   5
-- | [1, 2, 3, 4, 5, 6] => 1|2|3|4|5|6|5|6
-- |                     1|2|3|4                 5|6|5|6
-- |                   1|2           3|4               5|6 dup 5|6
-- |                  1   2          3   4              5          6
-- | [1, 2, 3, 4, 5, 6, 7] => 1|2|3|4|5|6|7|7
-- |                        1|2|3|4                    5|6|7|7
-- |                      1|2     3|4          5|6          7|7
-- |                     1         2   3         4         5   6         7dup7
-- | [1, 2, 3, 4, 5, 6, 7, 8] => 1|2|3|4|5|6|7|8
-- |                           1|2|3|4               5|6|7|8
-- |                         1|2         3|4             5|6     7|8
-- |                        1   2        3   4            5        6   7        8

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
verifyMerkleTree :: (Hashable a) => MerkleTree a -> Bool
verifyMerkleTree (LeafNode nodeHash dataVal) =
        nodeHash == takeHash dataVal -- leaf node hash should be the hash of the data
verifyMerkleTree (INode nodeHash left right) =
        -- inode hash should be the hash of the concatenation of the left and right children
        -- and the left and right children should also be valid Merkle trees
        nodeHash == takeHash (B.append (takeHash left) (takeHash right)) &&
        verifyMerkleTree left && verifyMerkleTree right

addToMerkleTree :: (Hashable a) => (MerkleTree a) -> a -> (MerkleTree a)
addToMerkleTree originalTree newElement =
    -- take original merkle tree and new element and create a new merkle tree with the new element added
    let newLeaf = LeafNode (takeHash newElement) newElement
    in createMerkleTreeFromListInternal [originalTree, newLeaf]

-- Credit: ChatGPT added skewness in generateInclusionProof and proveHashableInclusion for consistent order
generateInclusionProof :: (Hashable a) =>
                          MerkleTree a ->
                          a ->                  -- Element to prove inclusion
                          Maybe [(Bool, Hash)]        -- (IsRightSibling, SiblingHash)
generateInclusionProof (LeafNode hsh hble) y =
    if (takeHash y) == hsh
    then Just []  -- Element found, return empty proof path
    else Nothing  -- Element not found

generateInclusionProof (INode hsh lft rht) y =
    case generateInclusionProof lft y of
        Just path -> Just (path ++ [(True, takeHash rht)])  -- Right sibling
        Nothing -> case generateInclusionProof rht y of
            Just path -> Just (path ++ [(False, takeHash lft)])         -- Left sibling
            Nothing -> Nothing

-- Example section
data MerkleTreeExample = MerkleTreeExample B.ByteString
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

-- Verify the inclusion proof
-- (Bool, Hash): Bool = True if the sibling is on the right, False if on the left
proveHashableInclusion :: (Hashable a) =>
                          Hash ->        -- Root hash
                          a ->                -- Element to prove
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

