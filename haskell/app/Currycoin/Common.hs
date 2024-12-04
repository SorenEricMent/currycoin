{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Currycoin.Common where

import Crypto.Hash.SHA256
import Data.Word (Word8)
import Data.Maybe (listToMaybe, fromJust)
import Numeric (showHex, readHex)
import Data.List (intercalate)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU

type Hash = B.ByteString
type Amount = Integer
type TxInput = Hash -- Previous spendable output
data TxOutput = TxOutput String Amount -- Address and amount

class Hashable a where
    serialize :: a -> B.ByteString
    takeHash :: a -> Hash
    takeHash = hash . serialize

instance Hashable String where
    serialize = BSU.fromString

instance Hashable B.ByteString where
    serialize a = a

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

-- Convert Int to ByteString.UTF8
intToByteString :: Int -> BSU.ByteString
intToByteString n = BSU.fromString (show n)

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

