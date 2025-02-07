{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Currycoin.Common where

import Crypto.Hash.SHA256
import Data.Word (Word8)
import Data.Maybe (listToMaybe, fromJust)
import Numeric (showHex, readHex)
import Data.List (intercalate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Conversion as BC
import Data.ByteString.UTF8 (fromString)
import Data.Bits (testBit)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import System.Console.Haskeline

type Hash = B.ByteString
type Amount = Int
type TxInput = Hash -- Previous spendable output
data TxOutput = TxOutput String Amount -- Address and amount
    deriving (Eq)

class Hashable a where
    serialize :: a -> B.ByteString
    takeHash :: a -> Hash
    takeHash = hash . serialize

instance Hashable TxOutput where
    serialize (TxOutput addr amount) = (hash . B.pack) (addrhs ++ amounths)
        where
            addrhs = (B.unpack . fromString) addr
            amounths = (BL.unpack . BC.toByteString) amount

instance Hashable String where
    serialize = fromString

instance Hashable B.ByteString where
    serialize a = a
    takeHash a = a

instance Show TxOutput where
    show (TxOutput addr amount) = addr ++ " (" ++ (show amount) ++ ")"

txoToAmount (TxOutput _ amount) = amount

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

-- Convert Int to ByteString
intToByteString :: Int -> B.ByteString
intToByteString n = fromString (show n)

-- Convert a bytestring to its last four bin digit, for flag display, helped by GPT
lastFourBinaryDigits :: B.ByteString -> String
lastFourBinaryDigits bs =
    let fullBinary = concatMap byteToBinaryString (B.unpack bs)
    in drop (length fullBinary - 4) fullBinary
  where
    byteToBinaryString :: Word8 -> String
    byteToBinaryString w = [if testBit w i then '1' else '0' | i <- [7,6..0]]

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

getInputLineValidated :: String -> (String -> Maybe a) -> (String -> IO (Bool)) -> IO (Maybe a)
getInputLineValidated prompt f ferr =
            runInputT defaultSettings $ do
                input <- getInputLine prompt
                case input of
                    Just input_data ->
                        case (f input_data) of
                            Just u -> do
                                return (Just u)
                            Nothing -> do
                                retry <- liftIO $ ferr input_data
                                if retry then do
                                    liftIO $ getInputLineValidated prompt f ferr
                                else do
                                    return Nothing
                    Nothing -> do
                        return Nothing


(|||) :: Maybe a -> [(a -> Bool)] -> Maybe a
(|||) Nothing bs = Nothing
(|||) (Just x) [] = Just x
(|||) (Just x) (a:bs) | (a x)     = (|||) (Just x) bs
                      | otherwise = Nothing

