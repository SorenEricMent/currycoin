module Currycoin.Data.Key where

import System.Entropy
import Data.ByteString.Base58
import Crypto.Secp256k1
import Crypto.Hash.SHA256
import Crypto.Hash.RIPEMD160
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU

type Address = B.ByteString
type MyPubKey = BSU.ByteString
type MyPrivKey = BSU.ByteString

processPubKeyFromLib :: PubKey -> MyPubKey
processPubKeyFromLib (PubKey bs) =
    B.pack ([0x4] ++ (snd sres) ++ (fst sres))
    where
      sres = (splitAt 32 . reverse . B.unpack) bs

processPubKeyToLib :: MyPubKey -> PubKey
processPubKeyToLib pubkey = undefined

pubkeyToAddress :: MyPubKey -> Address
pubkeyToAddress pubkey =
    encodeBase58 bitcoinAlphabet (addChecksum (B.append (B.pack [0x00]) (Crypto.Hash.RIPEMD160.hash (Crypto.Hash.SHA256.hash pubkey))))
    where
      addChecksum :: B.ByteString -> B.ByteString
      addChecksum bs = B.append bs (B.take 4 (Crypto.Hash.SHA256.hash (Crypto.Hash.SHA256.hash bs)))

genPrivateKey :: IO MyPrivKey
genPrivateKey = Crypto.Hash.SHA256.hash <$> getEntropy 32

