module Currycoin.Data.Key where

import System.Entropy
import Data.ByteString.Base58
import Crypto.Secp256k1
import Crypto.Hash.SHA256
import Crypto.Hash.RIPEMD160
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU

processPubKeyFromLib :: PubKey -> BSU.ByteString
processPubKeyFromLib (PubKey bs) =
    B.pack ([0x4] ++ (snd sres) ++ (fst sres))
    where
      sres = (splitAt 32 . reverse . B.unpack) bs

pubkeyToAddress :: PubKey -> String
pubkeyToAddress pubkey =
    show ((encodeBase58 bitcoinAlphabet (addChecksum (B.append (B.pack [0x00]) (Crypto.Hash.RIPEMD160.hash (Crypto.Hash.SHA256.hash (processPubKeyFromLib pubkey)))))))
    where
      addChecksum :: B.ByteString -> B.ByteString
      addChecksum bs = B.append bs (B.take 4 (Crypto.Hash.SHA256.hash (Crypto.Hash.SHA256.hash bs)))

genPrivateKey = Crypto.Hash.SHA256.hash <$> getEntropy 32


