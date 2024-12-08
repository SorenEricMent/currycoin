{-# LANGUAGE BangPatterns #-}
module Main where

import Lib
import Currycoin.Common
import Currycoin.Data.Key
import Currycoin.Data.Transaction
import Currycoin.Data.Block
import Crypto.Secp256k1
import Data.ByteString.Base58
import Data.Maybe (listToMaybe, fromJust)
import Data.List (intercalate, intersperse)
import qualified Data.ByteString as B
import Currycoin.Data.MerkleTree

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import System.IO (hFlush, stdout)
import System.Console.Haskeline

data GlobalState = GlobalState {
    -- Todo
    block :: [Block],
    txPool :: [Transaction],
    utxo :: [(TxOutput, Hash)]
}

-- The Times 03/Jan/2009 Chancellor on brink of second bailout for banks
initialState = GlobalState {
    block = [genesis],
    txPool = [],
    utxo = [(getOutput $ fromJust $ getCoinbase genesis)!!0] -- We assume coinbase pay and only pay to one, no p2p mining share, so we uses unsafe fromJust
} where
  genesis = generateGenesis

showTxPool :: [Transaction] -> String
showTxPool txs = "Pending Transaction Pool: \n" ++ concat (intersperse "\n" (map show txs))

showUTXO :: [(TxOutput, Hash)] -> String
showUTXO utxo = intercalate "\n" (map singleOutputShow utxo)
  where  -- Somehow using show causes an overlap in type
    singleOutputShow :: (TxOutput, Hash) -> String
    singleOutputShow ((TxOutput addr amount), hash) =
        "Output Hash:\t" ++ byteStringToHex hash ++ "\n" ++
        "Address:\t" ++ addr ++ "\n" ++
        "Amount:\t\t" ++ show amount ++ "\n"
    
type AppState = StateT GlobalState IO

shell :: AppState ()
shell = do
    currentState <- get
    liftIO $ runInputT defaultSettings $ do
        outputStr "CurryCoin> "
        input <- getInputLine ""
        ctx <- liftIO Crypto.Secp256k1.createContext
        case input of
            Just input_data ->
                case (words (input_data))!?0 of
                    Just "exit" -> do
                        liftIO $ putStrLn "Exiting..."
                    Just "init" -> do
                        liftIO $ putStrLn "Reset all state to initial"
                        liftIO $ evalStateT shell initialState
                    Just "help" -> do
                        liftIO $ putStrLn "exit, help, init, new_address, height, transact"
                        liftIO $ evalStateT shell currentState
                    Just "new_address" -> do
                        private_key <- liftIO $ genPrivateKey
                        let pubKey = derivePubKey ctx (Crypto.Secp256k1.SecKey private_key)
                        liftIO $ putStrLn $ "New private key:\t" ++ (show . byteStringToHex) private_key
                        liftIO $ putStrLn $ "New public key:\t\t" ++ (show . byteStringToHex . processPubKeyFromLib) pubKey
                        liftIO $ putStrLn $ "New public address:\t" ++ (pubkeyToAddress pubKey)
                        liftIO $ evalStateT shell currentState
                    Just "height" -> do
                        case (block currentState)!?0 of
                            Just n -> do
                                liftIO $ putStrLn $ show $ length $ block currentState
                            Nothing -> do
                                liftIO $ putStrLn "No block exists in the database."
                        liftIO $ evalStateT shell currentState
                    Just "new_tx" -> do
                        liftIO $ putStrLn "placehold"
                        liftIO $ evalStateT shell currentState
                    Just "show_tx_pool" -> do
                        liftIO $ putStrLn "placehold"
                        liftIO $ evalStateT shell currentState
                    Just "show_tx" -> do
                        liftIO $ putStrLn "placehold"
                        liftIO $ evalStateT shell currentState
                    Just "show_utxo" -> do
                        liftIO $ putStrLn $ showUTXO $ utxo currentState
                        liftIO $ evalStateT shell currentState
                    Just "show_utxo_addr" -> do
                        liftIO $ putStrLn "placehold"
                        liftIO $ evalStateT shell currentState
                    Just "show_block" -> do
                        case (words (input_data))!?1 of
                            Just height ->
                                case (block currentState)!?((read height)-1) of
                                    Just blk -> do
                                        liftIO $ putStrLn (show blk)
                                        liftIO $ evalStateT shell currentState
                                    Nothing -> do
                                        liftIO $ putStrLn "No block with such height exists!"
                                        liftIO $ evalStateT shell currentState
                            Nothing -> do
                                liftIO $ putStrLn "Missing block height!"
                                liftIO $ evalStateT shell currentState
                    Just "mint_block" -> do
                        case (words (input_data))!?1 of
                            Just addr -> do
                                let height = (length $ block currentState)
                                let txs = txPool currentState
                                let txsMaybe = if (txs == []) then Nothing else Just (createMerkleTreeFromList txs)
                                let prevHash = B.pack [0x0] -- todo
                                let additional = B.pack [0x0] -- todo
                                liftIO $ putStrLn "Started mining!"
                                let !newBlock = mintBlock (B.pack [0x1]) flagConst addr txsMaybe (fromIntegral height) prevHash additional
                                let newState = GlobalState {
                                    block = (block currentState) ++ [newBlock],
                                    txPool = [],
                                    utxo = [] -- todo
                                                        }
                                liftIO $ evalStateT shell newState
                            Nothing -> do
                                liftIO $ putStrLn "Missing miner address!"
                                liftIO $ evalStateT shell currentState
                    Just _        -> do
                        liftIO $ putStrLn "Unknown command"
                        liftIO $ evalStateT shell currentState        -- Continue shell
                    Nothing -> return ()  -- If no input is provided
            Nothing -> return ()  -- If no input is provided
    -- Credit: ChatGPT replaced readline with haskeline

main :: IO ()
main = evalStateT shell initialState
