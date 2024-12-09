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
import qualified Data.ByteString.UTF8 as BSU
import Text.Read (readMaybe)

import Currycoin.Data.MerkleTree

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import System.IO (hFlush, stdout)
import System.Console.Haskeline
import Data.Maybe (isNothing, fromJust)

data GlobalState = GlobalState {
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

getInputHashes :: Int -> [Hash] -> IO ([Hash])
getInputHashes times accumulate =
    case times of
        0 ->
            return accumulate
        _ ->
            runInputT defaultSettings $ do
                input <- getInputLine "Input Hash: "
                case input of
                    Just input_data ->
                        liftIO $ getInputHashes (times - 1)  (accumulate ++ [hexToByteString input_data])
                    Nothing -> do
                        outputStr "Invalid input."
                        liftIO $ getInputHashes times accumulate

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
                        liftIO $ putStrLn "exit, help, init, new_address, height, new_tx, show_block, show_utxo, show_tx_pool, show_utxo_addr"
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
                        -- new_state <- liftIO $ new_tx 3 ""
                        input_num <- getInputLine "Please input the number of Transaction inputs (UTXOs)"
                        case input_num of
                            Just x ->
                                case (readMaybe $ fromJust input_num) of
                                    Just y -> do
                                        input_list <- liftIO $ getInputHashes y []
                                        liftIO $ evalStateT shell currentState -- Todo
                                    Nothing -> do
                                        liftIO $ putStrLn "Invalid numerical input!"
                                        liftIO $ evalStateT shell currentState
                            Nothing -> do
                                liftIO $ putStrLn "Invalid numerical input!"
                                liftIO $ evalStateT shell currentState
                    Just "show_tx_pool" -> do
                        liftIO $ putStrLn (show $ txPool $ currentState)
                        liftIO $ evalStateT shell currentState
                    Just "show_utxo" -> do
                        liftIO $ putStrLn $ showUTXO $ utxo currentState
                        liftIO $ evalStateT shell currentState
                    Just "show_utxo_addr" -> do
                        case (words (input_data))!?1 of
                            Just x -> do
                                liftIO $ putStrLn (x ++ " can spend the following outputs:")
                                liftIO $ putStrLn (concat (intersperse "\n" (map show (filter (\((TxOutput addr _),_) -> addr == x) (utxo currentState)))))
                                liftIO $ evalStateT shell currentState
                            Nothing -> do
                                liftIO $ putStrLn "Missing address to lookup."
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
                                let oldUTXO = utxo currentState
                                let txs = txPool currentState
                                let txsMaybe = if (txs == []) then Nothing else Just (createMerkleTreeFromList txs)
                                let prevBlock = (block currentState)!!(height-1) -- Previous block is guanranteed with genesis
                                let prevHash = takeHash prevBlock
                                let additionalList = snd (splitAt 2 (words (input_data)))
                                let additional = (if (additionalList == []) then B.pack [0x0] else BSU.fromString $ unwords $ additionalList)
                                liftIO $ putStrLn "Started mining!"
                                let !newBlock = mintBlock (B.pack [0x1]) flagConst addr txsMaybe (fromIntegral height) prevHash additional
                                let newState = GlobalState {
                                    block = (block currentState) ++ [newBlock],
                                    txPool = [],
                                    utxo = oldUTXO ++ (getOutput (fromJust (getCoinbase newBlock))) ++ (foldr (++) [] (map getOutput txs)) 
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
