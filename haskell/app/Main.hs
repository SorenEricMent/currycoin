{-# LANGUAGE BangPatterns #-}
module Main where

import Lib
import Currycoin.Common
import Currycoin.Data.Key
import Currycoin.Data.Transaction
import Currycoin.Data.Block
import Crypto.Secp256k1
import Crypto.Hash.SHA256
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
import Data.List

data GlobalState = GlobalState {
    block :: [Block],
    txPool :: [Transaction],
    utxo :: [UTXO]
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

showUTXO :: UTXO -> String
showUTXO (UTXO (TxOutput addr amount) hash) =
    "Output Hash:\t" ++ byteStringToHex hash ++ "\n" ++
    "Address:\t" ++ addr ++ "\n" ++
    "Amount:\t\t" ++ show amount

showUTXOs :: [UTXO] -> String
showUTXOs utxo = intercalate "\n" (map showUTXO utxo)
    
type AppState = StateT GlobalState IO

lookupUTXO :: [UTXO] -> Hash -> Maybe UTXO
lookupUTXO utxo hash = if (length l) == 0 then Nothing else Just (head l)
    where l = filter (\(UTXO (TxOutput _ _) u) -> hash == u) utxo

getInputHashes :: [UTXO] -> [UTXO] -> IO ([UTXO])
getInputHashes utxo acc = do
     input <- liftIO $ getInputLineValidated ("Input Hash (EOF to end) [" ++ (show (length acc)) ++ "]: ")
                                             validate
                                             ferr
     case input of
         Just u ->
             liftIO $ getInputHashes utxo (acc ++ [u])
         Nothing -> do
             return acc
    where validate s =
            if ((length s) == 64) then
                (lookupUTXO utxo (hexToByteString s)) ||| [chkdup]
            else
                Nothing
          ferr s = putStrLn ("Invalid UTXO output hash: " ++ s ++ "! Current UTXO:\n" ++ (showUTXOs utxo)) >> return True
          chkdup (UTXO _ s) = (length (filter (\(UTXO _ hash) -> hash == s) acc)) == 0

getOutputs :: Amount -> [TxOutput] -> IO ([TxOutput])
getOutputs total acc = do
    input <- liftIO $ getInputLineValidated ("Output Address (EOF to end) [" ++ (show (length acc)) ++ "] " ++ amountstr ++ ": ")
                                            validateAddr
                                            ferrAddr
    case input of
        Just address -> do
            input2 <- getInputLineValidated ("Amount " ++ amountstr ++ ": ")
                                            validateAmount
                                            ferrAmount
            case input2 of
                Just amount ->
                    liftIO $ getOutputs total (acc ++ [(TxOutput address amount)])
                Nothing ->
                    liftIO $ getOutputs total acc
        Nothing -> do
            if curamount /= total then do
                    liftIO $ putStrLn ("Total amount must be " ++ (show total) ++ "!")
                    liftIO $ getOutputs total acc
            else
                    return acc
    where validateAddr :: String -> Maybe String
          validateAddr s =
              if ((length s) == 34) && ((length (filter (\(TxOutput addr _) -> addr == s) acc)) == 0) then
                  Just s
              else
                  Nothing
          ferrAddr s = putStrLn ("Duplicate output address: " ++ s ++ "!") >> return True
          validateAmount s = (readMaybe s) ||| [(\i -> i >= 0 && (curamount + i) <= total)]
          ferrAmount s = putStrLn ("Invalid amount: " ++ s ++ "!") >> return True
          curamount = (sum (map txoToAmount acc))
          amountstr = "(" ++ (show curamount) ++ "/" ++ (show total) ++ ")"

getKeys :: [UTXO] -> [(UTXO, MyPrivKey)] -> IO ([(UTXO, MyPrivKey)])
getKeys [] acc = do return acc
getKeys (utxo:us) acc = do
            liftIO $ putStrLn ("Enter the private key for the following UTXO to sign the transaction:\n" ++ (showUTXO utxo))
            input <- getInputLineValidated ("Private key (" ++ (show (length us)) ++ " remaining): ")
                                           validate
                                           ferr
            case input of
                Just k -> do
                    liftIO $ getKeys us (acc ++ [(utxo, hexToByteString k)])
                Nothing -> do
                    return acc
    where validate s = if ((length s) == 64) then Just s else Nothing
          ferr s = putStrLn ("Invalid private key: " ++ s) >> return True

newUTXO :: Hash -> TxOutput -> UTXO
newUTXO hash txo = UTXO txo bh
    where
        bs = B.pack ((B.unpack hash) ++ (B.unpack (takeHash txo)))
        bh = Crypto.Hash.SHA256.hash bs

newTX :: [UTXO] -> IO (Maybe (Transaction, [UTXO]))
newTX utxo = do
    inputs <- liftIO $ getInputHashes utxo []
    let inamount = sum (map (\(UTXO (TxOutput _ amount) _) -> fromIntegral amount) inputs)
    if (length inputs == 0) then do
        liftIO $ putStrLn "No hash input!"
        return Nothing
    else do
        outputs <- liftIO $ getOutputs inamount []
        if (length outputs == 0) then do
            liftIO $ putStrLn "No outputs!"
            return Nothing
        else do
            let ins = map (\(UTXO _ h) -> h) inputs
            let inhash = Crypto.Hash.SHA256.hash (B.pack (concat (map B.unpack (map hash ins))))
            let out = map (\o -> newUTXO inhash o) outputs
            let tx = Transaction ins out []
            liftIO $ putStrLn ("Creating and signing the following transaction:\n" ++ (show tx))
            combined <- liftIO $ getKeys inputs []
            let res = (tx, inputs)
            return (Just res)

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
                        let pubKey = (processPubKeyFromLib (derivePubKey ctx (Crypto.Secp256k1.SecKey private_key)))
                        liftIO $ putStrLn $ "New private key:\t" ++ byteStringToHex private_key
                        liftIO $ putStrLn $ "New public key:\t\t" ++ byteStringToHex pubKey
                        liftIO $ putStrLn $ "New public address:\t" ++ (byteStringToHex . pubkeyToAddress) pubKey
                        liftIO $ evalStateT shell currentState
                    Just "height" -> do
                        case (block currentState)!?0 of
                            Just n -> do
                                liftIO $ putStrLn $ show $ length $ block currentState
                            Nothing -> do
                                liftIO $ putStrLn "No block exists in the database."
                        liftIO $ evalStateT shell currentState
                    Just "new_tx" -> do
                        tx <- liftIO $ newTX (utxo currentState)
                        case tx of
                            Just ((Transaction inputs outputs signatures), inputUTXOs) -> do
                                let newState = GlobalState {
                                    block = (block currentState),
                                    txPool = (txPool currentState) ++ [(Transaction inputs outputs signatures)],
                                    utxo = (utxo currentState) \\ inputUTXOs
                                                        }
                                liftIO $ evalStateT shell newState
                            Nothing -> do
                                liftIO $ evalStateT shell currentState
                    Just "show_tx_pool" -> do
                        liftIO $ putStrLn (show $ txPool $ currentState)
                        liftIO $ evalStateT shell currentState
                    Just "show_utxo" -> do
                        liftIO $ putStrLn $ showUTXOs $ utxo currentState
                        liftIO $ evalStateT shell currentState
                    Just "show_utxo_addr" -> do
                        case (words (input_data))!?1 of
                            Just x -> do
                                liftIO $ putStrLn (x ++ " can spend the following outputs:")
                                liftIO $ putStrLn (concat (intersperse "\n" (map show (filter (\(UTXO (TxOutput addr _)_) -> addr == x) (utxo currentState)))))
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
