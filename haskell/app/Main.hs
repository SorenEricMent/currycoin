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
import Data.Maybe (listToMaybe, isNothing, fromJust)
import Data.List (intercalate, intersperse)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU
import Text.Read (readMaybe)

import Currycoin.Data.MerkleTree

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import System.IO (hFlush, stdout)
import System.Console.Haskeline
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
    if (curamount == total) then do
        return acc
    else do
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
                    return []
    where validateAddr :: String -> Maybe String
          validateAddr s =
              if ((length (filter (\(TxOutput addr _) -> addr == s) acc)) == 0) then
                  Just s
              else
                  Nothing
          ferrAddr s = putStrLn ("Duplicate output address: " ++ s ++ "!") >> return True
          validateAmount s = (readMaybe s) ||| [(\i -> i >= 0 && (curamount + i) <= total)]
          ferrAmount s = putStrLn ("Invalid amount: " ++ s ++ "!") >> return True
          curamount = (sum (map txoToAmount acc))
          amountstr = "(" ++ (show curamount) ++ "/" ++ (show total) ++ ")"

getKeys :: Crypto.Secp256k1.Ctx -> [UTXO] -> [MyPrivKey] -> IO ([MyPrivKey])
getKeys _ [] acc = do return acc
getKeys ctx (utxo:us) acc = do
            liftIO $ putStrLn ("Enter the private key for the following UTXO to sign the transaction:\n" ++ (showUTXO utxo))
            input <- getInputLineValidated ("Private key for " ++ utxoaddr ++ " (" ++ (show (length us)) ++ " remaining): ")
                                           validate
                                           ferr
            case input of
                Just k -> do
                    liftIO $ getKeys ctx us (acc ++ [k])
                Nothing -> do
                    return acc
    where ferr s = putStrLn ("Invalid private key: " ++ s) >> return True
          utxoaddrf (UTXO (TxOutput address _) _) = address
          utxoaddr = utxoaddrf utxo
          validate s =
              if (addr == utxoaddr) then
                  Just privkey
              else
                  Nothing
              where
                  privkey = hexToByteString s
                  pubKey = (processPubKeyFromLib (derivePubKey ctx (Crypto.Secp256k1.SecKey privkey)))
                  addr = (addressToString . pubkeyToAddress) pubKey

newUTXO :: Hash -> TxOutput -> UTXO
newUTXO hash txo = UTXO txo bh
    where
        bs = B.pack ((B.unpack hash) ++ (B.unpack (takeHash txo)))
        bh = Crypto.Hash.SHA256.hash bs

signTX :: Crypto.Secp256k1.Ctx -> Transaction -> [MyPrivKey] -> Transaction
signTX ctx (Transaction inputs outputs signatures) kk =
    Transaction inputs outputs (sf kk)
    where sf [] = []
          sf (k:ks) = [ss] ++ (sf ks)
              where sk = Crypto.Secp256k1.SecKey k
                    sg = signMsg ctx sk ms
                    ss = get sg
                    get (Sig s) = s
          tx = (Transaction inputs outputs signatures)
          ms = (fromJust . msg . takeHash) tx

processMerkleProofInput :: [String] -> Maybe [(Bool, Hash)]
processMerkleProofInput [] = Just []
processMerkleProofInput [a] = error "Impossible! Received odd proof even after prefiltering"
processMerkleProofInput (a:b:rest) =
    if (a == "0")
    then (case (processMerkleProofInput rest) of
              Just result -> Just ([(False, convertedHash)] ++ result)
              Nothing -> Nothing)
    else if (a == "1")
    then (case (processMerkleProofInput rest) of
              Just result -> Just ([(True, convertedHash)] ++ result)
              Nothing -> Nothing)
    else Nothing
    where convertedHash = hexToByteString b

newTX :: Crypto.Secp256k1.Ctx -> [UTXO] -> IO (Maybe (Transaction, [UTXO]))
newTX ctx utxo = do
    inputs <- liftIO $ getInputHashes utxo []
    let inamount = sum (map (\(UTXO (TxOutput _ amount) _) -> fromIntegral amount) inputs)
    if (length inputs == 0) then do
        liftIO $ putStrLn "No hash input!"
        return Nothing
    else do
        outputs <- liftIO $ getOutputs inamount []
        if (length outputs == 0) then do
            return Nothing
        else do
            let ins = map (\(UTXO _ h) -> h) inputs
            let inhash = Crypto.Hash.SHA256.hash (B.pack (concat (map B.unpack (map hash ins))))
            let out = map (\o -> newUTXO inhash o) outputs
            let tx = Transaction ins out []
            liftIO $ putStrLn ("Creating and signing the following transaction:\n" ++ (show tx))
            keys <- liftIO $ getKeys ctx inputs []
            if (length keys == 0) then do
                return Nothing
            else do
                let txsigned = signTX ctx tx keys
                let res = (txsigned, inputs)
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
                        liftIO $ putStrLn "init\t\t\t\t\tReset to initial state"
                        liftIO $ putStrLn "exit\t\t\t\t\tExit the program"
                        liftIO $ putStrLn "help\t\t\t\t\tShow this help"
                        liftIO $ putStrLn "new_address\t\t\t\tGenerate a new private key, public key, and Bitcoin address"
                        liftIO $ putStrLn "height\t\t\t\t\tShow the latest block height"
                        liftIO $ putStrLn "new_tx\t\t\t\t\tCreate a new transaction"
                        liftIO $ putStrLn "show_block height\t\t\tShow the block at the specific height"
                        liftIO $ putStrLn "show_utxo\t\t\t\tShow UTXOs"
                        liftIO $ putStrLn "show_tx_pool\t\t\t\tShow transaction pool"
                        liftIO $ putStrLn "show_utxo_addr address\t\t\tFilter UTXO by address"
                        liftIO $ putStrLn "mint_block address\t\t\tMine a block and send coinbase to the specific address"
                        liftIO $ putStrLn "verify_tx height hash pubkeys ...\tVerify a transaction"
                        liftIO $ putStrLn "gen_merkle_proof Height TXHash ...\tGenerate a transactions' inclusion proof"
                        liftIO $ putStrLn "gen_verify_proof Height [((L|R), Hash)] ...\tVerify the proof that a transaction is included"
                        liftIO $ putStrLn "prune Height ...\tPrune a block, does not change stored UTXO"
                        liftIO $ evalStateT shell currentState
                    Just "new_address" -> do
                        private_key <- liftIO $ genPrivateKey
                        let pubKey = (processPubKeyFromLib (derivePubKey ctx (Crypto.Secp256k1.SecKey private_key)))
                        liftIO $ putStrLn $ "New private key:\t" ++ byteStringToHex private_key
                        liftIO $ putStrLn $ "New public key:\t\t" ++ byteStringToHex pubKey
                        liftIO $ putStrLn $ "New public address:\t" ++ (addressToString . pubkeyToAddress) pubKey
                        liftIO $ evalStateT shell currentState
                    Just "height" -> do
                        case (block currentState)!?0 of
                            Just n -> do
                                liftIO $ putStrLn $ show $ length $ block currentState
                            Nothing -> do
                                liftIO $ putStrLn "No block exists in the database."
                        liftIO $ evalStateT shell currentState
                    Just "new_tx" -> do
                        tx <- liftIO $ newTX ctx (utxo currentState)
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
                    Just "verify_tx" -> do
                        let w = words input_data
                        if (length w) > 3 then do
                            let h = (hexToByteString . fromJust) (w!?2)
                            let s = map hexToByteString (snd (splitAt 3 w))
                            case (block currentState)!?(((read . fromJust) (w!?1))-1) of
                                Just blk -> do
                                    case (findTX blk h) of
                                        Just tx -> do
                                            if (verifyTX ctx tx s) then
                                                liftIO $ putStrLn "The transaction is verified"
                                            else
                                                liftIO $ putStrLn "The transaction cannot be verified"
                                            liftIO $ evalStateT shell currentState
                                        Nothing -> do
                                            liftIO $ putStrLn "Transaction not found in the specific block"
                                            liftIO $ evalStateT shell currentState
                                Nothing -> do
                                    liftIO $ putStrLn "No block with such height exists!"
                                    liftIO $ evalStateT shell currentState
                        else do
                            liftIO $ putStrLn "Usage: verify_tx height hash pubkeys ..."
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
                    Just "gen_merkle_proof" -> do
                        case (words (input_data))!?1 of
                            Just height -> do
                                case (words (input_data))!?2 of
                                    Just txhash -> do
                                        let targetBlockMaybe = (block currentState)!?((read height)-1)
                                        case targetBlockMaybe of
                                            Just targetBlock -> do
                                                let targetTxMaybe = (findTX targetBlock (hexToByteString txhash))
                                                case targetTxMaybe of
                                                    Just targetTx -> do
                                                        let treeMaybe = (getTxTree targetBlock)
                                                        case treeMaybe of
                                                            Just txtree -> do
                                                                let proofMaybe = generateInclusionProof txtree targetTx
                                                                case proofMaybe of
                                                                    Just proof -> do
                                                                        liftIO $ putStrLn $ merkleProofPrettify proof
                                                                        liftIO $ evalStateT shell currentState
                                                                    Nothing -> do
                                                                        liftIO $ putStrLn "Failed to generate proof."
                                                                        liftIO $ evalStateT shell currentState
                                                            Nothing -> do
                                                                liftIO $ putStrLn "Failed to generate proof: no Tx in block"
                                                                liftIO $ evalStateT shell currentState
                                                    Nothing -> do
                                                        liftIO $ putStrLn "Failed to generate proof: tx not found"
                                                        liftIO $ evalStateT shell currentState
                                            Nothing -> do
                                                liftIO $ putStrLn "Invalid height"
                                                liftIO $ evalStateT shell currentState
                                    Nothing -> do
                                        liftIO $ putStrLn "Missing transaction hash."
                                        liftIO $ evalStateT shell currentState
                            Nothing -> do
                                liftIO $ putStrLn "Missing height."
                                liftIO $ evalStateT shell currentState
                    Just "verify_merkle_proof" -> do
                        if (odd ((length $ words (input_data)) - 2))
                        then do
                            let proof = processMerkleProofInput (tail (tail (tail (words input_data))))
                            if (proof == Nothing)
                            then do
                                liftIO $ putStrLn "Invalid proof, notice to use 0 for left and 1 for right."
                                liftIO $ evalStateT shell currentState
                            else do
                                if (proveHashableInclusion (hexToByteString ((words input_data)!!1)) (hexToByteString ((words input_data)!!2)) (fromJust proof))
                                then do
                                    liftIO $ putStrLn "Proof is valid"
                                    liftIO $ evalStateT shell currentState
                                else do
                                    liftIO $ putStrLn "Proof is invalid"
                                    liftIO $ evalStateT shell currentState
                        else do
                            liftIO $ putStrLn "Proof should be even number of parameters in 0(Left)1(Right) Hash"
                            liftIO $ evalStateT shell currentState
                    Just "prune" -> do
                        case (words (input_data))!?1 of
                            Just height -> do
                                if ((read height) > (length $ block currentState))
                                then do
                                    liftIO $ putStrLn "Invalid height"
                                    liftIO $ evalStateT shell currentState
                                else do
                                    let blockSplitTuple = splitAt ((read height) - 1) (block currentState)
                                    let newState = GlobalState {
                                        block = (fst blockSplitTuple) ++ [pruneBlock (head (snd blockSplitTuple))] ++ (tail $ snd $ blockSplitTuple),
                                        txPool = txPool currentState,
                                        utxo = utxo currentState
                                    }
                                    liftIO $ evalStateT shell newState
                            Nothing -> do
                                liftIO $ putStrLn "Missing height."
                                liftIO $ evalStateT shell currentState
                    Just _        -> do
                        liftIO $ putStrLn "Unknown command"
                        liftIO $ evalStateT shell currentState        -- Continue shell
                    Nothing -> return ()  -- If no input is provided
            Nothing -> return ()  -- If no input is provided
    -- Credit: ChatGPT replaced readline with haskeline

main :: IO ()
main = evalStateT shell initialState
