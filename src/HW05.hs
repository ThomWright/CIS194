{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, DisambiguateRecordFields #-}
module HW05 where

import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString)
import Data.List (maximumBy, sortOn)
import Data.Map.Strict (Map)
import Data.Ord
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import HW05.Parser (TId, Transaction, FromJSON, ToJSON, encode, decode)
import qualified HW05.Parser as Tx


-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret originalFP modifiedFP = do
  original <- BS.readFile originalFP
  modified <- BS.readFile modifiedFP
  return $ BS.pack $ filter (/= 0) $ BS.zipWith xor original modified


-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey secretKey filepath = do
  encrypted <- BS.readFile (filepath ++ ".enc")
  let decrypted = BS.pack $ BS.zipWith xor (BS.cycle secretKey) encrypted
  BS.writeFile filepath decrypted

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile filepath = do
  file <- BS.readFile filepath
  return $ decode file

-- Exercise 4 -----------------------------------------

transactionsIn :: [TId] -> [Transaction] -> [Transaction]
transactionsIn tIDs = filter ((`elem` tIDs) . Tx.tid)

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsListFP transactionsFP = do
  Just badTIds <- parseFile victimsListFP :: IO (Maybe [TId])
  Just potentialBadTs <- parseFile transactionsFP :: IO (Maybe [Transaction])
  return $ Just $ transactionsIn badTIds potentialBadTs

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow []     = Map.empty
getFlow (t:ts) = update Tx.to tAmount $ update Tx.from (-tAmount) $ getFlow ts
  where
    tAmount = Tx.amount t

    update getPerson amount = Map.alter (addAmount amount) (getPerson t)

    addAmount :: Integer -> Maybe Integer -> Maybe Integer
    addAmount amount Nothing      = Just amount
    addAmount amount (Just total) = Just (total + amount)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = fst $ maximumBy (comparing snd) $ Map.toList m

-- Exercise 7 -----------------------------------------

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

createTransactionsFor :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
createTransactionsFor [] _  _  = []
createTransactionsFor _  [] _  = []
createTransactionsFor _  _  [] = []
createTransactionsFor payers payees (t:ts) = newTransaction : createTransactionsFor nextPayers nextPayees ts
  where
    payer = head payers
    payee = head payees

    amount = min (snd payer) (snd payee)

    payer' = (fst payer, snd payer - amount)
    payee' = (fst payee, snd payee - amount)

    nextPayers = [payer' | snd payer' > 0] ++ tail payers
    nextPayees = [payee' | snd payee' > 0] ++ tail payees

    newTransaction = Tx.Transaction { from   = fst payer
                                    , to     = fst payee
                                    , amount = amount
                                    , tid    = t
                                    }

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs people = createTransactionsFor payers payees
  where
    (payers, payees) = mapTuple sortByDescAmount $ Map.partition (> 0) people

    sortByDescAmount = sortOn (Down . snd) . map toAbs . Map.toList
    toAbs (name, amount) = (name, abs amount)

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON outputFP output = BS.writeFile outputFP $ encode output

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

run :: IO ()
run = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "data/HW05/dog-original.jpg"
                        "data/HW05/dog.jpg"
                        "data/HW05/transactions.json"
                        "data/HW05/victims.json"
                        "data/HW05/new-ids.json"
                        "data/HW05/new-transactions.json"
  putStrLn crim
