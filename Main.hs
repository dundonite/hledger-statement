#!/usr/bin/env stack
-- stack script --resolver lts-23.0 --package hledger-lib --package text

{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Hledger.Cli.Script
import qualified "text" Data.Text as T
import qualified "text" Data.Text.IO as T
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time.Calendar (Day)
import Hledger.Query (matchesPosting)
import Hledger.Data (MixedAmount, nullmixedamt, amounts, acommodity, ptransaction, Status(..), Amount, mixed, pamount, tstatus)

cmdmode = hledgerCommandMode (unlines
  ["statement"
  ,"List transactions for a specific account with additional details."
  ,"Usage: hledger-statement [OPTS] ACCOUNT"
  ,"or:    hledger statement -- [OPTS] ACCOUNT"
  ]) [
    flagNone ["historical", "H"] (setboolopt "historical") "Show historical running total/balance (includes postings before report start date)"
  ] [generalflagsgroup1] [] ([], Just $ argsFlag "[ACCOUNT]")

main = do
  opts@CliOpts{reportspec_=rspec, rawopts_=rawopts} <- getHledgerCliOpts cmdmode
  let historic = boolopt "historical" rawopts
  withJournalDo opts $ \journal -> do
    let account = _rsQuery rspec
        startDate = extractStartDate (journalPostings journal)
        (initialRunning, initialReconciled) =
          if historic
            then initializeBalancesHistorical journal account startDate
            else initializeBalances journal account startDate
        filteredPostings = filter (matchesPosting account) (journalPostings journal)

    -- Print column headers
    T.putStrLn $ T.intercalate "\t"
      [ "TxID", "Status", "Date", "Description", "Other Accounts", "Amount", "Running Balance", "Reconciled Balance" ]

    -- Prepare and output the report
    let (_, _, report) = foldl formatAndAccumulate (initialRunning, initialReconciled, []) filteredPostings
    mapM_ T.putStrLn report

-- Extract the start date based on the earliest posting
extractStartDate :: [Posting] -> Day
extractStartDate postings =
  case listToMaybe (map tdateFromPosting postings) of
    Just date -> date
    Nothing   -> error "No transactions found to determine start date"

-- Initialize balances at the start date (non-historical)
initializeBalances :: Journal -> Query -> Day -> (MixedAmount, MixedAmount)
initializeBalances journal query startDate =
  let postings = filter (\p -> tdateFromPosting p < startDate && matchesPosting query p) (journalPostings journal)
  in (nullmixedamt, nullmixedamt) -- Both balances start at 0

-- Initialize balances with historical data
initializeBalancesHistorical :: Journal -> Query -> Day -> (MixedAmount, MixedAmount)
initializeBalancesHistorical journal query startDate =
  let postings = filter (\p -> matchesPosting query p) (journalPostings journal)
      historicBalance = foldl mappend nullmixedamt $ map extractDollarAmount $
        filter (\p -> tdateFromPosting p <= startDate) postings
  in (historicBalance, historicBalance)

-- Extract the dollar amount from a Posting
extractDollarAmount :: Posting -> MixedAmount
extractDollarAmount p = 
  foldl mappend nullmixedamt $
    map toMixedAmount $ filter isDollarAmount (amounts $ pamount p)

-- Check if an Amount is in dollars
isDollarAmount :: Amount -> Bool
isDollarAmount a =
  acommodity a == "$" && case acost a of
    Nothing -> True  -- No cost basis, valid dollar amount
    Just (TotalCost cost) -> acommodity cost == "$"  -- Cost basis is also in dollars
    _ -> False  -- Exclude amounts with non-dollar cost bases

-- Convert an Amount to MixedAmount
toMixedAmount :: Amount -> MixedAmount
toMixedAmount amt = mixed [amt]

-- Accumulate balances and format postings for display
formatAndAccumulate :: (MixedAmount, MixedAmount, [T.Text]) -> Posting -> (MixedAmount, MixedAmount, [T.Text])
formatAndAccumulate (priorRunning, priorReconciled, accumulatedLines) p =
  let currentAmount = extractDollarAmount p
      runningBalance = priorRunning `mappend` currentAmount
      reconciledBalance = case getStatus p of
        "cleared" -> priorReconciled `mappend` currentAmount
        _          -> priorReconciled
      formattedLine = T.intercalate "\t"
        [ T.pack $ getTxID p
        , T.pack $ getStatus p
        , T.pack $ show (tdateFromPosting p)
        , maybe "" T.strip (tdescription <$> ptransaction p)
        , T.intercalate "; " (getOtherAccounts p)
        , T.pack $ showMixedAmount currentAmount
        , T.pack $ showMixedAmount runningBalance
        , T.pack $ showMixedAmount reconciledBalance
        ]
  in (runningBalance, reconciledBalance, accumulatedLines ++ [formattedLine])

-- Extract the transaction date from a posting
tdateFromPosting :: Posting -> Day
tdateFromPosting p =
  fromMaybe (error "No transaction found") (tdate <$> ptransaction p)

-- Extract the unique transaction ID (user_txid)
getTxID :: Posting -> String
getTxID p =
  case ptransaction p of
    Just txn -> fromMaybe "" (lookup "user_txid" (map (\(k, v) -> (T.unpack k, T.unpack v)) (ttags txn)))
    Nothing  -> ""

-- Determine the status of the transaction
getStatus :: Posting -> String
getStatus p =
  case tstatus <$> ptransaction p of
    Just Cleared  -> "cleared"
    Just Pending  -> "pending"
    Just Unmarked -> "unset"
    Nothing       -> "unset"

-- Get other accounts involved in the transaction
getOtherAccounts :: Posting -> [T.Text]
getOtherAccounts p =
  let accounts = map paccount (fromMaybe [] (tpostings <$> ptransaction p))
  in filter (/= paccount p) accounts