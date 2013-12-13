{-# LANGUAGE TypeSynonymInstances #-}
module FinTrack.Transaction (
             Account(..)
             , Category(..)
             ,Labels
             ,Transaction(..)
             ,readCategory
             ,parseTransactions 
             ,parseImportTransactions 
             ,groupSums
             ,tSum
             ,nubCategories
             ) where

import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Lex.Double
import Text.CSV.ByteString
import FinTrack.Date
--import Debug.Trace

data Account = Account String deriving (Eq,Ord)
instance Show Account where
  show (Account a) = show a
data Transaction = Transaction { tDate::Date
                               , tAmount::Double
                               , tDescription::S.ByteString
                               , tBalance::Double
                               , tCategory::Category
                               , tLabels::Labels
                               , tAccount::Account
                               } deriving (Ord)

--TODO: should this include account?
instance Eq Transaction where
  (==) a b = tDate a == tDate b && tAmount a == tAmount b && tDescription a == tDescription b

instance Show Transaction where
  show (Transaction date amt desc balance category labels account)  = 
    intercalate "," [show date, show desc, show amt, show balance, show category, show labels, show account]

data Category = Category S.ByteString | NoCategory deriving (Eq,Ord)

instance Show Category where
  show NoCategory = "\"\""
  show (Category c) = S.unpack c

readCategory :: String -> Category
readCategory s | s == "" = NoCategory
               | otherwise = Category (S.pack s)

data Labels = Labels [S.ByteString] deriving (Eq, Ord)
instance Show Labels where
  show (Labels ls) = show $ S.intercalate (S.pack ";") ls

parseLabels :: S.ByteString -> Labels
parseLabels s = Labels $ S.split ';' s

-- this is for loading our own transactions file
parseTransactions :: S.ByteString -> [Transaction]
parseTransactions s = 
  case parseCSV s of
    Nothing -> []
    Just csv -> map recordToTransaction csv

recordToTransaction :: Record -> Transaction
recordToTransaction [dt, description, tamt, balance, category, tags, account] =
    Transaction (parseDate (DateFormat '-' YMD) (S.unpack dt)) (parseDouble tamt) description (parseDouble balance) category' (parseLabels tags) (Account (S.unpack account))
    where category' = if category == S.empty then NoCategory else Category category
recordToTransaction x = error ("Can't parse Transaction from " ++ show x)

parseImportTransactions :: Account -> S.ByteString -> [Transaction]
parseImportTransactions acc s = 
  case parseCSV s of 
    Nothing -> []
    -- TODO use header to order columns properly
    Just csv -> map (importRecordToTransaction acc dateFormat) body
      where dates = head $ transpose body
            dateFormat = detectDateFormat (map S.unpack dates)
            hasHeader = S.pack "Date" `S.isInfixOf` S.takeWhile (/= '\n') s
            body = if hasHeader then tail csv else csv

importRecordToTransaction :: Account->DateFormat -> Record -> Transaction
importRecordToTransaction acc df [dt, description, tamt, balance] = 
  Transaction (parseDate df (S.unpack dt)) (parseDouble tamt) description (parseDouble balance) NoCategory (Labels []) acc
importRecordToTransaction _ _ x = error ("Can't parse Transaction from " ++ show x)

parseDouble :: S.ByteString -> Double
parseDouble = fst . fromJust . readDouble

groupSums :: [Transaction] -> [(Category, Double)]
groupSums ts = [(c, tSum $ filter (\t -> tCategory t == c) ts )| c <- categories ] 
  where categories = nubCategories ts

nubCategories :: [Transaction] -> [Category]
nubCategories = nub . map tCategory

tSum :: [Transaction] -> Double
tSum = sum . map tAmount

