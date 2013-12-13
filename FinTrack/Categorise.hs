module FinTrack.Categorise (setCategory, categorise,exactMatch) where

import qualified Data.ByteString.Char8 as S
import Crm.Crm
import FinTrack.Transaction

setCategory :: CrmHandle -> [Transaction] -> [Transaction] -> IO [Transaction]
setCategory h oldTs = mapM (\t -> case tCategory t of {NoCategory ->categorise h oldTs t; Category _ -> return t}) 

categorise :: CrmHandle -> [Transaction] -> Transaction -> IO Transaction
categorise h ts t = 
  case exactMatch ts t of
    NoCategory  -> do 
                   cResult <- classify h (S.unpack $ tDescription t)
                   case cResult of 
                     Nothing -> return t
                     Just c -> return (t {tCategory = readCategory c})
    c  -> return (t {tCategory = c} )

-- TODO should choose latest by date
exactMatch :: [Transaction] -> Transaction -> Category
exactMatch ts t = case matches of {[]->NoCategory;(c:_) -> c}
  where matches = [tCategory x | x <- ts, tDescription t == tDescription x, tCategory x /= NoCategory]

