module FinTrack.Date (Date
                      , Month
                      , DateFormat(..)
                      , DateOrder(..)
                      , parseDate
                      , parseMonth
                      , detectDateFormat
                      , getMonth
                      ) where 

import Data.Char
import Data.List
import Data.List.Split
import Data.Ord

-- Date in year month day 
data Date = Date Int Int Int deriving (Eq,Ord)
instance Show Date where
  show (Date y m d) = intercalate "-" $ map show [y,m,d]

-- Month is year month
data Month = Month Int Int deriving (Eq,Ord)
instance Show Month where
  show (Month y m) = monthName m ++ " " ++ show y
    where monthName n | n <13 && n>0= ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"] !! (n - 1)
                      | otherwise = "xxx"

parseMonth :: String -> Month
parseMonth s = g $ map read $ splitOn "-" s
  where g [a,b] = Month a b
        g _ = error ("Unable to parse month from:["++s++"]")

getMonth :: Date -> Month
getMonth (Date y m _) = Month y m

-- dates can be in ymd, dmy, mdy format, need a function to detect which is in use
data DateFormat = DateFormat { dfDelim::Char, dfDateOrder :: DateOrder} deriving (Eq, Show)

data DateOrder = DMY | MDY | YMD deriving (Eq,Ord, Show)

parseDate :: DateFormat -> String -> Date
parseDate df s = g $ map read (splitOn [dfDelim df] s)
  where g [a,b,c] | dfDateOrder df == DMY = Date c b a 
                  | dfDateOrder df == MDY = Date c a b 
                  | otherwise = Date a b c 
        g _       = error ("Unable to parse date:"++s)

detectDateFormat :: [String] -> DateFormat
detectDateFormat ds = DateFormat {dfDelim = delim, dfDateOrder = dateOrder} 
  where delim = topElem $ filter (\x -> not (isDigit x) && x `notElem` "\",$!" ) $ concat ds
        topElem = head . maximumBy (comparing length) . group . sort
        [as, bs, cs] = transpose $ map (map read . splitOn [delim]) ds
        dateOrder = detectDateOrder (as,bs,cs) 

detectDateOrder :: ([Int],[Int],[Int]) -> DateOrder
detectDateOrder (as,bs,cs) = head $ sort (yearHeuristic possibilities)
  where exclude = map snd $ filter fst [(notDay as || notMonth bs,DMY), (notMonth as || notDay bs,MDY), (notMonth bs || notDay cs,YMD)]
        possibilities = [DMY,YMD,MDY] \\ exclude
        yearHeuristic p = if YMD `elem` p && (DMY `elem` p || MDY `elem` p) then 
                            if length (nub as) < length (nub cs) then [YMD]
                              else p \\ [YMD]
                          else p
        notMonth ns = maximum ns > 12
        notDay ns = maximum ns > 31
