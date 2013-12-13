{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs
import System.IO
import System.Directory
import System.FilePath
import Data.List
import Data.List.Split
import Data.Ord
import qualified Data.ByteString.Char8 as S
import Text.Printf
import FinTrack.Transaction
import FinTrack.Trainer
import FinTrack.Categorise
import FinTrack.Date
import Crm.Crm 

-- TODO: optimize import workflow, better looking interface and fewer keystrokes, easier to fix mistakes
-- TODO: still having issues with false positive classifications, very irritating
-- TODO: account created automatically on import, if user misspells account name this is a problem
-- TODO: add undo import feature, would remove all transactions imported in last import
-- TODO: fix cmdargs annotations so help looks good
-- TODO: Auto-complete categories when user is inputting them
-- TODO: Consider adding ability to record budget info
-- TODO: add transaction ids and amend command
-- 
-- TODO: add forecast mode to project expenditure over future period based on recurring and trends.

data FinTrack = FinTrackHelp 
              | FinTrackAdd{ ftAccount::String }
              | FinTrackImport { ftAccount::String, ftTransactionsFile :: FilePath } 
              | FinTrackLog { ftStartDate::String, ftEndDate::String , ftCategory::String, ftExCategory::String
                            , ftAccountList::String, ftExAccountList::String}
              | FinTrackReport { ftStartDate::String, ftEndDate::String, ftCategory::String,ftExCategory::String
                            , ftAccountList::String, ftExAccountList::String}
              | FinTrackMonthlyReport {ftStartDate::String, ftEndDate::String, ftCategory::String,ftExCategory::String
                            , ftAccountList::String, ftExAccountList::String}
              | FinTrackTrainBatch { ftTrainingFile :: FilePath}
              | FinTrackTrainInteractive { ftTrainingFile :: FilePath}
              | FinTrackReclassify 
              deriving (Data, Typeable, Show, Eq)

fintrackHelp, fintrackAdd, fintrackImport, fintrackLog, fintrackReport, fintrackMonthlyReport, fintrackTrainBatch, fintrackTrainInteractive, fintrackReclassify :: FinTrack

fintrackHelp = FinTrackHelp &= name "help"
fintrackAdd = FinTrackAdd {ftAccount = def &= name "account"} &= name "add"
fintrackImport = FinTrackImport { ftAccount = def &= name "account"
                                , ftTransactionsFile = def &= args} &= name "import"
fintrackLog = FinTrackLog { ftStartDate = def &= name "start"
                          , ftEndDate = def &= name "end"
                          , ftCategory=def &= name "category"
                          , ftExCategory=def &= name "excategory"
                          , ftAccountList=def &= name "accounts"
                          , ftExAccountList=def &= name "exaccounts"} &= name "log"
fintrackReport = FinTrackReport { ftStartDate = def &= name "start"
                                , ftEndDate = def &= name "end"
                                , ftCategory=def &= name "category"
                                , ftExCategory=def &= name "excategory"
                                , ftAccountList=def &= name "accounts"
                                , ftExAccountList=def &= name "exaccounts"} &= name "report"
fintrackMonthlyReport = FinTrackMonthlyReport { ftStartDate = def &= name "start"
                                , ftEndDate = def &= name "end"
                                , ftCategory=def &= name "category"
                                , ftExCategory=def &= name "excategory"
                                , ftAccountList=def &= name "accounts"
                                , ftExAccountList=def &= name "exaccounts"} &= name "monthlyreport"
fintrackTrainBatch = FinTrackTrainBatch { ftTrainingFile = def &= args } &= name "trainbatch" 
fintrackTrainInteractive = FinTrackTrainInteractive { ftTrainingFile = def &= args } &= name "train" 
fintrackReclassify = FinTrackReclassify &= name "reclassify"

main :: IO()
main = do
  command <- cmdArgs (modes [fintrackHelp &= auto, fintrackAdd, fintrackImport, fintrackLog, fintrackReport, fintrackMonthlyReport, fintrackTrainBatch, fintrackTrainInteractive, fintrackReclassify])
  root <- fmap (\x -> addTrailingPathSeparator $ combine x ".fintrack") getHomeDirectory
  let transFile = getTransactionsFile root
  touchFile transFile
  let crmDir = addTrailingPathSeparator $ combine root "css"
  -- TODO if the transfile has labelled content, and there are no .css files, generate the .css files from the transfile
  crmHandle <- initCrm crmDir
  case command of
    FinTrackHelp -> print "Please enter a command"
    FinTrackAdd account -> do 
      addAccount root account
      putStrLn ("added account:["++account++"]")
    FinTrackImport account importFile -> do
      putStrLn ("import file:["++importFile++"] account:["++account++"]")
      importts <- fmap (parseImportTransactions (Account account))$ S.readFile importFile
      putStrLn ("importing ["++show (length importts)++"] records\n")
      importTransactions crmHandle transFile importts 
    FinTrackLog start end category excategory accounts exaccounts -> showLog transFile start end category excategory accounts exaccounts
    FinTrackMonthlyReport start end category excategory  accounts exaccounts-> showMonthlyReport root start end category excategory accounts exaccounts
    FinTrackReport start end category excategory accounts exaccounts-> showReport root start end category excategory accounts exaccounts
    FinTrackTrainBatch trainingFile -> loadTransactions trainingFile >>= trainBatch crmHandle 
    FinTrackTrainInteractive trainingFile -> do
      tsTraining <- loadTransactions trainingFile 
      tsOld <- loadTransactions transFile 
      _<-trainInteractive crmHandle tsOld tsTraining
      putStrLn "Done."
    FinTrackReclassify -> print "not implemented yet"

-- create the file if it doesn't exist, make the directory the file is in if it doesn't exist 
touchFile :: FilePath -> IO ()
touchFile f = do
  createDirectoryIfMissing True (takeDirectory f)
  h <- openFile f AppendMode
  hClose h

getAccountsFile :: FilePath -> FilePath
getAccountsFile root = combine root "./accounts.csv"

loadTransactions :: FilePath -> IO [Transaction]
loadTransactions  = fmap parseTransactions . S.readFile 

getTransactionsFile :: FilePath -> FilePath
getTransactionsFile root = combine root "./transactions.csv"

addAccount :: FilePath -> String -> IO ()
addAccount root ac = appendFile (getAccountsFile root) (ac++"\n")

importTransactions:: CrmHandle -> FilePath -> [Transaction]-> IO ()
importTransactions h transFile tsImported = do
  tfileHandle <- openFile transFile ReadMode
  tsOld <- fmap parseTransactions (S.hGetContents tfileHandle)
  let newTransactions = tsImported \\ tsOld
  tsNew <- setCategory h tsOld newTransactions
  let unclassifieds = reverse $ sortBy (comparing (abs . tAmount)) $ filter (\x -> tCategory x == NoCategory) tsNew
  let classifieds = tsNew \\ unclassifieds
  tsNew' <- trainInteractive h tsOld unclassifieds
  let newContent = unlines $ map show (tsNew' ++ classifieds)
  hClose tfileHandle
  tfileHandle' <- openFile transFile AppendMode
  hPutStr tfileHandle' newContent
  hClose tfileHandle'


dateRangeFilter :: String -> String ->[Transaction]->[Transaction]
dateRangeFilter s e ts = intersect ss es
  where ss = if s/=""  then filter (\t -> getDate s<= tDate t) ts else ts
        es = if e/="" then filter (\t -> getDate e>= tDate t) ts else ts 
        getDate = parseDate (DateFormat '-' YMD) 

categoryFilter :: String ->String-> [Transaction] -> [Transaction]
categoryFilter cs excs ts = intersect incs exs
  where getList = map readCategory . splitOn "," 
        incs = if cs /= "" then filter (\t -> tCategory t `elem` getList cs) ts else ts
        exs = if excs /= "" then filter (\t -> tCategory t `notElem` getList excs) ts else ts  

accountFilter ::String->String->[Transaction]->[Transaction]
accountFilter acs exacs ts = intersect incs exs
  where getList = splitOn ","
        incs = if acs /= "" then filter (\t -> tAccount t `elem` map Account (getList acs)) ts else ts
        exs = if exacs /= "" then filter (\t -> tAccount t `notElem` map Account (getList exacs)) ts else ts

filterAll :: String->String->String->String->String->String->[Transaction]->[Transaction]
filterAll s e cs excs acs exacs ts = accountFilter acs exacs $ categoryFilter cs excs $ dateRangeFilter s e ts

showLog :: FilePath -> String -> String ->String->String->String->String-> IO ()
showLog transFile s e cs excs acs exacs= do
  ts <- loadTransactions transFile 
  let selection = sort $ filterAll s e cs excs acs exacs ts
  mapM_ print selection
 
showReport :: FilePath -> String-> String -> String ->String-> String->String -> IO()
showReport root s e cs excs acs exacs = do
  ts <- loadTransactions (getTransactionsFile root)
  let selection = filterAll s e cs excs acs exacs ts
  let stotals = reverse $ sortBy (comparing snd) $ groupSums selection
  mapM_ (\(category,total) -> printf "%+20s %+10.2f\n" (show category) total) stotals
  printf "%+20s %+10.2f\n" "Total" (sum (map snd stotals))

showMonthlyReport :: FilePath -> String-> String -> String->String-> String->String->IO ()
showMonthlyReport root s e cs excs acs exacs= do
  ts' <- loadTransactions (getTransactionsFile root)
  let selection = filterAll s e cs excs acs exacs ts'
  let stotals = reverse $ sortBy (comparing snd) $ groupSums selection
  let categories = nub (map fst stotals)
  let months = sort $ nub $ map (getMonth . tDate) selection
  _<-printHeader months
  mapM_ (printRow selection months) categories
  printGrandTotals months selection
  where printRow selection ms c = do 
          _ <- printf "%+20s" (show c)
          mapM_ (\m -> printf "%+10.2f" (totalBy c m selection)) ms
          let categoryTotal = sum $ map (\m -> totalBy c m selection) ms
          printf "%+10.2f\n" categoryTotal
        printHeader ms = do
          _ <- printf "%+20s" "Catgory"
          mapM_ (printf "%+10s" . show) ms
          printf "%+10s\n" "Total"
        printGrandTotals ms selection = do
          _<-printf "%+20s" "Total"
          mapM_ (printf "%+10.2f" . flip totalForMonth selection) ms
          printf "%+10.2f\n" (tSum selection)

totalForMonth :: Month -> [Transaction] -> Double
totalForMonth m ts = tSum $ filter ((==m) . getMonth . tDate) ts

totalBy :: Category -> Month -> [Transaction] -> Double
totalBy c m ts' = tSum ts
  where ts = filter (\x -> m == getMonth (tDate x) && c == tCategory x) ts'
        
