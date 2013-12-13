module FinTrack.Trainer (trainInteractive, trainBatch) where
import qualified Data.ByteString.Char8 as S
import Crm.Crm
import FinTrack.Categorise
import FinTrack.Transaction
import Data.Char
--import Debug.Trace
--TODO: still a big problem with false positives from CRM in this 
trainInteractive :: CrmHandle -> [Transaction] ->[Transaction] -> IO [Transaction] 
trainInteractive  h tsOld tsNew = repl h tsOld tsNew []

trainBatch :: CrmHandle -> [Transaction] -> IO ()
trainBatch _ [] = return ()
trainBatch h ts = do
  let cts = filter ((/=NoCategory) . tCategory) ts :: [Transaction]
  classifications <- mapM (classify h . S.unpack . tDescription) cts :: IO [Maybe String]
  let incorrects =  map fst $ filter (\(t,c) -> tCategory t /= c) $  zip cts (map (\x -> case x of {Nothing -> NoCategory; Just c->readCategory c}) classifications)
  mapM_ (\t -> trainMany h (S.unpack $ tDescription t) [show (tCategory t)]) incorrects


repl :: CrmHandle -> [Transaction] -> [Transaction] -> [Transaction]-> IO [Transaction] 
repl _ _ [] ts = return ts
repl h tsOld (t:tsNew) tsProcessed = 
  case tCategory t of   
    -- if it already has a category, skip it
    Category _ -> repl h tsOld tsNew (t:tsProcessed)
    NoCategory -> do
      -- try to categorise it, if that fails, ask the user to decide
      ct <- categorise h (tsOld++tsProcessed) t
      case tCategory ct of 
        NoCategory -> do
          putStrLn ("Unknown Category:\""++ show t ++ "\"")
          correctTag <- fmap (takeWhile (not.isSpace) . dropWhile isSpace) getLine 
          if correctTag == [] then repl h tsOld tsNew (t:tsProcessed)
			      else do 
              trainMany h (S.unpack $ tDescription t) [correctTag]
              -- TODO show user unclassified transactions that will be classified as correctTag
              repl h tsOld tsNew (t{tCategory = readCategory correctTag}:tsProcessed)
        _ -> repl h tsOld tsNew (ct:tsProcessed)
       
