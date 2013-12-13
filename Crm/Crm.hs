module Crm.Crm(
          CrmHandle(..)
          , Tag
          , Content
          , initCrm
          , label
          , classify
          , train
          , trainMany
          , readTags
          ) where

import System.FilePath
import Data.List
import HSH
import System.Directory
import Data.Ord
--import Debug.Trace

data CrmHandle = CrmHandle {crmhDir::FilePath} deriving (Eq, Show)
type Tag = String
type Content = String
type Pr = Double
type Prob = Double

initCrm :: FilePath -> IO CrmHandle
initCrm crmDir = do
  let h = CrmHandle crmDir 
  createDirectoryIfMissing True crmDir
  return h

-- find all the labels that are reasonably likely to apply to the content
-- test each possible tag against its non-tag class, return any that are more likely than not to apply
label :: CrmHandle -> Content -> IO [Tag]
label h content = do
  let crmDir = crmhDir h
  tags <- readTags h
  results <- mapM (\x -> crmClassify crmDir (genNons [x]) content) tags
  -- matches with probability > 0.6, and pr value > 0.01, in descending order of match likelihood (i.e. highest to lowest)
  let matches = reverse $ sortBy (comparing snd) [(t,pr) | (t, prob, pr) <- results, take 3 t /= "non", pr > 0.01 && prob > 0.6]
  let matchingTags = map fst matches
  return matchingTags

-- choose the best match from the labels that are more likely than not to apply
minProbThreshold, minPrThreshold :: Double
minProbThreshold = 0.8
minPrThreshold = 10.0
classify :: CrmHandle -> Content -> IO (Maybe Tag)
classify h content = do
  let crmDir = crmhDir h
  tags <- label h content
  if tags /= [] then 
    do (t, prob, pr) <- crmClassify crmDir tags content 
       count <- documentCount h t 
       if prob > minProbThreshold && pr > minPrThreshold && count > 4 then return (Just t) else return Nothing
    else return Nothing

documentCount :: CrmHandle -> Tag -> IO Int
documentCount h t = do
  let cmd = "cssutil -r -b " ++ combine (addTrailingPathSeparator (crmhDir h)) (t ++ ".css") ++" | grep 'Documents learned'"
  result <- run  cmd
  return (read (words result !! 3) :: Int)

-- run crm learn applying the tag specified to the given content
train :: CrmHandle -> Tag -> Content -> IO()
train h tag content = do
  let crmDir = crmhDir h
  let cmd = crmLearnCommand crmDir tag 
  runIO $ echo content -|- cmd

-- train crm to apply the given tags to the content, correct any mislabelling by 
-- training the "non" versions of false positives
trainMany :: CrmHandle -> Content -> [Tag] -> IO ()
trainMany h content correctTags = do
  currentTags <- label h content
  let nonTagsToApply = map ("non"++) (currentTags \\ correctTags)
  let learnTags = correctTags ++ nonTagsToApply
  mapM_ (\x -> train h x content) learnTags
  -- if the non versions of the correctTags do not yet exist, create them
  allNonTags <- readNonTags h
  let missingNonTags = map ("non"++) correctTags \\ allNonTags
  mapM_ (\x -> train h x "dummy_content_to_init_non") missingNonTags

readTags :: CrmHandle -> IO [Tag]
readTags = fmap (map dropExtension . filter (\f -> takeExtension f == ".css" && take 3 f /= "non")) . getDirectoryContents . crmhDir 

readNonTags :: CrmHandle -> IO [Tag]
readNonTags = fmap (map dropExtension . filter (\f -> takeExtension f == ".css" && "non" `isPrefixOf` f)) . getDirectoryContents . crmhDir 

-- evaluate the probability that a label applies to the content
-- TODO write a proper parser to parse the output from crm
crmClassify :: FilePath -> [Tag] -> Content -> IO(Tag, Prob, Pr)
crmClassify crmDir tags content = do
  let cmd = crmClassifyCommand crmDir tags 
  result <- run $ echo content -|-  cmd
  let bestMatchWords = words $ lines result !! 1
  let classTag = takeWhile (/='.') $ filter (`notElem` "()") $ bestMatchWords !! 5
  let prob = read (bestMatchWords !! 7) :: Double
  let pr = read (bestMatchWords !! 9) :: Double
  return (classTag, prob, pr)

genNons :: [Tag] -> [Tag]
genNons = concatMap (\x -> [x,"non"++x]) 

crmLearnCommand :: FilePath -> String -> String
crmLearnCommand crmDir tagName = "crm -C -u "++crmDir++" '-{ learn <osb unique microgroom> ( " ++ tagName ++ ".css ) /[-_[:alpha:]]+/ }'"

crmClassifyCommand :: FilePath -> [String] -> String
crmClassifyCommand crmDir tagList = "crm -C -u "++crmDir++" '-{ isolate (:stats:); classify <osb unique microgroom> (" ++ tagString  ++") (:stats:) /[-_[:alpha:]]+/ ; output /:*:stats:/}'"
  where tagString = unwords (map (++".css") tagList)

