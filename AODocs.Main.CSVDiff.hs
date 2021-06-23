{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AODocs.Main.Workflow   
   Description : This parse AODocs Library JSON files inot CSV export files 
   Copyright   : ( c ) Matthew Lawler 2020   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Tests: 
     Count 
     Key test - common ids such title and DocumentID 
     Dependent data test - not much as there is only key data input 
   
 -}

import Data.Csv as C 
-- (FromNamedRecord, ToNamedRecord, DefaultOrdered, decodeByName, encodeDefaultOrderedByName) 
import Data.Text as T (Text, unpack) 
import Data.Vector as V (Vector, singleton, toList) 
import Data.Either 
import GHC.Generics 
import qualified Data.ByteString.Lazy as B 
--(ByteString, readFile, writeFile)
import Data.Map.Strict as M (Map, fromAscList, lookup) 
import Data.List  
import Data.Ord   
import Data.Maybe  

import CassavaUtils 

--  This is the 4 basic columns for export from a doc class or library 
data AODocsBUExport4 =  
    AODocsBUExport4 { 
               title :: !Text
             , className :: !Text   
             , documentID :: !Text  
             , libraryID :: !Text 
          } deriving (Show,Generic) 

instance FromNamedRecord AODocsBUExport4
instance ToNamedRecord AODocsBUExport4
instance DefaultOrdered AODocsBUExport4     

aODocsBUExport4DefaultValue = AODocsBUExport4 "" "" "" ""  
aODocsBUExport4Default = singleton aODocsBUExport4DefaultValue   

decodeAODocsBUExport4 :: B.ByteString -> Either String (Vector AODocsBUExport4) 
decodeAODocsBUExport4 = fmap snd . decodeByName 

decodeAODocsBUExport4FromFile :: FilePath -> IO (Either String (Vector AODocsBUExport4)) 
decodeAODocsBUExport4FromFile filepath = 
    catchShowIO (B.readFile filepath)
      >>= return . either Left decodeAODocsBUExport4

data AODocsCountTest =  
    AODocsCountTest { 
               classNameCount :: Text    
             , countBefore1AM :: Int   
             , countBeforeXAM :: Int 
             , countAfter1AM :: Int 
             , isCountEqual :: Bool 
             , countDiff :: Int 
          } deriving (Show,Generic) 

instance FromNamedRecord AODocsCountTest
instance ToNamedRecord AODocsCountTest
instance DefaultOrdered AODocsCountTest       

-- VIP Use clean function in excel to get rid of unprintable characters 
-- VIP Then use Notep++ Search -> Find cahracters in range, and remove manually 
-- | Location of the local copy, in case you have it, of the JSON file.
csvFile1AMBefore :: FilePath
csvFile1AMBefore = "1AM SAS.csv" 

csvFileXAM :: FilePath
csvFileXAM = "2AM SAS.csv" 

csvFile1AMAfter :: FilePath
csvFile1AMAfter = "1AM SAS After.csv" 

--  last is unsafe?? 
countTest :: [AODocsBUExport4] -> [AODocsBUExport4] -> [AODocsBUExport4] -> AODocsCountTest 
countTest a1 ax aa1  
  = AODocsCountTest (className (last a1)) la lx laA ((la + lx) == laA) (laA - (la + lx))  
      where  
        la = (length a1) 
        lx = (length ax) 
        laA = (length aa1) 

sortAODocsBUExport4 :: [AODocsBUExport4] -> [(String,AODocsBUExport4)]
sortAODocsBUExport4 ps = 
  sortBy ( comparing fst ) (map (\p -> (unpack (title p), p)) ps) 

makeAODocsBUExport4Map :: [AODocsBUExport4] -> Map String AODocsBUExport4 
makeAODocsBUExport4Map ds = fromAscList (sortAODocsBUExport4 ds) 

data AODocsBUExport4Pair =  
    AODocsBUExport4Pair { 
               key :: String  
             , classNamePair :: Text     
             , title1 :: Text
             , documentID1 :: Text  
             , libraryID1 :: Text 
             , title2 :: Text
             , documentID2 :: Text  
             , libraryID2 :: Text    
             , equalLibraryID :: Bool    
             , equalDocumentID :: Bool                                       
          } deriving (Show,Generic) 

instance FromNamedRecord AODocsBUExport4Pair
instance ToNamedRecord AODocsBUExport4Pair
instance DefaultOrdered AODocsBUExport4Pair   

compareAODocsBUExport4 :: AODocsBUExport4 -> Map String AODocsBUExport4 -> AODocsBUExport4Pair  
compareAODocsBUExport4 p1 mp2 = 
  AODocsBUExport4Pair k (className p1) 
    (title p1) (documentID p1) (libraryID p1)
    (title p2) (documentID p2) (libraryID p2)  
    ((libraryID p1) == (libraryID p2))     
    ((v p1) == (v p2)) 
      where 
        k = unpack (title p1) 
        p2 = fromMaybe aODocsBUExport4DefaultValue (M.lookup k mp2) 
--        v p = documentID p <> libraryID p 
        v p = documentID p  

main :: IO ()
main = do

    file1AMBefore <-  (decodeAODocsBUExport4FromFile csvFile1AMBefore )  
    fileXAM <-  (decodeAODocsBUExport4FromFile csvFileXAM )     
    file1AMAfter <-  (decodeAODocsBUExport4FromFile csvFile1AMAfter )  

--  extract into the data type lists 
    let a1 = toList (fromRight aODocsBUExport4Default file1AMBefore) 
    let ax = toList (fromRight aODocsBUExport4Default fileXAM)     
    let aa1 = toList (fromRight aODocsBUExport4Default file1AMAfter)   

--  very simple encoding with Header 
    let file1AMBeforeOut = encodeDefaultOrderedByName a1 
    let fileXAMOut = encodeDefaultOrderedByName ax 
    let file1AMAfterOut = encodeDefaultOrderedByName aa1 

-- do count test     
    let ct = countTest a1 ax aa1  
    let ctcsv = encodeDefaultOrderedByName [ct] 

-- use title to lookup on each document 
-- Many DocIds are missing, so this cannot be used as a lookup 
    let a1x = a1 ++ ax 

    let a1xMap = makeAODocsBUExport4Map a1x 
    let aa1Map = makeAODocsBUExport4Map aa1 

    let am1toamXTitle = map (\f -> compareAODocsBUExport4 f aa1Map) a1x  
    let amXtoam1Title = map (\f -> compareAODocsBUExport4 f a1xMap) aa1  

    let fs1 = C.encodeDefaultOrderedByName am1toamXTitle 
    let fsX = C.encodeDefaultOrderedByName amXtoam1Title  

--    print file1AMBefore 
--    print fileXAM  
--    print ct 
--  check that the csv files can be read, and there are no special chars 
--    B.writeFile "file1AMBeforeOut.csv" file1AMBeforeOut 
--    B.writeFile "fileXAMOut.csv" fileXAMOut 
--    B.writeFile "file1AMAfterOut.csv" file1AMAfterOut 

--  write out the test 
    B.writeFile "countTest.csv" ctcsv  
    B.writeFile "am1toamXTitle.csv" fs1  
    B.writeFile "amXtoam1Title.csv" fsX  
