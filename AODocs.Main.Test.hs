{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AODocs.Main.Library  
   Description : This parse AODocs Library JSON files inot CSV export files 
   Copyright   : ( c ) Matthew Lawler 2020   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com
   
 -}

import Data.Aeson
import Data.List as L 
import Data.Either 
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Data.Csv as C 
import Data.Map.Strict as M 
import Data.Ord  
import Data.Maybe 

import AODocsCommon
import AODocsDocClass 


-- | Location of the local copy, in case you have it, of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "0 DocClass.json" 
--jsonFileFrom = "0 Libraries.json" 

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom


main :: IO ()
main = do
 -- Get JSON data from file 1  decode it
 dcIn <- (eitherDecode <$> getJSONFrom) :: IO (Either String AODocsJSONDocClass)  
-- bdc <- getJSONFrom 

 let dc = fromRight aODocsJSONDocClassDefault dcIn  

--   Scoring 
 putStrLn $ "\nTesting \n "
-- putStrLn $ "prop_inverse_B_to_JSON_to_B B.empty dc = exp True " ++ show ( prop_inverse_B_to_JSON_to_B B.empty bdc ) ++ "\n" 
 putStrLn $ "prop_inverse_JSON_to_B_to_JSON aODocsJSONDocClassDefault dc = exp True " ++ 
      show ( prop_inverse_JSON_to_B_to_JSON aODocsJSONDocClassDefault dc ) ++ "\n" 


-- B.writeFile "alldocClassPermsCompare.csv" fs 
