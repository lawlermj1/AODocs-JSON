{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AODocs Views 
   Description : This parse AODocs Views JSON files into CSVs 
   Copyright   : ( c ) Matthew Lawler 2020   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com
   
 -}

import Data.Aeson
import Data.List 
import Data.Either 
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Data.Csv (encodeDefaultOrderedByName) 

import AODocsEnum
import AODocsCommon 
import AODocsView    

-- | Location of the local copy, in case you have it,
--   of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "AM2.views.json" 

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom

-- extract the view list 
getItems :: AODocsJSONView -> [AODocsView] 
getItems jv = sort (items jv) 

--	this is needed as id is used as tag name in AODocs!!! 
id1 :: a -> a 
id1 x = x 

-- view to (class name, view name, order, browseBy, visibility) 
viewTuple :: AODocsView -> AODocsViewOut
viewTuple v = AODocsViewOut 
                 (className v) 
                 (name v) 
                 ((co_name.sortByField.order) v) 
                 (maybe "" co_name (browseBy v)) 
                 (show (maybe NOTVISIBLE vi_type (visibility v)))
                 (maybe "" id1 (vi_value (maybe aODocsVisibilityDefault id1 (visibility v)))) 


-- view to (class name, view name, joinType, column, operator, value) 
viewFilters :: AODocsView -> [AODocsFilterOut] 
viewFilters v = maybe [] (\fls -> map (viewFilter (className v) (name v)) fls) (filters v) 

viewFilter :: String -> String -> AODocsFilter -> AODocsFilterOut 
viewFilter c v fl = AODocsFilterOut 
                       c
                       v
                       (show (joinType fl))
                       (co_name (column fl)) 
                       (show (operator fl))
                       (viewFilterValues (head (head (values fl)))) 

viewFilterValues :: AODocsFilterValues -> String 
viewFilterValues (AODocsFilterValue _ x _)  = x 
viewFilterValues (AODocsFilterValueFixed y) = y 

main :: IO ()
main = do
 -- Get JSON data from file 1  decode it
 f1 <- (eitherDecode <$> getJSONFrom) :: IO (Either String AODocsJSONView)  
 let jv1 = fromRight aODocsJSONViewDefault f1 

 let vt1 = map viewTuple (getItems jv1) 

 let vf1 = concat(map viewFilters (getItems jv1)) 

-- print vf1 

-- write       
 B.writeFile "vf1.csv" (encodeDefaultOrderedByName vf1) 

