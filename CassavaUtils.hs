{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

{- |
   Module      : CassavaUtils
   Description : Utilities for Cassava CSV library 
   Copyright   : ( c ) Matthew Lawler 2020   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Utilities for Cassava CSV library 

   Maybe, add in diff functions. Also need a KV Map. 
   
   Best site: https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/ 
   
 -}
module CassavaUtils 
    (  

      FromField,
      ToField,
      catchShowIO, 

     ) where

import Data.Csv  
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Control.Monad 


catchShowIO
  :: IO a
  -> IO (Either String a)
catchShowIO action =
  fmap Right action
    `Exception.catch` handleIOException
  where
    handleIOException
      :: IOException
      -> IO (Either String a)
    handleIOException =
      return . Left . show

--	add in date + UUID = length 18 + enum 

--	parse 29/07/2020 23:32:55
--	default = epoch date 

instance FromField Bool where
    parseField s
        | s == "false"  = pure False
        | s == "False"  = pure False 
        | s == "0"  = pure False         
        | s == "True"  = pure True                
        | s == "true"  = pure True
        | s == "1"  = pure True 
        | otherwise = mzero 

instance ToField Bool where
    toField False = "False"
    toField True = "True"    

