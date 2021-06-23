{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AODocs.Main.Library  
   Description : This parse AODocs Library JSON files inot CSV export files 
   Copyright   : ( c ) Matthew Lawler 2020   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com
   
 -}

import Data.Aeson
import Data.List 
import Data.Either 
import qualified Data.ByteString.Lazy as B
import Control.Monad

import Data.Csv as C 

import AODocsLibrary   

-- | Location of the local copy, in case you have it, of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "0 Libraries.json" 
--jsonFileFrom = "0 Libraries.json" 
--jsonFileFrom = "0 Libraries.test2.json" 

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom

-- extract the view list 
getLibraries :: AODocsJSONLibrary -> [AODocsLibrary] 
getLibraries jl = sort (libraries jl) 

-- extract library ids  
getLibraryOut :: AODocsLibrary -> AODocsLibraryOut
getLibraryOut lb = AODocsLibraryOut 
          (name lb) 
          (storageAdmin lb) 
          (rootFolderId lb) 
          (libraryId lb) 
          (getAODocsFoldertype (attachmentMode lb)) 
          (show (attachmentMode lb))
          (show (permissionMode lb)) 
          (show (notifyUsersAboutPushToMyDrive lb)) 
          (show (onlyAdminsCanManageFolders lb)) 
          (show (onlyAdminsCanEditRootFolder lb)) 
          (show (daysBeforeDelete lb)) 
          (timeZone lb) 

-- extract library all   
getLibraryAll :: AODocsLibrary -> AODocsLibraryOutAll
getLibraryAll lb = AODocsLibraryOutAll 
          (kind lb)
          (libraryId lb)
          (name lb)
          (name_i18n lb)
          (domainName lb)
          (lastModified lb)
          (lastConfigModified lb)
          (createdAt lb)
          (homeUrl lb)
          (show (daysBeforeDelete lb)) 
          (welcomeText lb)
          (storageAdmin lb)
          (show (pushToMyDrive lb))
          (show (notifyUsersAboutPushToMyDrive lb)) 
          (show (onlyAdminsCanManageFolders lb))
          (show (onlyAdminsCanEditRootFolder lb))
          (defaultDocumentType lb)
          (rootFolderId lb)
          (creator lb)
          (show (forwardRequestAccessRole lb)) 
          (trashResourceId lb)
          (show (attachmentMode lb))
          (show (permissionMode lb)) 
          (show (defaultOnlyAdminCanDelete lb))
          (show (defaultOnlyAdminCanShare lb))
          (show(currentUserRight lb)) 
          (sequenceId lb)
          (show (restrictedDownload lb))
          (locale lb)
          (timeZone lb)
          (show (favorited lb)) 

-- names 
libraryNames :: AODocsJSONLibrary -> [String] 
libraryNames jl = map name (getLibraries jl) 

-- extract the permissions list AODocsPermissionOut 
getPermissions :: AODocsLibrary -> [AODocsPermissionOut] 
getPermissions lb = 
       map (\l -> AODocsPermissionOut (name lb) (pm_type l) (role l) (value l)) (permissions lb) 


main :: IO ()
main = do
 -- Get JSON data from file 1  decode it
 f1 <- (eitherDecode <$> getJSONFrom) :: IO (Either String AODocsJSONLibrary)  
 let jl1 = fromRight aODocsJSONLibraryDefault f1 

 let ls1 = getLibraries jl1 

 let libout = C.encodeDefaultOrderedByName (map getLibraryOut ls1) 

 let permsout = C.encodeDefaultOrderedByName (concat (map getPermissions ls1)) 

 let liboutall = C.encodeDefaultOrderedByName (map getLibraryAll ls1) 

-- print f1       
 B.writeFile "libout.csv" libout  
