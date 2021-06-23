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

-- extract the view list 
getDocClasses :: AODocsJSONDocClass -> [AODocsDocClass] 
getDocClasses jl = sort (js_items jl) 

getAODocsDocClassField :: AODocsDocClass -> [AODocsDocClassFieldOut] 
getAODocsDocClassField d = 
      L.map 
        (\f -> AODocsDocClassFieldOut 
            (fi_displayName f) 
            ((show.fi_type) f) 
            (name d) 
            (libraryId d) 
            (show (fi_multiple f))
            (show (fi_readOnly f))
            (show (fi_mandatory f))
            (show (fi_hidden f))
            (show (fi_rank f))
            (show (fi_folder f))
            (show (fi_dynamicValues f)) 
         ) (fields d) 

-- fio_libraryId :: String fio_name :: String fio_displayName ::  String fio_type :: String 

sortAODocsDocClassField :: [AODocsDocClassFieldOut] -> [(String,AODocsDocClassFieldOut)]
sortAODocsDocClassField ds = sortBy ( comparing fst ) (L.map (\d -> (fio_name d ++ fio_displayName d, d)) ds) 

-- fromAscList :: Eq k => [(k, a)] -> Map k a 
makeAODocsDocClassFieldMap :: [AODocsDocClassFieldOut] -> Map String AODocsDocClassFieldOut 
makeAODocsDocClassFieldMap ds = M.fromAscList (sortAODocsDocClassField ds) 

compareAODocsDocClassFieldOut :: AODocsDocClassFieldOut -> Map String AODocsDocClassFieldOut -> AODocsDocClassFieldPairOut 
compareAODocsDocClassFieldOut f1 mf2 = 
  AODocsDocClassFieldPairOut k 
    (fio_name f1) (fio_displayName f1) 
    (fio_libraryId f1) (fio_type f1) (fio_multiple f1) (fio_readOnly f1) (fio_mandatory f1) (fio_hidden f1) (fio_rank f1) (fio_folder f1) (fio_dynamicValues f1) 
    (fio_libraryId f2) (fio_type f2) (fio_multiple f2) (fio_readOnly f2) (fio_mandatory f2) (fio_hidden f2) (fio_rank f2) (fio_folder f2) (fio_dynamicValues f2)
    (show ((fio_type f1) == (fio_type f2))) 
      where 
        k = fio_name f1 ++ fio_displayName f1 
        f2 = fromMaybe aODocsDocClassFieldOutDefault (M.lookup k mf2) 

getAODocsDocClassCreators :: AODocsDocClass -> [AODocsPermissionV2Out] 
getAODocsDocClassCreators d = 
      L.map 
        (\p -> AODocsPermissionV2Out 
            (libraryId d) 
            (name d) 
            "creators"
            (pm_displayName p)    
            (show (pm_type p))  
            (show (pm_role p)) 
            (pm_value p)  
         ) (creators d) 

getAODocsDocClassPermissions :: AODocsDocClass -> [AODocsPermissionV2Out] 
getAODocsDocClassPermissions d = 
      L.map 
        (\p -> AODocsPermissionV2Out 
            (libraryId d) 
            (name d) 
            "permissions"
            (pm_displayName p) 
            (show (pm_type p))  
            (show (pm_role p))  
            (pm_value p)  
         ) (permissions d) 

getAODocsDocClassPermissionsAll :: AODocsDocClass -> [AODocsPermissionV2Out] 
getAODocsDocClassPermissionsAll d = (getAODocsDocClassCreators d) ++ (getAODocsDocClassPermissions d) 

keyAODocsDocClassPermissions :: AODocsPermissionV2Out -> String 
keyAODocsDocClassPermissions p = p2o_dcname p ++ p2o_CorP p ++ p2o_displayName p 

sortAODocsDocClassPermissions :: [AODocsPermissionV2Out] -> [(String,AODocsPermissionV2Out)]
sortAODocsDocClassPermissions ps = 
  sortBy ( comparing fst ) (L.map (\p -> (keyAODocsDocClassPermissions p, p)) ps) 

-- fromAscList :: Eq k => [(k, a)] -> Map k a 
makeAODocsDocClassPermissionsMap :: [AODocsPermissionV2Out] -> Map String AODocsPermissionV2Out 
makeAODocsDocClassPermissionsMap ds = M.fromAscList (sortAODocsDocClassPermissions ds) 

compareAODocsDocClassPermissions :: AODocsPermissionV2Out -> Map String AODocsPermissionV2Out -> AODocsPermissionV2PairOut 
compareAODocsDocClassPermissions p1 mp2 = 
  AODocsPermissionV2PairOut k 
    (p2o_dcname p1) (p2o_CorP p1) (p2o_displayName p1) 
    (p2o_libraryId p1) (p2o_type p1) (p2o_role p1) (p2o_value p1) 
    (p2o_libraryId p2) (p2o_type p2) (p2o_role p2) (p2o_value p2) 
    (show ((v p1) == (v p2))) 
      where 
        k = keyAODocsDocClassPermissions p1 
        p2 = fromMaybe aODocsPermissionV2OutDefault (M.lookup k mp2) 
        v p = p2o_type p ++ p2o_role p ++ p2o_value p 

main :: IO ()
main = do
 -- Get JSON data from file 1  decode it
 f1 <- (eitherDecode <$> getJSONFrom) :: IO (Either String AODocsJSONDocClass)  
 let jl1 = fromRight aODocsJSONDocClassDefault f1 

 let dcs1 = getDocClasses jl1 
 let am1dcs = L.filter (\d -> (libraryId d) == am1LibraryId && elem (name d) docClassMove) dcs1 
 let amXdcs = L.filter (\d -> elem (libraryId d) amXLibraryId && elem (name d) docClassMove) dcs1 

 let am1dcfs = concat (L.map getAODocsDocClassField am1dcs) 
 let amXdcfs = concat (L.map getAODocsDocClassField amXdcs)  
 
 let am1Fieldmap = makeAODocsDocClassFieldMap am1dcfs 
 let amXFieldmap = makeAODocsDocClassFieldMap amXdcfs 

 let am1toamXField = L.map (\f -> compareAODocsDocClassFieldOut f amXFieldmap) am1dcfs  
 let amXtoam1Field = L.map (\f -> compareAODocsDocClassFieldOut f am1Fieldmap) amXdcfs   

-------- Permissions 
 let allPerms = concat (L.map getAODocsDocClassPermissionsAll dcs1) 

 let am1Perms = concat (L.map getAODocsDocClassPermissionsAll am1dcs)  
 let amXPerms = concat (L.map getAODocsDocClassPermissionsAll amXdcs)  
 
 let am1Permsmap = makeAODocsDocClassPermissionsMap am1Perms 
 let amXPermsmap = makeAODocsDocClassPermissionsMap amXPerms  

 let am1toamXPerms = L.map (\f -> compareAODocsDocClassPermissions f amXPermsmap) am1Perms   
 let amXtoam1Perms = L.map (\f -> compareAODocsDocClassPermissions f am1Permsmap) amXPerms 

-- let fs = C.encodeDefaultOrderedByName (getAODocsDocClassField (last dcs1Technical)) 
-- let fs = C.encodeDefaultOrderedByName am1toamX  
-- let fs = C.encodeDefaultOrderedByName amXtoam1  
-- let fs = C.encodeDefaultOrderedByName am1Perms     
-- let fs = C.encodeDefaultOrderedByName am1toamXPerms    
 let fs = C.encodeDefaultOrderedByName amXtoam1Perms       
-- print f1       
-- print am3sort     

-- B.writeFile "docClassField.csv" fs 
-- B.writeFile "docClassFieldCompare.csv" fs 
-- B.writeFile "alldocClassPerms.csv" fs 
 B.writeFile "alldocClassPermsCompare.csv" fs 
