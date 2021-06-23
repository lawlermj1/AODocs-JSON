{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AODocs.Main.Workflow   
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
import AODocsWorkflow 


-- | Location of the local copy, in case you have it, of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "0 Workflows.json" 
-- jsonFileFrom = "AM3.Workflows.json" 

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom

-- extract the view list 
getWorkflows :: AODocsJSONWorkflow -> [AODocsWorkflow] 
getWorkflows jl = sort (js_workflows jl) 

getAODocsWFPerm :: AODocsWorkflow -> [AODocsPermissionV2WFOut] 
getAODocsWFPerm w = 
      concat 
        (L.map 
          (\s -> 
            L.map 
              (\p -> AODocsPermissionV2WFOut 
                (libraryId w ) 
                (className w) 
                (name w) 
                (ws_name s) 
                (pm_displayName p) 
                (show (pm_type p)) 
                (show (pm_role p)) 
                (pm_value p)
              ) (ps s) 
           ) (states w)
         )  
          where ps s = fromMaybe [aODocsPermissionV2Default] (ws_permissions s) 

keyAODocsWFPerm :: AODocsPermissionV2WFOut -> String 
keyAODocsWFPerm p = pwo_className p ++ pwo_WorkflowName p ++ pwo_WFStateName p ++ pwo_displayName p 

sortAODocsWFPerm :: [AODocsPermissionV2WFOut] -> [(String,AODocsPermissionV2WFOut)]
sortAODocsWFPerm ps = 
  sortBy ( comparing fst ) (L.map (\p -> (keyAODocsWFPerm p, p)) ps)     

makeAODocsWFPermMap :: [AODocsPermissionV2WFOut] -> Map String AODocsPermissionV2WFOut 
makeAODocsWFPermMap ds = M.fromAscList (sortAODocsWFPerm ds) 

compareAODocsWFPerm :: AODocsPermissionV2WFOut -> Map String AODocsPermissionV2WFOut -> AODocsPermissionV2WFPairOut 
compareAODocsWFPerm p1 mp2 = 
  AODocsPermissionV2WFPairOut k 
    (pwo_className p1) (pwo_WorkflowName p1) (pwo_WFStateName p1) (pwo_displayName p1)
    (pwo_libraryId p1) (pwo_type p1) (pwo_role p1) (pwo_value p1) 
    (pwo_libraryId p2) (pwo_type p2) (pwo_role p2) (pwo_value p2) 
    ( ((v p1) == (v p2))) 
      where 
        k = keyAODocsWFPerm p1 
        p2 = fromMaybe aODocsPermissionV2WFOutDefault (M.lookup k mp2) 
        v p = pwo_type p ++ pwo_role p ++ pwo_value p 

main :: IO ()
main = do
 -- Get JSON data from file 1  decode it
 f1 <- (eitherDecode <$> getJSONFrom) :: IO (Either String AODocsJSONWorkflow)  
 let jl1 = fromRight aODocsJSONWorkflowDefault f1 

 let wfs1 = getWorkflows jl1 

 let wsPerms = concat (L.map getAODocsWFPerm wfs1) 

 let am1wfs = L.filter (\w -> (libraryId w) == am1LibraryId && elem (className w) docClassMove) wfs1 
 let amXwfs = L.filter (\w -> elem (libraryId w) amXLibraryId && elem (className w) docClassMove) wfs1  

 let am1Perms = concat (L.map getAODocsWFPerm am1wfs) 
 let amXPerms = concat (L.map getAODocsWFPerm amXwfs)  

 let am1WFPermmap = makeAODocsWFPermMap am1Perms 
 let amXWFPermmap = makeAODocsWFPermMap amXPerms 

 let am1toamXField = L.map (\f -> compareAODocsWFPerm f amXWFPermmap) am1Perms  
 let amXtoam1Field = L.map (\f -> compareAODocsWFPerm f am1WFPermmap) amXPerms  
 
-- let fs = C.encodeDefaultOrderedByName am1toamXField  
 let fs = C.encodeDefaultOrderedByName amXtoam1Field         
-- print f1       
-- print am1Perms       

 B.writeFile "workflowPerms.csv" fs 


