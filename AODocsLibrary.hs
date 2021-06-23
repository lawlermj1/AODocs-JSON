{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AODocsLibrary
   Description : Product types for AODocs Library API 
   Copyright   : ( c ) Matthew Lawler 2020   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module defines Product types needed for AODocs JSON functions. 

   Best site: https://artyom.me/aeson 

 -}
module AODocsLibrary  
    (  
      
      AODocsLibrary( .. ), 
      AODocsPermission( .. ), 
      AODocsJSONLibrary( .. ), 
      AODocsLibraryOut( .. ), 
      AODocsLibraryOutAll( .. ), 
      AODocsPermissionOut( .. ), 

      aODocsJSONLibraryDefault, 
      getAODocsFoldertype, 

     ) where
    

import Data.Aeson
import Data.List 
import GHC.Generics
import qualified Data.HashMap.Strict as HM 
import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered, decodeByName) 
import qualified Data.ByteString.Lazy as B
import Data.Text   
import Data.Vector as V 

import CassavaUtils 

import AODocsEnum 
import AODocsCommon 

data AODocsDocType  =
  AODocsDocType { 
            dt_kind :: String
          , dt_items :: [AODocsColumn]
           } deriving (Show,Generic) 

instance ToJSON AODocsDocType where 
    toJSON (AODocsDocType dt_kind dt_items ) = 
        object ["kind" .= dt_kind,  "items" .= dt_items ] 

instance FromJSON AODocsDocType where 
    parseJSON = withObject "AODocsDocType" $ \v -> AODocsDocType
        <$> v .: "kind" 
        <*> v .: "items" 

instance Eq AODocsDocType where 
  (AODocsDocType kind1 items1 ) == (AODocsDocType kind2 items2 ) = kind1 == kind2 && items1 == items2 

instance Ord AODocsDocType where
  (AODocsDocType _ items1 ) `compare` (AODocsDocType _ items2 ) = items1 `compare` items2        

data AODocsFolder =
  AODocsFolder  { 
            fl_kind :: String
          , fl_libraryId :: String
          , id :: String
          , fl_name :: String
          , dynamicValues :: Bool
          , folderId :: String
          , onlyAdminCanManage :: Bool
          , onlyAdminCanEditRootFolder :: Bool
          , folderVisibility :: AODocsVisType 
           } deriving (Show,Generic) 

instance ToJSON AODocsFolder where 
    toJSON (AODocsFolder fl_kind fl_libraryId id fl_name dynamicValues folderId onlyAdminCanManage onlyAdminCanEditRootFolder folderVisibility) = 
        object ["kind" .= fl_kind,  "libraryId" .= fl_libraryId
               ,"id" .= id ,"name" .= fl_name 
               ,"dynamicValues" .= dynamicValues ,"folderId" .= folderId 
               ,"onlyAdminCanManage" .= onlyAdminCanManage
               ,"onlyAdminCanEditRootFolder" .= onlyAdminCanEditRootFolder
               ,"folderVisibility" .= folderVisibility               
                ] 

instance FromJSON AODocsFolder where 
    parseJSON = withObject "AODocsFolder" $ \v -> AODocsFolder
        <$> v .: "kind" 
        <*> v .: "libraryId" 
        <*> v .: "id" 
        <*> v .: "name" 
        <*> v .: "dynamicValues" 
        <*> v .: "folderId" 
        <*> v .: "onlyAdminCanManage"  
        <*> v .: "onlyAdminCanEditRootFolder"  
        <*> v .: "folderVisibility"                  

data AODocsWokflows =
  AODocsWokflows { 
            wk_kind :: String
          , workflows :: Maybe [AODocsWorkflow] 
           } deriving (Show,Generic) 

instance ToJSON AODocsWokflows where 
    toJSON (AODocsWokflows wk_kind workflows) = 
        object ["kind" .= wk_kind,  "workflows" .= workflows] 

instance FromJSON AODocsWokflows where 
    parseJSON = withObject "AODocsWokflows" $ \v -> AODocsWokflows
        <$> v .: "kind" 
        <*> v .:? "workflows"  

data AODocsWorkflow =
  AODocsWorkflow { 
            wf_kind :: String
          , wf_id :: String 
          , wf_name :: String 
          , wf_name_i18n :: String                 
          , wf_classId :: String
          , wf_states :: [AODocsWorkflowState] 
           } deriving (Show,Generic) 

instance ToJSON AODocsWorkflow where 
    toJSON (AODocsWorkflow wf_kind wf_id wf_name wf_name_i18n wf_classId wf_states) = 
        object ["kind" .= wf_kind,  "id" .= wf_id
               ,"name" .= wf_name,  "name_i18n" .= wf_name_i18n
               ,"classId" .= wf_classId,  "states" .= wf_states
               ] 

instance FromJSON AODocsWorkflow where 
    parseJSON = withObject "AODocsWorkflow" $ \v -> AODocsWorkflow
        <$> v .: "kind" 
        <*> v .: "id"  
        <*> v .: "name"
        <*> v .: "name_i18n"  
        <*> v .: "classId"
        <*> v .: "states"  

instance Eq AODocsWorkflow where 
  (AODocsWorkflow kind1 _ name1 _ _ states1) == (AODocsWorkflow kind2 _ name2 _ _ states2) 
     = kind1 == kind2 && name1 == name2 && states1 == states2  

instance Ord AODocsWorkflow where
  (AODocsWorkflow _ _ name1 _ _ _ ) `compare` (AODocsWorkflow _ _ name2  _ _ _ ) 
     = name1 `compare` name2        

data AODocsWorkflowState =
  AODocsWorkflowState { 
            ws_kind :: String
          , ws_name :: String 
          , ws_name_i18n :: String                 
          , ws_value :: String
          , ws_noAction :: Bool 
          , ws_displayFlags :: Maybe [AODocsWFDisplay] 
          , ws_transitions :: Maybe [AODocsWFTransition] 
           } deriving (Show,Generic) 

instance ToJSON AODocsWorkflowState where 
    toJSON (AODocsWorkflowState ws_kind ws_name ws_name_i18n ws_value ws_noAction ws_displayFlags ws_transitions) = 
        object ["kind" .= ws_kind, "name" .= ws_name
               ,"name_i18n" .= ws_name_i18n, "value" .= ws_value
               ,"noAction" .= ws_noAction, "displayFlags" .= ws_displayFlags 
               ,"transitions" .= ws_transitions               
        ] 

instance FromJSON AODocsWorkflowState where 
    parseJSON = withObject "AODocsWorkflowState" $ \v -> AODocsWorkflowState
        <$> v .: "kind" 
        <*> v .: "name"
        <*> v .: "name_i18n"  
        <*> v .: "value"
        <*> v .: "noAction"  
        <*> v .:? "displayFlags" 
        <*> v .:? "transitions" 

instance Eq AODocsWorkflowState where 
  (AODocsWorkflowState kind1 name1 _ _ noAction1 _ transitions1) == (AODocsWorkflowState kind2 name2 _ _ noAction2 _ transitions2) 
     = kind1 == kind2 && name1 == name2 && noAction1 == noAction2 && transitions1 == transitions2

instance Ord AODocsWorkflowState where
  (AODocsWorkflowState _ name1 _ _ _ _ _) `compare` (AODocsWorkflowState _ name2 _ _ _ _ _ ) 
     = name1 `compare` name2   

data AODocsWFTransition =
  AODocsWFTransition { 
            wt_kind :: String
          , wt_name :: Maybe String 
          , wt_name_i18n :: Maybe String                 
          , wt_id :: String
          , wt_value :: String          
          , wt_parallel :: Maybe Bool 
          , wt_fromStateId :: String 
          , wt_fromStateName_i18n :: String 
          , wt_fromStateName :: String 
          , wt_targetStateId :: String 
          , wt_targetStateName :: String 
          , wt_targetStateName_i18n :: String 
          , wt_workflowId :: Maybe String   
          , wt_condition :: AODocsWFCondition  
          , wt_mandatoryComment :: Maybe Bool   
           } deriving (Show,Generic) 

instance ToJSON AODocsWFTransition where 
    toJSON (AODocsWFTransition wt_kind wt_name wt_name_i18n wt_id wt_value wt_parallel 
            wt_fromStateId wt_fromStateName_i18n wt_fromStateName wt_targetStateId 
            wt_targetStateName wt_targetStateName_i18n wt_workflowId wt_condition wt_mandatoryComment 
           ) = 
        object ["kind" .= wt_kind, "name" .= wt_name
               ,"name_i18n" .= wt_name_i18n, "id" .= wt_id
               ,"value" .= wt_value, "parallel" .= wt_parallel               
               ,"fromStateId" .= wt_fromStateId, "fromStateName_i18n" .= wt_fromStateName_i18n 
               ,"fromStateName" .= wt_fromStateName, "targetStateId" .= wt_targetStateId 
               ,"targetStateName" .= wt_targetStateName, "targetStateName_i18n" .= wt_targetStateName_i18n 
               ,"workflowId" .= wt_workflowId 
               ,"condition" .= wt_condition, "mandatoryComment" .= wt_mandatoryComment 
               ] 

instance FromJSON AODocsWFTransition where 
    parseJSON = withObject "AODocsWFTransition" $ \v -> AODocsWFTransition
        <$> v .: "kind" 
        <*> v .:? "name"
        <*> v .:? "name_i18n"  
        <*> v .: "id"
        <*> v .: "value"  
        <*> v .:? "parallel" 
        <*> v .: "fromStateId"   
        <*> v .: "fromStateName_i18n"   
        <*> v .: "fromStateName"   
        <*> v .: "targetStateId"               
        <*> v .: "targetStateName"   
        <*> v .: "targetStateName_i18n"  
        <*> v .:? "workflowId" 
        <*> v .: "condition"   
        <*> v .:? "mandatoryComment" 

instance Eq AODocsWFTransition where 
  (AODocsWFTransition kind1 name1 _ _ _ _ _ _ _ fromStateName1 _ targetStateName1 _ condition1 _ ) 
   == (AODocsWFTransition kind2 name2 _ _ _ _ _ _ _ fromStateName2 _ targetStateName2 _ condition2 _ ) 
     = kind1 == kind2 && name1 == name2 && fromStateName1 == fromStateName2 
        && targetStateName1 == targetStateName2 && condition1 == condition2 

instance Ord AODocsWFTransition where
  (AODocsWFTransition _ name1 _ _ _ _ _ _ _ _ _ _ _ _ _ ) `compare` (AODocsWFTransition _ name2 _ _ _ _ _ _ _ _ _ _ _ _ _ ) 
     = name1 `compare` name2   

data AODocsVisibility  =
  AODocsVisibility { 
            vs_scope :: AODocsVisType
          , vs_level :: Maybe AODocsVisLevel 
           } deriving (Show,Generic) 

instance ToJSON AODocsVisibility where 
    toJSON (AODocsVisibility vs_scope vs_level ) = 
        object ["scope" .= vs_scope,  "level" .= vs_level ] 

instance FromJSON AODocsVisibility where 
    parseJSON = withObject "AODocsVisibility" $ \v -> AODocsVisibility
        <$> v .: "scope" 
        <*> v .:? "level" 

data AODocsRelation = 
  AODocsRelation   { 
            rl_kind :: String
          , rl_id :: String
          , rl_libraryId :: String
          , rl_name :: String
          , rl_name_i18n :: String      
          , rl_fromId :: String     
          , rl_toId :: String      
          , rl_fromName :: String 
          , rl_fromName_i18n :: String      
          , rl_toName :: String     
          , rl_toName_i18n :: String      
          , rl_fromSort :: Maybe String 
          , rl_fromDescending :: Bool
          , rl_toSort :: Maybe String 
          , rl_toDescending :: Bool
          , rl_fromDisplayColumns :: [AODocsColumn] 
          , rl_toDisplayColumns :: [AODocsColumn] 
          , rl_selfRelated :: Bool 
           } deriving (Show,Generic) 

instance ToJSON AODocsRelation where 
    toJSON (AODocsRelation 
            rl_kind rl_id 
            rl_libraryId rl_name 
            rl_name_i18n rl_fromId 
            rl_toId rl_fromName 
            rl_fromName_i18n rl_toName 
            rl_toName_i18n rl_fromSort 
            rl_fromDescending rl_toSort 
            rl_toDescending rl_fromDisplayColumns
            rl_toDisplayColumns rl_selfRelated) = 
        object ["kind" .= rl_kind,  "id" .= rl_id 
               ,"libraryId" .= rl_libraryId ,"name" .= rl_name 
               ,"name_i18n" .= rl_name_i18n ,"fromId" .= rl_fromId 
               ,"toId" .= rl_toId ,"fromName" .= rl_fromName 
               ,"fromName_i18n" .= rl_fromName_i18n ,"toName" .= rl_toName 
               ,"toName_i18n" .= rl_toName_i18n ,"fromSort" .= rl_fromSort 
               ,"fromDescending" .= rl_fromDescending ,"toSort" .= rl_toSort 
               ,"toDescending" .= rl_toDescending ,"fromDisplayColumns" .= rl_fromDisplayColumns 
               ,"toDisplayColumns" .= rl_toDisplayColumns ,"selfRelated" .= rl_selfRelated 
               ] 

instance FromJSON AODocsRelation where 
    parseJSON = withObject "AODocsRelation" $ \v -> AODocsRelation
        <$> v .: "kind" 
        <*> v .: "id" 
        <*> v .: "libraryId" 
        <*> v .: "name" 
        <*> v .: "name_i18n" 
        <*> v .: "fromId" 
        <*> v .: "toId" 
        <*> v .: "fromName" 
        <*> v .: "fromName_i18n" 
        <*> v .: "toName" 
        <*> v .: "toName_i18n" 
        <*> v .:? "fromSort" 
        <*> v .: "fromDescending" 
        <*> v .:? "toSort" 
        <*> v .: "toDescending" 
        <*> v .: "fromDisplayColumns" 
        <*> v .: "toDisplayColumns" 
        <*> v .: "selfRelated" 

instance Eq AODocsRelation where 
  (AODocsRelation kind1 _ _ name1 _ _ _ fromName1 _ toName1 _ _ _ _ _ fromDisplayColumns1 toDisplayColumns1 selfRelated1 ) 
   == (AODocsRelation kind2 _ _ name2 _ _ _ fromName2 _ toName2 _ _ _ _ _ fromDisplayColumns2 toDisplayColumns2 selfRelated2 ) 
     = kind1 == kind2 && name1 == name2 && fromName1 == fromName2 && toName1 == toName2 
       && fromDisplayColumns1 == fromDisplayColumns2 && toDisplayColumns1 == toDisplayColumns2 && selfRelated1 == selfRelated2

instance Ord AODocsRelation where
  (AODocsRelation _ _ _ name1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ ) `compare` (AODocsRelation _ _ _ name2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ ) 
     = name1 `compare` name2 

data AODocsPermission = 
  AODocsPermission   { 
            pm_kind :: String
          , pm_type :: String
          , role :: String
          , value :: String
          , withLink :: Bool
          , pm_name :: Maybe String
          , thumbnailPhotoUrl :: Maybe String
           } deriving (Show,Generic) 

instance ToJSON AODocsPermission where 
    toJSON (AODocsPermission pm_kind pm_type role value withLink pm_name thumbnailPhotoUrl ) = 
        object ["kind" .= pm_kind,  "type" .= pm_type
               ,"role" .= role ,"value" .= value 
               ,"withLink" .= withLink ,"name" .= pm_name 
               ,"thumbnailPhotoUrl" .= thumbnailPhotoUrl ] 

instance FromJSON AODocsPermission where 
    parseJSON = withObject "AODocsPermission" $ \v -> AODocsPermission
        <$> v .: "kind" 
        <*> v .: "type" 
        <*> v .: "role" 
        <*> v .: "value" 
        <*> v .: "withLink" 
        <*> v .:? "name" 
        <*> v .:? "thumbnailPhotoUrl"        

instance Eq AODocsPermission where 
  (AODocsPermission kind1 type1 role1 value1 _ _ _ ) == (AODocsPermission kind2 type2 role2 value2 _ _ _ ) 
     = kind1 == kind2 && type1 == type2 && role1 == role2 && value1 == value2

instance Ord AODocsPermission where
  (AODocsPermission _ _ _ value1 _ _ _ ) `compare` (AODocsPermission _ _ _ value2 _ _ _ ) 
     = value1 `compare` value2  

data AODocsLibraryLabel = 
  AODocsLibraryLabel   { 
            ll_kind :: String
          , ll_id :: String
          , ll_referenceCatalogId :: AODocsReferenceCatalog 
          , ll_name :: String
          , ll_lastModifiedAt :: String
          , ll_createdAt :: String
          , ll_deleted :: Bool
           } deriving (Show,Generic) 

instance ToJSON AODocsLibraryLabel where 
    toJSON (AODocsLibraryLabel ll_kind ll_id ll_referenceCatalogId ll_name ll_lastModifiedAt ll_createdAt ll_deleted ) = 
        object ["kind" .= ll_kind,  "id" .= ll_id
               ,"referenceCatalogId" .= ll_referenceCatalogId ,"name" .= ll_name 
               ,"lastModifiedAt" .= ll_lastModifiedAt ,"createdAt" .= ll_createdAt 
               ,"deleted" .= ll_deleted ] 

instance FromJSON AODocsLibraryLabel where 
    parseJSON = withObject "AODocsLibraryLabel" $ \v -> AODocsLibraryLabel
        <$> v .: "kind" 
        <*> v .: "id" 
        <*> v .: "referenceCatalogId" 
        <*> v .: "name" 
        <*> v .: "lastModifiedAt" 
        <*> v .: "createdAt" 
        <*> v .: "deleted"   

--  attachmentMode
getAODocsFoldertype :: AODocsAttachment -> String 
getAODocsFoldertype s 
         | s == COMPOSITE = "DML"
         | s == GOOGLE_DRIVE = "SF"
         | s == DRIVE_CONTROLLED = "TF"

data AODocsLibrary  = 
  AODocsLibrary { 
            kind :: String
          , libraryId :: String
          , name :: String
          , name_i18n :: String
          , domainName :: String
          , lastModified :: String
          , lastConfigModified :: String
          , createdAt :: String
          , homeUrl :: String
          , daysBeforeDelete :: Int
          , logoUrl :: Maybe String
          , welcomeText :: String
          , storageAdmin :: String
          , pushToMyDrive :: Bool
          , notifyUsersAboutPushToMyDrive :: Bool
          , onlyAdminsCanManageFolders :: Bool
          , onlyAdminsCanEditRootFolder :: Bool
          , defaultDocumentType :: String
          , rootFolderId :: String
          , creator :: String
          , lastAccessed :: Maybe String
          , forwardRequestAccessRole :: String  
          , historyVisibility :: AODocsVisTypeBox 
          , trashResourceId :: String
          , attachmentMode :: AODocsAttachment 
          , permissionMode :: AODocsPermissionMode 
          , defaultOnlyAdminCanDelete :: Bool
          , defaultOnlyAdminCanShare :: Bool
          , currentUserRight :: AODocsUserRight 
          , documentTypes :: AODocsDocType
          , categoryDefinitions :: AODocsDocType 
          , folderDefinition :: AODocsFolder
          , views :: AODocsDocType 
          , wokflows :: AODocsWokflows 
          , visibility :: AODocsVisibility             
          , relations :: Maybe [AODocsRelation] 
          , permissions :: [AODocsPermission]
          , libraryLabels :: Maybe [AODocsLibraryLabel]
          , sequenceId :: String
          , defaultView :: Maybe String
          , restrictedDownload :: Bool
          , locale :: String
          , timeZone :: String  
          , favorited :: Bool
           } deriving (Show,Generic) 

instance Eq AODocsLibrary where 
  (AODocsLibrary _ _ name1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ) == 
    (AODocsLibrary _ _ name2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ) 
      = name1 == name2 

instance Ord AODocsLibrary where
  (AODocsLibrary _ _ name1  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ) `compare` 
    (AODocsLibrary _ _ name2  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ) 
      = name1 `compare` name2 

instance ToJSON AODocsLibrary     
instance FromJSON AODocsLibrary 

data AODocsJSONLibrary =
  AODocsJSONLibrary { 
            js_kind :: String
          , libraries :: [AODocsLibrary] 
           } deriving (Show,Generic) 

instance ToJSON AODocsJSONLibrary where 
    toJSON (AODocsJSONLibrary js_kind libraries) = 
        object ["kind" .= js_kind,  "libraries" .= libraries] 

instance FromJSON AODocsJSONLibrary where 
    parseJSON = withObject "AODocsJSONLibrary" $ \v -> AODocsJSONLibrary
        <$> v .: "kind" 
        <*> v .: "libraries"         

aODocsJSONLibraryDefault = AODocsJSONLibrary "" [] 

data AODocsLibraryOut  = 
  AODocsLibraryOut { 
            library_Name_AODocs :: String
          , storage_Admin_AODocs :: String
          , root_Folder_Id_GSD :: String
          , library_Id_AODocs :: String 
          , folder_Type :: String  
          , attachment_Mode :: String 
          , permission_Mode :: String 
          , notify_UsersAboutPushToMyDrive :: String
          , only_AdminsCanManageFolders :: String
          , only_AdminsCanEditRootFolder :: String
          , days_BeforeDelete :: String 
          , time_Zone :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AODocsLibraryOut
instance ToNamedRecord AODocsLibraryOut
instance DefaultOrdered AODocsLibraryOut       

decodeAODocsLibraryOut :: B.ByteString -> Either String (Vector AODocsLibraryOut) 
decodeAODocsLibraryOut = fmap snd . decodeByName 

decodeAODocsLibraryOutFromFile :: FilePath -> IO (Either String (Vector AODocsLibraryOut)) 
decodeAODocsLibraryOutFromFile filepath = 
    catchShowIO (B.readFile filepath)
      >>= return . either Left decodeAODocsLibraryOut

data AODocsLibraryOutAll  = 
  AODocsLibraryOutAll { 
            la_kind :: String
          , la_libraryId :: String
          , la_name :: String
          , la_name_i18n :: String
          , la_domainName :: String
          , la_lastModified :: String
          , la_lastConfigModified :: String
          , la_createdAt :: String
          , la_homeUrl :: String
          , la_daysBeforeDelete :: String
          , la_welcomeText :: String
          , la_storageAdmin :: String
          , la_pushToMyDrive :: String
          , la_notifyUsersAboutPushToMyDrive :: String
          , la_onlyAdminsCanManageFolders :: String
          , la_onlyAdminsCanEditRootFolder :: String
          , la_defaultDocumentType :: String
          , la_rootFolderId :: String
          , la_creator :: String
          , la_forwardRequestAccessRole :: String
          , la_trashResourceId :: String
          , la_attachmentMode :: String
          , la_permissionMode :: String
          , la_defaultOnlyAdminCanDelete :: String
          , la_defaultOnlyAdminCanShare :: String
          , la_currentUserRight :: String
          , la_sequenceId :: String
          , la_restrictedDownload :: String
          , la_locale :: String
          , la_timeZone :: String
          , la_favorited :: String
           } deriving (Show,Generic) 

instance FromNamedRecord AODocsLibraryOutAll
instance ToNamedRecord AODocsLibraryOutAll
instance DefaultOrdered AODocsLibraryOutAll     

data AODocsPermissionOut  = 
  AODocsPermissionOut { 
            pmo_library_name :: String
          , pmo_type :: String
          , pmo_role :: String
          , pmo_value :: String
           } deriving (Show,Generic) 

instance FromNamedRecord AODocsPermissionOut
instance ToNamedRecord AODocsPermissionOut
instance DefaultOrdered AODocsPermissionOut     
