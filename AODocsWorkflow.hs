{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AODocsWorkflow
   Description : Product types for AODocs Library API 
   Copyright   : ( c ) Matthew Lawler 2020   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module defines Product types needed for AODocs JSON functions. 

   Best site: https://artyom.me/aeson 

 -}
module AODocsWorkflow   
    (  
      
      AODocsWorkflow( .. ), 
      AODocsJSONWorkflow( .. ), 
      AODocsWorkflowState( .. ), 
      AODocsPermissionV2WFPairOut( .. ),
      AODocsPermissionV2WFOut( .. ),

      aODocsJSONWorkflowDefault, 
      aODocsPermissionV2WFOutDefault, 

     ) where
    
import Data.Aeson
import GHC.Generics
import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered) 

import CassavaUtils  

import AODocsEnum 
import AODocsCommon  

data AODocsCustomEmail = 
  AODocsCustomEmail   { 
            ce_layout :: Maybe String 
          , ce_senderTemplate :: Maybe String
          , ce_subjectTemplate :: Maybe String       
          , ce_bodyTemplate :: Maybe String  
           } deriving (Show,Generic) 

instance ToJSON AODocsCustomEmail where 
    toJSON (AODocsCustomEmail ce_layout ce_senderTemplate ce_subjectTemplate ce_bodyTemplate ) = 
        object ["layout" .= ce_layout, "senderTemplate" .= ce_senderTemplate,
                "subjectTemplate" .= ce_subjectTemplate, "bodyTemplate" .= ce_bodyTemplate ] 

instance FromJSON AODocsCustomEmail where 
    parseJSON = withObject "AODocsCustomEmail" $ \v -> AODocsCustomEmail 
        <$> v .:? "layout" 
        <*> v .:? "senderTemplate" 
        <*> v .:? "subjectTemplate"
        <*> v .:? "bodyTemplate" 

data AODocsWFNotifications = 
  AODocsWFNotifications   { 
            wn_notifyHumanActionPerformer :: Bool 
          , wn_notificationEmail :: AODocsCustomEmail
          , wn_actionEmail :: AODocsCustomEmail              
           } deriving (Show,Generic) 

instance ToJSON AODocsWFNotifications where 
    toJSON (AODocsWFNotifications wn_notifyHumanActionPerformer wn_notificationEmail wn_actionEmail ) = 
        object ["notifyHumanActionPerformer" .= wn_notifyHumanActionPerformer
        , "notificationEmail" .= wn_notificationEmail, "actionEmail" .= wn_actionEmail] 

instance FromJSON AODocsWFNotifications where 
    parseJSON = withObject "AODocsWFNotifications" $ \v -> AODocsWFNotifications 
        <$> v .: "notifyHumanActionPerformer" 
        <*> v .: "notificationEmail" 
        <*> v .: "actionEmail"   

data AODocsPropertyData = 
  AODocsPropertyData   { 
            pd_propertyId :: String 
          , pd_operator :: String   
          , pd_value :: Maybe String             
           } deriving (Show,Generic) 

instance ToJSON AODocsPropertyData where 
    toJSON (AODocsPropertyData pd_propertyId pd_operator pd_value ) = 
        object ["propertyId" .= pd_propertyId, "operator" .= pd_operator, "value" .= pd_value] 

instance FromJSON AODocsPropertyData where 
    parseJSON = withObject "AODocsPropertyData" $ \v -> AODocsPropertyData 
        <$> v .: "propertyId" 
        <*> v .: "operator" 
        <*> v .:? "value"     

data AODocsWorkflowTransition = 
  AODocsWorkflowTransition   { 
            wt_kind :: String
          , wt_id :: String 
          , wt_createdTime :: String  
          , wt_lastModifiedTime :: String                             
          , wt_targetStateId :: String  
          , wt_targetStateName :: String 
          , wt_targetStateName_i18n :: String 
          , wt_transitionType :: AODocsApplyActionToDocuments  
          , wt_createNewDocumentVersion :: Bool 
          , wt_discardCheckedOutDocument :: Bool 
          , wt_propertyData :: Maybe AODocsPropertyData
           } deriving (Show,Generic) 

instance ToJSON AODocsWorkflowTransition where 
    toJSON (AODocsWorkflowTransition wt_kind wt_id wt_createdTime wt_lastModifiedTime
            wt_targetStateId wt_targetStateName wt_targetStateName_i18n wt_transitionType
            wt_createNewDocumentVersion wt_discardCheckedOutDocument wt_propertyData ) =  
        object ["kind" .= wt_kind ,"id" .= wt_id   
               ,"createdTime" .= wt_createdTime   ,"lastModifiedTime" .= wt_lastModifiedTime  
               ,"targetStateId" .= wt_targetStateId ,"targetStateName" .= wt_targetStateName 
               ,"targetStateName_i18n" .= wt_targetStateName_i18n 
               ,"transitionType" .= wt_transitionType 
               ,"createNewDocumentVersion" .= wt_createNewDocumentVersion 
               ,"discardCheckedOutDocument" .= wt_discardCheckedOutDocument 
               ,"propertyData" .= wt_propertyData                
               ]

instance FromJSON AODocsWorkflowTransition where 
    parseJSON = withObject "AODocsWorkflowTransition" $ \v -> AODocsWorkflowTransition 
        <$> v .: "kind" 
        <*> v .: "id"         
        <*> v .: "createdTime" 
        <*> v .: "lastModifiedTime"          
        <*> v .: "targetStateId"
        <*> v .: "targetStateName"                        
        <*> v .: "targetStateName_i18n" 
        <*> v .: "transitionType"  
        <*> v .: "createNewDocumentVersion"    
        <*> v .: "discardCheckedOutDocument" 
        <*> v .:? "propertyData" 

data AODocsHiddenFieldId = 
  AODocsHiddenFieldId   { 
            hf_id :: String 
          , hf_name :: String   
           } deriving (Show,Generic) 

instance ToJSON AODocsHiddenFieldId where 
    toJSON (AODocsHiddenFieldId hf_id hf_name ) = 
        object ["id" .= hf_id , "name" .= hf_name ] 

instance FromJSON AODocsHiddenFieldId where 
    parseJSON = withObject "AODocsHiddenFieldId" $ \v -> AODocsHiddenFieldId 
        <$> v .: "id" 
        <*> v .: "name" 

data AODocsWorkflowState = 
  AODocsWorkflowState   { 
            ws_kind :: String
          , ws_id :: String 
          , ws_name ::  String  
          , ws_name_i18n ::  String                    
          , ws_createdTime :: String  
          , ws_lastModifiedTime :: String                             
          , ws_classReadOnlyDocumentsNotCheckedOut :: AODocsCheckoutMode  
          , ws_readOnlyDocumentsNotCheckedOut :: Bool 
          , ws_resetDocumentPermissions :: Bool 
          , ws_permissions :: Maybe [AODocsPermissionV2] 
          , ws_hiddenFields :: Maybe [AODocsHiddenFieldId] 
          , ws_applyActionToDocuments :: AODocsApplyActionToDocuments 
          , ws_transitions :: Maybe [AODocsWorkflowTransition] 
          , ws_notifications ::  AODocsWFNotifications
          , ws_draftDocuments :: Bool 
           } deriving (Show,Generic) 

instance ToJSON AODocsWorkflowState where 
    toJSON (AODocsWorkflowState ws_kind ws_id ws_name ws_name_i18n ws_createdTime ws_lastModifiedTime 
            ws_classReadOnlyDocumentsNotCheckedOut ws_readOnlyDocumentsNotCheckedOut ws_resetDocumentPermissions 
            ws_permissions ws_hiddenFields 
            ws_applyActionToDocuments ws_transitions ws_notifications 
            ws_draftDocuments) =  
        object ["kind" .= ws_kind ,"id" .= ws_id   
               ,"name" .= ws_name   ,"name_i18n" .= ws_name_i18n  
               ,"createdTime" .= ws_createdTime ,"lastModifiedTime" .= ws_lastModifiedTime 
               ,"classReadOnlyDocumentsNotCheckedOut" .= ws_classReadOnlyDocumentsNotCheckedOut 
               ,"readOnlyDocumentsNotCheckedOut" .= ws_readOnlyDocumentsNotCheckedOut 
               ,"resetDocumentPermissions" .= ws_resetDocumentPermissions 
               ,"permissions" .= ws_permissions  ,"hiddenFields" .= ws_hiddenFields               
               ,"applyActionToDocuments" .= ws_applyActionToDocuments 
               ,"transitions" .= ws_transitions    
               ,"notifications" .= ws_notifications                               
               ,"draftDocuments" .= ws_draftDocuments                
               ]

instance FromJSON AODocsWorkflowState where 
    parseJSON = withObject "AODocsWorkflowState" $ \v -> AODocsWorkflowState 
        <$> v .: "kind" 
        <*> v .: "id"         
        <*> v .: "name" 
        <*> v .: "name_i18n"          
        <*> v .: "createdTime"
        <*> v .: "lastModifiedTime"                        
        <*> v .: "classReadOnlyDocumentsNotCheckedOut" 
        <*> v .: "readOnlyDocumentsNotCheckedOut"  
        <*> v .: "resetDocumentPermissions"    
        <*> v .:? "permissions" 
        <*> v .:? "hiddenFields"         
        <*> v .: "applyActionToDocuments" 
        <*> v .:? "transitions" 
        <*> v .: "notifications"         
        <*> v .: "draftDocuments" 

data AODocsWorkflow  = 
  AODocsWorkflow { 
            kind :: String
          , id :: String
          , name :: String
          , libraryId :: String
          , libraryName :: String
          , classId :: String
          , className :: String
          , createdTime :: String
          , lastModifiedTime :: String 
          , initialStateId :: String 
          , states :: [AODocsWorkflowState] 
          , muteNotifications :: Bool
          , allowNotificationForAllExternalUsers :: Bool
          } deriving (Show,Generic) 

instance Eq AODocsWorkflow where 
  (AODocsWorkflow _ _ name1 _ _ _ _ _ _ _ _ _ _ ) == 
    (AODocsWorkflow _ _ name2 _ _ _ _ _ _ _ _ _ _ )   
      = name1 == name2 

instance Ord AODocsWorkflow where
  (AODocsWorkflow _ _ name1 _ _ _ _ _ _ _ _ _ _ ) `compare` 
    (AODocsWorkflow _ _ name2 _ _ _ _ _ _ _ _ _ _ ) 
      = name1 `compare` name2 

instance ToJSON AODocsWorkflow     
instance FromJSON AODocsWorkflow 

data AODocsJSONWorkflow =
  AODocsJSONWorkflow { 
            js_count :: Int 
          , js_kind :: String
          , js_workflows :: [AODocsWorkflow] 
           } deriving (Show,Generic) 

instance ToJSON AODocsJSONWorkflow where 
    toJSON (AODocsJSONWorkflow js_count js_kind js_workflows) = 
        object ["count" .= js_count, "kind" .= js_kind,  "workflows" .= js_workflows] 

instance FromJSON AODocsJSONWorkflow where 
    parseJSON = withObject "AODocsJSONWorkflow" $ \v -> AODocsJSONWorkflow
        <$> v .: "count" 
        <*> v .: "kind"         
        <*> v .: "workflows"         

aODocsJSONWorkflowDefault = AODocsJSONWorkflow 0 "" [] 

data AODocsPermissionV2WFOut  = 
  AODocsPermissionV2WFOut { 
            pwo_libraryId :: String
          , pwo_className :: String
          , pwo_WorkflowName :: String            
          , pwo_WFStateName :: String 
          , pwo_displayName :: String           
          , pwo_type :: String 
          , pwo_role :: String 
          , pwo_value :: String  
           } deriving (Show,Generic) 

instance FromNamedRecord AODocsPermissionV2WFOut
instance ToNamedRecord AODocsPermissionV2WFOut
instance DefaultOrdered AODocsPermissionV2WFOut

aODocsPermissionV2WFOutDefault = AODocsPermissionV2WFOut "" "" "" "" "" "" "" "" 

data AODocsPermissionV2WFPairOut  = 
  AODocsPermissionV2WFPairOut { 
            pwp_key :: String  
          , pwp_className :: String
          , pwp_WorkflowName ::  String   
          , pwp_WFStateName ::  String                      
          , pwp_displayName ::  String   
          , pwp_libraryId1 :: String                   
          , pwp_type1 :: String 
          , pwp_role1 :: String 
          , pwp_value1 :: String  
          , pwp_libraryId2 :: String 
          , pwp_type2 :: String 
          , pwp_role2 :: String 
          , pwp_value2 :: String  
          , pwp_equal_perms ::  Bool           
           } deriving (Show,Generic) 

instance FromNamedRecord AODocsPermissionV2WFPairOut
instance ToNamedRecord AODocsPermissionV2WFPairOut
instance DefaultOrdered AODocsPermissionV2WFPairOut 

