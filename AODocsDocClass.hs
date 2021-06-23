{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AODocsDocClass
   Description : Product types for AODocs Library API 
   Copyright   : ( c ) Matthew Lawler 2020   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module defines Product types needed for AODocs JSON functions. 

   Best site: https://artyom.me/aeson 

 -}
module AODocsDocClass  
    (  
      
      AODocsDocClass( .. ), 
      AODocsJSONDocClass( .. ), 
      AODocsField( .. ), 
      AODocsPermissionV2Out( .. ), 
      AODocsPermissionV2PairOut( .. ),    
      AODocsDocClassFieldOut( .. ), 
      AODocsDocClassFieldPairOut( .. ), 

      aODocsJSONDocClassDefault, 
      aODocsPermissionV2OutDefault, 
      aODocsDocClassFieldOutDefault,       

     ) where
    
import Data.Aeson
import Data.List 
import GHC.Generics
import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered) 

import CassavaUtils 

import AODocsEnum 
import AODocsCommon  

data AODocsField = 
  AODocsField   { 
            fi_kind :: String
          , fi_id :: String 
          , fi_displayName ::  String          
          , fi_type :: AODocsType                        
          , fi_multiple :: Bool 
          , fi_readOnly :: Bool 
          , fi_mandatory :: Bool 
          , fi_hidden :: Bool         
          , fi_rank :: Int             
          , fi_categoryId :: Maybe String   
          , fi_categoryDefinition :: Maybe AODocsCategoryDefinition  
          , fi_levelNames :: Maybe [String] 
          , fi_folder :: Bool  
          , fi_dynamicValues :: Bool  
           } deriving (Show,Generic) 

instance ToJSON AODocsField where 
    toJSON (AODocsField fi_kind fi_id fi_displayName fi_type fi_multiple fi_readOnly
            fi_mandatory fi_hidden fi_rank fi_categoryId fi_categoryDefinition 
            fi_levelNames fi_folder fi_dynamicValues) = 
        object ["kind" .= fi_kind ,"id" .= fi_id  
               ,"displayName" .= fi_displayName ,"type" .= fi_type   
               ,"multiple" .= fi_multiple ,"readOnly" .= fi_readOnly 
               ,"mandatory" .= fi_mandatory ,"hidden" .= fi_hidden 
               ,"rank" .= fi_rank ,"categoryId" .= fi_categoryId 
               ,"categoryDefinition" .= fi_categoryDefinition 
               ,"levelNames" .= fi_levelNames 
               ,"folder" .= fi_folder ,"dynamicValues" .= fi_dynamicValues                
               ]

instance FromJSON AODocsField where 
    parseJSON = withObject "AODocsField" $ \v -> AODocsField 
        <$> v .: "kind" 
        <*> v .: "id" 
        <*> v .: "displayName"                 
        <*> v .: "type" 
        <*> v .: "multiple" 
        <*> v .: "readOnly" 
        <*> v .: "mandatory" 
        <*> v .: "hidden"  
        <*> v .: "rank"  
        <*> v .:? "categoryId"  
        <*> v .:? "categoryDefinition" 
        <*> v .:? "levelNames" 
        <*> v .: "folder"  
        <*> v .: "dynamicValues"  

instance Eq AODocsField where 
  (AODocsField _ _ displayName1 type1 _ _ _ _ _ _ categoryDefinition1 _ _ _ ) == 
    (AODocsField _ _ displayName2 type2 _ _ _ _ _ _ categoryDefinition2 _ _ _ )   
      = displayName1 == displayName2 && type1 == type2 && categoryDefinition1 == categoryDefinition2 

instance Ord AODocsField where
  (AODocsField _ _ displayName1 _ _ _ _ _ _ _ _ _ _ _ ) `compare` 
    (AODocsField _ _ displayName2 _ _ _ _ _ _ _ _ _ _ _ ) 
      = displayName1 `compare` displayName2  

data AODocsCategoryDefinition = 
  AODocsCategoryDefinition   { 
            cd_kind :: String
          , cd_libraryId :: String             
          , cd_id :: String 
          , cd_name ::  String          
          , cd_dynamicValues :: Bool  
          , cd_folder :: Bool                             
          , cd_tags :: Bool 
          , cd_levelNames :: [String] 
          , cd_levelNames_i18n :: Maybe [String] 
          , cd_value ::  String     
           } deriving (Show,Generic) 

instance ToJSON AODocsCategoryDefinition where 
    toJSON (AODocsCategoryDefinition cd_kind cd_libraryId cd_id cd_name cd_dynamicValues cd_folder 
            cd_tags cd_levelNames cd_levelNames_i18n cd_value ) =  
        object ["kind" .= cd_kind ,"libraryId" .= cd_libraryId   
               ,"id" .= cd_id   ,"name" .= cd_name  
               ,"dynamicValues" .= cd_dynamicValues ,"folder" .= cd_folder 
               ,"tags" .= cd_tags ,"levelNames" .= cd_levelNames 
               ,"levelNames_i18n" .= cd_levelNames_i18n ,"value" .= cd_value                
               ]

instance FromJSON AODocsCategoryDefinition where 
    parseJSON = withObject "AODocsCategoryDefinition" $ \v -> AODocsCategoryDefinition 
        <$> v .: "kind" 
        <*> v .: "libraryId"         
        <*> v .: "id" 
        <*> v .: "name"          
        <*> v .: "dynamicValues"
        <*> v .: "folder"                        
        <*> v .: "tags" 
        <*> v .: "levelNames"  
        <*> v .:? "levelNames_i18n"                
        <*> v .: "value" 

instance Eq AODocsCategoryDefinition where 
  (AODocsCategoryDefinition _ _ _ name1 _ _ _ levelNames1 _ _ ) == 
    (AODocsCategoryDefinition _ _ _ name2 _ _ _ levelNames2 _ _ )   
      = name1 == name2 && levelNames1 == levelNames2 

instance Ord AODocsCategoryDefinition where
  (AODocsCategoryDefinition _ _ _ name1  _ _ _ _ _ _ ) `compare` 
    (AODocsCategoryDefinition _ _ _ name2  _ _ _ _ _ _ ) 
      = name1 `compare` name2 

data AODocsSystemField = 
  AODocsSystemField   { 
            sf_title :: AODocsSystemFieldContent 
           } deriving (Show,Generic) 

instance ToJSON AODocsSystemField where 
    toJSON (AODocsSystemField sf_title  ) = 
        object ["_title" .= sf_title ] 

instance FromJSON AODocsSystemField where 
    parseJSON = withObject "AODocsSystemField" $ \v -> AODocsSystemField 
        <$> v .: "_title" 

instance Eq AODocsSystemField where 
  (AODocsSystemField t1 ) == (AODocsSystemField t2 ) = t1 == t2 
  
data AODocsSystemFieldContent = 
  AODocsSystemFieldContent   { 
            sc_kind :: String 
          , sc_readOnly :: Bool  
          , sc_type ::  AODocsType             
           } deriving (Show,Generic) 

instance ToJSON AODocsSystemFieldContent where 
    toJSON (AODocsSystemFieldContent sc_kind sc_readOnly sc_type ) = 
        object ["kind" .= sc_kind , "readOnly" .= sc_readOnly, "type" .= sc_type] 

instance FromJSON AODocsSystemFieldContent where 
    parseJSON = withObject "AODocsSystemFieldContent" $ \v -> AODocsSystemFieldContent 
        <$> v .: "kind" 
        <*> v .: "readOnly" 
        <*> v .: "type" 

instance Eq AODocsSystemFieldContent where 
  (AODocsSystemFieldContent sfc11 sfc12 sfc13) == (AODocsSystemFieldContent sfc21 sfc22 sfc23 ) 
    = sfc11 == sfc21 && sfc12 == sfc22 && sfc13 == sfc23 

data AODocsCustomAction = 
  AODocsCustomAction   { 
            ca_scriptId :: String 
          , ca_scriptName :: String  
          , ca_kind :: String  
          , ca_id :: String  
          , ca_name :: String  
          , ca_event :: String            
           } deriving (Show,Generic) 

instance ToJSON AODocsCustomAction where 
    toJSON (AODocsCustomAction ca_scriptId ca_scriptName ca_kind ca_id ca_name ca_event) = 
        object ["scriptId" .= ca_scriptId , "scriptName" .= ca_scriptName
               ,"kind" .= ca_kind , "id" .= ca_id
               ,"name" .= ca_name , "event" .= ca_event
               ] 

instance FromJSON AODocsCustomAction where 
    parseJSON = withObject "AODocsCustomAction" $ \v -> AODocsCustomAction 
        <$> v .: "scriptId" 
        <*> v .: "scriptName" 
        <*> v .: "kind" 
        <*> v .: "id" 
        <*> v .: "name" 
        <*> v .: "event"         

instance Eq AODocsCustomAction where 
  (AODocsCustomAction _ _ _ _ name1 _ ) == 
    (AODocsCustomAction _ _ _ _ name2 _ )   
      = name1 == name2   

instance Ord AODocsCustomAction where
  (AODocsCustomAction _ _ _ _ name1 _ ) `compare` 
    (AODocsCustomAction _ _ _ _ name2 _ ) 
      = name1 `compare` name2            

data AODocsACM = 
  AODocsACM   { 
            ac_DEFAULT :: AODocsACMValue 
           } deriving (Show,Generic) 

instance ToJSON AODocsACM where 
    toJSON (AODocsACM ac_DEFAULT  ) = 
        object ["DEFAULT" .= ac_DEFAULT ] 

instance FromJSON AODocsACM where 
    parseJSON = withObject "AODocsACM" $ \v -> AODocsACM 
        <$> v .: "DEFAULT" 

instance Eq AODocsACM where 
  (AODocsACM acm1 ) == (AODocsACM acm2 ) = acm1 == acm2          

data AODocsACMValue = 
  AODocsACMValue   { 
            enabled :: Bool 
          , mustBeEnabled :: Bool  
           } deriving (Show,Generic) 

instance ToJSON AODocsACMValue where 
    toJSON (AODocsACMValue enabled mustBeEnabled ) = 
        object ["enabled" .= enabled , "mustBeEnabled" .= mustBeEnabled ] 

instance FromJSON AODocsACMValue where 
    parseJSON = withObject "AODocsACMValue" $ \v -> AODocsACMValue 
        <$> v .: "enabled" 
        <*> v .: "mustBeEnabled" 

instance Eq AODocsACMValue where 
  (AODocsACMValue acv11 acv12 ) == (AODocsACMValue acv21 acv22 ) 
   = acv11 == acv21 && acv12 == acv22 

data AODocsSection = 
  AODocsSection   { 
            se_id :: String 
          , se_name :: String  
          , se_name_i18n ::  String  
          , se_description ::  String   
          , se_fields ::  [AODocsFieldId] 
           } deriving (Show,Generic) 

instance ToJSON AODocsSection where 
    toJSON (AODocsSection se_id se_name se_name_i18n se_description se_fields) = 
        object ["id" .= se_id ,"name" .= se_name  
               ,"name_i18n" .= se_name_i18n ,"description" .= se_description 
               ,"fields" .= se_fields 
               ] 

instance FromJSON AODocsSection where 
    parseJSON = withObject "AODocsSection" $ \v -> AODocsSection 
        <$> v .: "id" 
        <*> v .: "name" 
        <*> v .: "name_i18n" 
        <*> v .: "description" 
        <*> v .: "fields" 

instance Eq AODocsSection where 
  (AODocsSection sct11 sct12 sct13 sct14 sct15 ) == (AODocsSection sct21 sct22 sct23 sct24 sct25 ) 
   = sct11 == sct21 && sct12 == sct22 && sct13 == sct23 && sct14 == sct24 && sct15 == sct25    

data AODocsFieldId = 
  AODocsFieldId   { 
            af_id :: String 
          , af_type :: AODocsType   
           } deriving (Show,Generic) 

instance ToJSON AODocsFieldId where 
    toJSON (AODocsFieldId af_id af_type ) = 
        object ["id" .= af_id , "type" .= af_type ] 

instance FromJSON AODocsFieldId where 
    parseJSON = withObject "AODocsFieldId" $ \v -> AODocsFieldId 
        <$> v .: "id" 
        <*> v .: "type" 

instance Eq AODocsFieldId where 
  (AODocsFieldId fdi11 fdi12 ) == (AODocsFieldId fdi21 fdi22 ) 
   = fdi11 == fdi21 && fdi12 == fdi22 

data AODocsDocClass  = 
  AODocsDocClass { 
            kind :: String
          , id :: String
          , displayName :: String
          , displayName_i18n :: String
          , libraryId :: String
          , workflowId :: Maybe String
          , description :: Maybe String
          , fields :: [AODocsField] 
          , systemFields :: AODocsSystemField
          , checkoutMode :: AODocsCheckoutMode 
          , onlyAdminsCanShare :: Bool 
          , onlyAdminsCanDelete :: Bool
          , managedPermissionSource :: AODocsType 
          , explicitPermissionMode :: String
          , forceNumberVersions :: Bool
          , coordinate :: String
          , restrictedDownload :: Bool
          , manualVersioningDisabled :: Maybe Bool          
          , readOnlyTitle :: Bool
          , firstVersionNumberingMode :: String
          , firstVersionName :: String
          , usersCanAccessPreviousVersions :: Bool
          , previousVersionsVisibility :: AODocsVisTypeBox 
          , creators :: [AODocsPermissionV2] 
          , permissions :: [AODocsPermissionV2]  
          , customActions :: Maybe [AODocsCustomAction] 
          , attachmentCreationModes :: AODocsACM
          , sequenceId :: String
          , sections :: [AODocsSection] 
          , sectionsDefaultColumnCount :: Int 
          , defaultClass :: Bool
          , documentCopyDisabled :: Bool 
          , name :: String
          , value :: String 
          , name_i18n :: String 
          } deriving (Show,Generic) 

instance Eq AODocsDocClass where 
  (AODocsDocClass adc111 adc112 adc113 adc114 adc115 adc116 adc117 adc118 adc119 adc120 adc121 adc122 adc123 
    adc124 adc125 adc126 adc127 adc128 adc129 adc130 adc131 adc132 adc133 adc134 adc135 adc136 adc137 adc138 
    adc139 adc140 adc141 adc142 adc143 adc144 adc145 ) == 
    (AODocsDocClass adc211 adc212 adc213 adc214 adc215 adc216 adc217 adc218 adc219 adc220 adc221 adc222 
      adc223 adc224 adc225 adc226 adc227 adc228 adc229 adc230 adc231 adc232 adc233 adc234 adc235 adc236 
      adc237 adc238 adc239 adc240 adc241 adc242 adc243 adc244 adc245 )   
      = adc111 == adc211 && adc112 == adc212 && adc113 == adc213 && adc114 == adc214 && adc115 == adc215 
       && adc116 == adc216 && adc117 == adc217 && adc118 == adc218 && adc119 == adc219 && adc120 == adc220 
       && adc121 == adc221 && adc122 == adc222 && adc123 == adc223 && adc124 == adc224 && adc125 == adc225 
       && adc126 == adc226 && adc127 == adc227 && adc128 == adc228 && adc129 == adc229 && adc130 == adc230 
       && adc131 == adc231 && adc132 == adc232 && adc133 == adc233 && adc134 == adc234 && adc135 == adc235 
       && adc136 == adc236 && adc137 == adc237 && adc138 == adc238 && adc139 == adc239 && adc140 == adc240 
       && adc141 == adc241 && adc142 == adc242 && adc143 == adc243 && adc144 == adc244 && adc145 == adc245  


--instance Eq AODocsDocClass where 
--  (AODocsDocClass _ _ _ _ _ _ _ fields1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ name1 _ _ ) == 
--    (AODocsDocClass _ _ _ _ _ _ _ fields2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ name2 _ _ )   
--      = name1 == name2 && fields1 == fields2 

-- diff??  creators permissions customActions 

instance Ord AODocsDocClass where
  (AODocsDocClass _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ name1 _ _ ) `compare` 
    (AODocsDocClass _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ name2 _ _ ) 
      = name1 `compare` name2 

instance ToJSON AODocsDocClass     
instance FromJSON AODocsDocClass 

data AODocsJSONDocClass =
  AODocsJSONDocClass { 
            js_kind :: String
          , js_items :: [AODocsDocClass] 
           } deriving (Show,Generic) 

instance Eq AODocsJSONDocClass where 
  (AODocsJSONDocClass k1 is1 ) == (AODocsJSONDocClass k2 is2 )   
      = k1 == k2 && is1 == is2 

instance ToJSON AODocsJSONDocClass where 
    toJSON (AODocsJSONDocClass js_kind js_items) = 
        object ["kind" .= js_kind,  "items" .= js_items] 

instance FromJSON AODocsJSONDocClass where 
    parseJSON = withObject "AODocsJSONDocClass" $ \v -> AODocsJSONDocClass
        <$> v .: "kind" 
        <*> v .: "items"         

aODocsJSONDocClassDefault = AODocsJSONDocClass "" [] 


data AODocsPermissionV2Out  = 
  AODocsPermissionV2Out { 
            p2o_libraryId :: String
          , p2o_dcname :: String
          , p2o_CorP ::  String            
          , p2o_displayName ::  String 
          , p2o_type :: String 
          , p2o_role :: String 
          , p2o_value :: String  
           } deriving (Show,Generic) 

instance FromNamedRecord AODocsPermissionV2Out
instance ToNamedRecord AODocsPermissionV2Out
instance DefaultOrdered AODocsPermissionV2Out 

aODocsPermissionV2OutDefault = AODocsPermissionV2Out "" "" "" "" "" "" "" 

data AODocsPermissionV2PairOut  = 
  AODocsPermissionV2PairOut { 
            p2p_key :: String  
          , p2p_dcname :: String
          , p2p_CorP ::  String            
          , p2p_displayName ::  String   
          , p2p_libraryId1 :: String                   
          , p2p_type1 :: String 
          , p2p_role1 :: String 
          , p2p_value1 :: String  
          , p2p_libraryId2 :: String 
          , p2p_type2 :: String 
          , p2p_role2 :: String 
          , p2p_value2 :: String  
          , p2p_equal_perms ::  String           
           } deriving (Show,Generic) 

instance FromNamedRecord AODocsPermissionV2PairOut
instance ToNamedRecord AODocsPermissionV2PairOut
instance DefaultOrdered AODocsPermissionV2PairOut 

data AODocsDocClassFieldOut  = 
  AODocsDocClassFieldOut { 
            fio_displayName ::  String 
          , fio_type :: String  
          , fio_name :: String 
          , fio_libraryId :: String   
          , fio_multiple :: String 
          , fio_readOnly :: String 
          , fio_mandatory :: String 
          , fio_hidden :: String         
          , fio_rank :: String 
          , fio_folder :: String  
          , fio_dynamicValues :: String                                      
           } deriving (Show,Generic) 

instance FromNamedRecord AODocsDocClassFieldOut
instance ToNamedRecord AODocsDocClassFieldOut
instance DefaultOrdered AODocsDocClassFieldOut 

aODocsDocClassFieldOutDefault = AODocsDocClassFieldOut "" "" "" "" "" "" "" "" "" "" "" 

data AODocsDocClassFieldPairOut  = 
  AODocsDocClassFieldPairOut { 
            fip_key ::  String 
          , fip_name :: String 
          , fip_displayName :: String                       
          , fip_libraryId1 :: String   
          , fip_type1 :: String   
          , fip_multiple1 :: String 
          , fip_readOnly1 :: String 
          , fip_mandatory1 :: String 
          , fip_hidden1 :: String         
          , fip_rank1 :: String 
          , fip_folder1 :: String  
          , fip_dynamicValues1 :: String                    
          , fip_libraryId2 :: String    
          , fip_type2 :: String  
          , fip_multiple2 :: String 
          , fip_readOnly2 :: String 
          , fip_mandatory2 :: String 
          , fip_hidden2 :: String         
          , fip_rank2 :: String 
          , fip_folder2 :: String  
          , fip_dynamicValues2 :: String                     
          , fip_equal_type2 :: String                                      
           } deriving (Show,Generic) 

instance FromNamedRecord AODocsDocClassFieldPairOut
instance ToNamedRecord AODocsDocClassFieldPairOut
instance DefaultOrdered AODocsDocClassFieldPairOut  

