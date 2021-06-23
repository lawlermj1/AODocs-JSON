{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AODocs
   Description : Product types for AODocs View APIs  
   Copyright   : ( c ) Matthew Lawler 2020   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module defines Product types needed for AODocs JSON functions. 

   Best site: https://artyom.me/aeson 
   
 -}
module AODocsView  
    (  
      
      AODocsJSONView( .. ), 
      AODocsView( .. ), 
      AODocsField( .. ), 
      AODocsOrder( .. ),
      AODocsVisibility( .. ),
      AODocsVisType( .. ),
      AODocsFilter( .. ),
      AODocsFilterValues( .. ),
      AODocsViewOut( .. ), 
      AODocsFilterOut( .. ), 

      aODocsJSONViewDefault, 
      aODocsVisibilityDefault, 

     ) where
    

import Data.Aeson
import Data.List 
import GHC.Generics
import qualified Data.HashMap.Strict as HM 
import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered) 

import AODocsEnum 
import AODocsCommon 
----- Product types 

data AODocsFilterValues =  
        AODocsFilterValue { fv_id :: String , fv_name :: String , fv_name_i18n :: String } 
      | AODocsFilterValueFixed { fx_value :: String } 
        deriving (Show,Generic) 

instance FromJSON AODocsFilterValues where 
    parseJSON = withObject "AODocsFilterValues" $ \v -> do 
-- look for a JSON field called value 
        let idvalue = HM.lookup "value" v 
-- if present then fixed value, else a variable 
        afv <- case idvalue of 
          Just x  -> AODocsFilterValueFixed 
            <$> v .: "value" 
          Nothing -> AODocsFilterValue 
            <$> v .: "id"
            <*> v .: "name"
            <*> v .: "name_i18n"  
        return afv 

instance ToJSON AODocsFilterValues where 
    toJSON (AODocsFilterValue fv_id fv_name fv_name_i18n) =
        object ["id" .= fv_id, "name" .= fv_name, "name_i18n" .= fv_name_i18n]
    toJSON (AODocsFilterValueFixed fx_value) =
        object ["value" .= fx_value]

data AODocsFilter =  
    AODocsFilter { 
            fi_kind :: String
          , joinType :: AODocsJoinType
          , column :: AODocsColumn 
          , operator :: AODocsFilterOperator
          , values :: [[AODocsFilterValues]]           
          , fi_value :: String 
          } deriving (Show,Generic)  

instance ToJSON AODocsFilter where 
    toJSON (AODocsFilter fi_kind joinType column operator values fi_value) =
        object ["kind" .= fi_kind, "joinType" .= joinType, 
                "column" .= column, "operator" .= operator, 
                "values" .= values, 
                "value" .= fi_value ]

instance FromJSON AODocsFilter where 
    parseJSON = withObject "AODocsFilter" $ \v -> AODocsFilter
        <$> v .: "kind"
        <*> v .: "joinType"
        <*> v .: "column" 
        <*> v .: "operator" 
        <*> v .: "values" 
        <*> v .: "value" 

data AODocsOrder =  
    AODocsOrder { 
            or_kind :: String
          , sortByField :: AODocsColumn
          , descending :: Bool
          } deriving (Show,Generic)   

instance ToJSON AODocsOrder where 
    toJSON (AODocsOrder or_kind sortByField descending) =
        object ["kind" .= or_kind, "sortByField" .= sortByField, "descending" .= descending]

instance FromJSON AODocsOrder where 
    parseJSON = withObject "AODocsOrder" $ \v -> AODocsOrder
        <$> v .: "kind"
        <*> v .: "sortByField"
        <*> v .: "descending"            

data AODocsVisibility =  
    AODocsVisibility { 
            vi_type :: AODocsVisType 
          , vi_value :: Maybe String 
          , displayName :: Maybe String 
          , displayName_i18n :: Maybe String 
          , description :: Maybe String 
          , thumbnailPictureUrl :: Maybe String 
          } deriving (Show,Generic)     

instance ToJSON AODocsVisibility where 
    toJSON (AODocsVisibility vi_type vi_value displayName displayName_i18n description thumbnailPictureUrl) = 
        object ["type" .= vi_type, "value" .= vi_value, 
                "displayName" .= displayName, "displayName_i18n" .= displayName_i18n, 
                "description" .= description, "thumbnailPictureUrl" .= thumbnailPictureUrl ]

instance FromJSON AODocsVisibility where 
    parseJSON = withObject "AODocsVisibility" $ \v -> AODocsVisibility
        <$> v .: "type" 
        <*> v .:? "value"
        <*> v .:? "displayName" 
        <*> v .:? "displayName_i18n" 
        <*> v .:? "description" 
        <*> v .:? "thumbnailPictureUrl" 

aODocsVisibilityDefault = AODocsVisibility NOTVISIBLE Nothing Nothing Nothing Nothing Nothing

data AODocsField =  
    AODocsField { 
            fi_id :: String
          , fi_name :: String
          , fi_name_i18n :: String
          , system  :: Bool 
          , fi_type :: AODocsType 
          , multiple  :: Bool 
          , capabilities :: Maybe [AODocsCapability] 
          , filterOperators :: Maybe [AODocsFilterOperator] 
          } deriving (Show,Generic)  

instance Eq AODocsField where 
  (AODocsField _ fi_name1 _ _ _ _ _ _) == 
    (AODocsField _ fi_name2 _ _ _ _ _ _) 
      = fi_name1 == fi_name2

instance Ord AODocsField where
  (AODocsField _ fi_name1 _ _ _ _ _ _) `compare` 
    (AODocsField _ fi_name2 _ _ _ _ _ _) 
      = fi_name1 `compare` fi_name2

instance ToJSON AODocsField where 
    toJSON (AODocsField fi_id fi_name fi_name_i18n system fi_type multiple capabilities filterOperators ) =
        object ["id" .= fi_id, "name" .= fi_name, 
                "name_i18n" .= fi_name_i18n, "system" .= system,
                "type" .= fi_type, "multiple" .= multiple, 
                "capabilities" .= capabilities, "filterOperators" .= filterOperators]

instance FromJSON AODocsField where 
    parseJSON = withObject "AODocsField" $ \v -> AODocsField
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "name_i18n" 
        <*> v .: "system"
        <*> v .: "type" 
        <*> v .: "multiple"
        <*> v .:? "capabilities" 
        <*> v .:? "filterOperators" 

-- | Type of each JSON entry in record syntax.
data AODocsView =
  AODocsView { 
            kind :: String
          , id :: String
          , showInLibrary :: Bool
          , name :: String
          , name_i18n :: String
          , url :: String
          , displayColumns :: [AODocsColumn] 
          , filterColumns :: Maybe [AODocsColumn] 
          , filters :: Maybe [AODocsFilter]           
          , order :: AODocsOrder  
          , browseBy :: Maybe AODocsColumn 
--	ignore "type": "CATEGORY" in browseBy 
          , classId :: String
          , className :: String
          , libraryId :: String
          , includeSubfolders :: Bool
          , includeOtherClasses :: Bool
          , searchInAttachmentsByDefault :: Bool
          , hideOutdatedCategoryValues :: Bool
          , defaultView :: Bool
          , visibility  :: Maybe AODocsVisibility 
          , numberOfDocumentsPerPage :: Int
          , expandFilters :: Bool
          , publishState :: AODocsPublishState 
          , availableFields :: [AODocsField]
          , canDisplay :: Bool
          , openDocumentTargetPage :: AODocsPage          
          , openDocumentTargetTab :: AODocsTab
          , value :: String
           } deriving (Show,Generic)

instance Eq AODocsView where 
  (AODocsView _ _ _ name1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ af1 _ _ _ _ ) == 
    (AODocsView _ _ _ name2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ af2 _ _ _ _ ) 
      = name1 == name2 && (sort af1) == (sort af2) 

instance Ord AODocsView where
  (AODocsView _ _ _ name1  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ) `compare` 
    (AODocsView _ _ _ name2  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ) 
      = name1 `compare` name2 
      
instance ToJSON AODocsView 
instance FromJSON AODocsView

data AODocsJSONView =
  AODocsJSONView { 
            js_kind :: String
          , items :: [AODocsView] 
           } deriving (Show,Generic) 

instance ToJSON AODocsJSONView where 
    -- this generates a Value
    toJSON (AODocsJSONView js_kind items) = 
        object ["kind" .= js_kind,  "items" .= items] 

instance FromJSON AODocsJSONView where 
    parseJSON = withObject "AODocsJSONView" $ \v -> AODocsJSONView
        <$> v .: "kind" 
        <*> v .: "items"         

aODocsJSONViewDefault = AODocsJSONView "" [] 

data AODocsViewOut  = 
  AODocsViewOut { 
            class_Name :: String
          , view_Name :: String
          , sort_By_Field :: String
          , browse_By :: String 
          , visibility_Type :: String 
          , visibility_Value :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AODocsViewOut
instance ToNamedRecord AODocsViewOut
instance DefaultOrdered AODocsViewOut    

data AODocsFilterOut  = 
  AODocsFilterOut { 
            class_Name_Filter :: String
          , view_Name_Filter :: String
          , join_Type :: String
          , column_Name :: String 
          , operator_type :: String 
          , filter_Value :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AODocsFilterOut
instance ToNamedRecord AODocsFilterOut
instance DefaultOrdered AODocsFilterOut     

