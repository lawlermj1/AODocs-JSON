{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

{- |
   Module      : AODocsCommon 
   Description : Common product types for AODocs APIs  
   Copyright   : ( c ) Matthew Lawler 2020   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module defines shared common types needed for AODocs JSON functions. 

   Best site: https://artyom.me/aeson 
   
 -}
module AODocsCommon 
    (  
      
      AODocsColumn( .. ),   
      AODocsVisTypeBox( .. ),  
      AODocsPermissionV2( .. ),  

      aODocsColumnDefault,   
      aODocsPermissionV2Default,  
      am1LibraryId, 
      amXLibraryId, 
      docClassMove,

      prop_inverse_B_to_JSON_to_B, 
      prop_inverse_JSON_to_B_to_JSON, 

     ) where
    

import Data.Aeson
import GHC.Generics
import Data.Either 
import qualified Data.ByteString.Lazy as B 

import AODocsEnum 

am1LibraryId = "Pi6VQAw4szhUcyHGpy" 
amXLibraryId = ["Pu5DR2x6C1SasBULr8", "PtwsrnX2Sis0TpQPnB"]

docClassMove = ["Market Regulation & Compliance","Mobile Plant & Vehicles","Operating Activity","Strategic Asset Specification","Technical"] 

-------------------   
data AODocsColumn =  
    AODocsColumn { 
            co_kind :: String
          , co_name :: String
          , co_name_i18n :: Maybe String
          , co_value  :: String 
          , co_canDisplay :: Maybe Bool 
          } deriving (Show,Generic) 

instance Eq AODocsColumn where 
  (AODocsColumn _ co_name1 _ _ _) == (AODocsColumn _ co_name2 _ _ _) = co_name1 == co_name2

instance Ord AODocsColumn where
  (AODocsColumn _ co_name1 _ _ _) `compare` (AODocsColumn _ co_name2 _ _ _) = co_name1 `compare` co_name2 

instance ToJSON AODocsColumn where 
    toJSON (AODocsColumn co_kind co_name co_name_i18n co_value co_canDisplay) =
        object ["kind" .= co_kind, "name" .= co_name, 
                "name_i18n" .= co_name_i18n, "value" .= co_value, 
                "canDisplay" .= co_canDisplay
                ]

instance FromJSON AODocsColumn where 
    parseJSON = withObject "AODocsColumn" $ \v -> AODocsColumn
        <$> v .: "kind"
        <*> v .: "name"
        <*> v .:? "name_i18n" 
        <*> v .: "value"
        <*> v .:? "canDisplay"         

aODocsColumnDefault = AODocsColumn "" "" (Just "") "" (Just False) 

--   ADT - Sum and Product Example 
--       { "id": "Pk6OvuY3LL47Iy0kpr",  This is a unqiue key 
--        "name": "Manuals",
--        "name_i18n": "Manuals",  
--        "value": "true" } 

data AODocsVisTypeBox  =
  AODocsVisTypeBox { 
            vt_type :: AODocsVisType   
          , vt_value :: Maybe String
          , vt_displayName :: Maybe String    
          , vt_description :: Maybe String                    
          , vt_thumbnailPictureUrl :: Maybe String            
           } deriving (Show,Generic) 

instance ToJSON AODocsVisTypeBox where 
    toJSON (AODocsVisTypeBox vt_type vt_value vt_displayName vt_description vt_thumbnailPictureUrl ) = 
        object ["type" .= vt_type,  "value" .= vt_value 
               ,"displayName" .= vt_displayName, "description" .= vt_description 
               ,"thumbnailPictureUrl" .= vt_thumbnailPictureUrl ] 

instance FromJSON AODocsVisTypeBox where 
    parseJSON = withObject "AODocsVisTypeBox" $ \v -> AODocsVisTypeBox
        <$> v .: "type"        
        <*> v .:? "value" 
        <*> v .:? "displayName" 
        <*> v .:? "description"         
        <*> v .:? "thumbnailPictureUrl" 

instance Eq AODocsVisTypeBox where 
  (AODocsVisTypeBox vtb11 vtb12 vtb13 vtb14 vtb15 ) == (AODocsVisTypeBox vtb21 vtb22 vtb23 vtb24 vtb25 ) 
   = vtb11 == vtb21 && vtb12 == vtb22 && vtb13 == vtb23 && vtb14 == vtb24 && vtb15 == vtb25    

data AODocsPermissionV2 = 
  AODocsPermissionV2   { 
            pm_kind :: String
          , pm_type :: AODocsVisType 
          , pm_role :: AODocsPermissionRole 
          , pm_value :: String  
          , pm_displayName ::  String
          , pm_thumbnailPictureUrl :: Maybe String
           } deriving (Show,Generic) 

instance ToJSON AODocsPermissionV2 where 
    toJSON (AODocsPermissionV2 pm_kind pm_type pm_role pm_value pm_displayName pm_thumbnailPictureUrl ) = 
        object ["kind" .= pm_kind ,"type" .= pm_type
               ,"role" .= pm_role ,"value" .= pm_value 
               ,"displayName" .= pm_displayName ,"thumbnailPhotoUrl" .= pm_thumbnailPictureUrl ] 

instance FromJSON AODocsPermissionV2 where 
    parseJSON = withObject "AODocsPermissionV2" $ \v -> AODocsPermissionV2 
        <$> v .: "kind" 
        <*> v .: "type" 
        <*> v .: "role" 
        <*> v .: "value" 
        <*> v .: "displayName" 
        <*> v .:? "thumbnailPhotoUrl"        

instance Eq AODocsPermissionV2 where 
  (AODocsPermissionV2 _ type1 role1 value1 _ _ ) == 
    (AODocsPermissionV2 _ type2 role2 value2 _ _ )   
      = type1 == type2 && role1 == role2 && value1 == value2  

instance Ord AODocsPermissionV2 where
  (AODocsPermissionV2 _ _ _ value1 _ _ ) `compare` 
    (AODocsPermissionV2 _ _ _ value2  _ _ ) 
      = value1 `compare` value2          

aODocsPermissionV2Default = AODocsPermissionV2 "" NOTVISIBLE NOPERMISSION "" "" (Just "") 

--    property function composed with inverse function  
--prop_inverse_B_to_JSON_to_B :: (ToJSON a, FromJSON a) => a -> B.ByteString -> B.ByteString -> Bool 
--prop_inverse_B_to_JSON_to_B :: B.ByteString -> B.ByteString -> Bool 
--prop_inverse_B_to_JSON_to_B a defaultB b = encode (fromRight defaultB (eitherDecode b)) == b  

prop_inverse_B_to_JSON_to_B :: (ToJSON B.ByteString, FromJSON B.ByteString) => B.ByteString -> B.ByteString -> Bool 
prop_inverse_B_to_JSON_to_B defaultB b = encode (fromRight defaultB (eitherDecode b)) == b  

prop_inverse_JSON_to_B_to_JSON :: (FromJSON a, ToJSON a, Eq a) => a -> a -> Bool 
prop_inverse_JSON_to_B_to_JSON defaultA a = fromRight defaultA (eitherDecode (encode a)) == a 

