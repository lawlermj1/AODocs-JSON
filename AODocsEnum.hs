{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AODocsEnum 
   Description : Sum types for AODocs APIs  
   Copyright   : ( c ) Matthew Lawler 2020   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module defines Sum types needed for AODocs JSON functions. 

   Best site: https://artyom.me/aeson 
   
 -}
module AODocsEnum 
    (  
      
      AODocsCapability( .. ), 
      AODocsFilterOperator( .. ), 
      AODocsType( .. ), 
      AODocsVisType( .. ), 
      AODocsJoinType( .. ), 
      AODocsPublishState( .. ), 
      AODocsPage( .. ), 
      AODocsTab( .. ), 
      AODocsFoldertype( .. ), 
      AODocsVisLevel( .. ),
      AODocsWFDisplay( .. ),
      AODocsWFCondition( .. ),
      AODocsAttachment( .. ),   
      AODocsPermissionMode( .. ),  
      AODocsReferenceCatalog( .. ),
      AODocsUserRight( .. ),
      AODocsCheckoutMode( .. ),
      AODocsPermissionRole( .. ),
      AODocsApplyActionToDocuments( .. ), 

     ) where
    

import Data.Aeson
import GHC.Generics
import Control.Monad 
import Data.Csv (FromField, ToField, parseField, toField) 
--import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered) 

----- Sum types 
data AODocsFoldertype = 
            DML | SF | TF | ZFT 
              deriving (Show,Read,Eq,Ord,Generic)

data AODocsCapability = 
            BROWSE_BY | DISPLAY | FILTER_ON | ORDER_BY | PRE_FILTER_ON 
              deriving (Show,Read,Eq,Ord,Generic)

data AODocsFilterOperator =  
            EMPTY | EQUAL | GREATER_THAN_OR_EQUAL_TO | IN | LESSER_THAN_OR_EQUAL_TO | 
            NOT_EMPTY | NOT_EQUAL | NOT_IN | 
            STRICTLY_GREATER_THAN | STRICTLY_LESSER_THAN | WITHIN_THE_AREA 
              deriving (Show,Read,Eq,Ord,Generic)

data AODocsType = 
            BOOLEAN | CATEGORY | CLASS | DATE | DATETIME | DECIMAL | FILESIZE | 
            FOLDER | GEOPOINT | INTEGER | MIMETYPEGROUP | PERSON | 
            REFERENCE_CATALOG | STATE | STRING | TEXT | TIME | URL | WORKFLOW 
              deriving (Show,Read,Eq,Ord,Generic) 

data AODocsVisType =  DOMAIN | GROUP | NONE | PRIVATE | PUBLIC | PUBLICLY_EDITABLE | ROLE | USER | NOTVISIBLE 
               deriving (Show,Read,Eq,Ord,Generic)

data AODocsJoinType =  AND | OR  
               deriving (Show,Read,Eq,Ord,Generic)   

data AODocsVisLevel =  READ | WRITE   
               deriving (Show,Read,Eq,Ord,Generic)   

data AODocsWFDisplay =  HIDDEN_FROM_WORKFLOW_TASKS | HIDDEN_FROM_BROWSEBY | HIDDEN_FROM_VIEW_FILTERS 
               deriving (Show,Read,Eq,Ord,Generic)  

data AODocsWFCondition =  PROPERTY_TIME | HUMAN_ACTION | PROPERTY_DATA | RELATIVE_TIME | ELAPSED_TIME 
               deriving (Show,Read,Eq,Ord,Generic)                                                                      

data AODocsAttachment =  COMPOSITE | GOOGLE_DRIVE | DRIVE_CONTROLLED 
               deriving (Show,Read,Eq,Ord,Generic)   

data AODocsPermissionMode =  LOW_SECURITY_PRISON | CLUB_MED  
               deriving (Show,Read,Eq,Ord,Generic) 

data AODocsCheckoutMode =  DISABLED | OPTIONAL  
               deriving (Show,Read,Eq,Ord,Generic) 

data AODocsPermissionRole =  WRITER | READER | COMMENTER | NOPERMISSION 
               deriving (Show,Read,Eq,Ord,Generic) 

data AODocsApplyActionToDocuments = CHECK_IN | CHECK_OUT | DISCARD | DO_NOTHING | NORMAL  
               deriving (Show,Read,Eq,Ord,Generic)                

--data AODocsPermissionValue =  CONTRIBUTORS | ZPV    
--               deriving (Show,Read,Eq,Ord,Generic)                

-- added dummy ZPS ZZD ZZT which fixes a generic bug, 
-- where an array is expected if only a single instance sum type 
data AODocsPublishState = ALL | ZPS 
              deriving (Show,Read,Eq,Ord,Generic)             

data AODocsPage = DOCUMENT | ZZD 
              deriving (Show,Read,Eq,Ord,Generic)

data AODocsTab = CURRENT | ZZT 
              deriving (Show,Read,Eq,Ord,Generic)

data AODocsReferenceCatalog = LIBRARY_LABELS | ZZR 
              deriving (Show,Read,Eq,Ord,Generic)

data AODocsUserRight =  ADMIN | ZZA   
               deriving (Show,Read,Eq,Ord,Generic)                

instance ToJSON AODocsCapability 
instance FromJSON AODocsCapability

instance ToJSON AODocsFilterOperator 
instance FromJSON AODocsFilterOperator

instance ToJSON AODocsType 
instance FromJSON AODocsType

instance ToJSON AODocsPublishState 
instance FromJSON AODocsPublishState

instance ToJSON AODocsPage 
instance FromJSON AODocsPage

instance ToJSON AODocsTab  
instance FromJSON AODocsTab 

instance ToJSON AODocsVisType  
instance FromJSON AODocsVisType 

instance ToJSON AODocsJoinType  
instance FromJSON AODocsJoinType 

instance ToJSON AODocsVisLevel  
instance FromJSON AODocsVisLevel  

instance ToJSON AODocsWFDisplay  
instance FromJSON AODocsWFDisplay   

instance ToJSON AODocsWFCondition  
instance FromJSON AODocsWFCondition    

instance ToJSON AODocsAttachment  
instance FromJSON AODocsAttachment   

instance ToJSON AODocsPermissionMode  
instance FromJSON AODocsPermissionMode   

instance ToJSON AODocsReferenceCatalog  
instance FromJSON AODocsReferenceCatalog   

instance ToJSON AODocsUserRight  
instance FromJSON AODocsUserRight   

instance ToJSON AODocsCheckoutMode  
instance FromJSON AODocsCheckoutMode   

instance ToJSON AODocsPermissionRole  
instance FromJSON AODocsPermissionRole    

instance ToJSON AODocsApplyActionToDocuments  
instance FromJSON AODocsApplyActionToDocuments   

--  this is the pattern for any sum type. 
--  there will be a way to use template hs to do this. 
instance FromField AODocsApplyActionToDocuments where
    parseField s
        | s == "CHECK_IN"  = pure CHECK_IN
        | s == "CHECK_OUT" = pure CHECK_OUT 
        | s == "DISCARD"   = pure DISCARD         
        | s == "DO_NOTHING" = pure DO_NOTHING                
        | s == "NORMAL"    = pure NORMAL
        | otherwise = mzero 

instance ToField AODocsApplyActionToDocuments where
    toField CHECK_IN = "CHECK_IN"
    toField CHECK_OUT = "CHECK_OUT"    
    toField DISCARD = "DISCARD"  
    toField DO_NOTHING = "DO_NOTHING"  
    toField NORMAL = "NORMAL"  
