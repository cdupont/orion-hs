{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Orion.Types where

import           Network.Wreq as W hiding (delete)
import           Control.Lens hiding ((.=))
import           Data.Aeson as JSON
import           Data.Aeson.BetterErrors as AB
import           Data.Text hiding (head, tail, find, map, filter, drop, toLower)
import           Data.String
import           GHC.Generics (Generic)
import           Data.Maybe
import           Data.String.Conversions
import           Control.Monad.Reader
import           Data.Foldable as F
import           Network.HTTP.Client (HttpException)
import           Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.HashMap.Lazy as HML 
import           Data.Map as M hiding (map, drop, toLower)
import           Data.Char
import           Debug.Trace

-- * Orion monad

type Orion a = ReaderT OrionConfig (ExceptT OrionError IO) a

data OrionError = HTTPError HttpException  -- ^ Keycloak returned an HTTP error.
                | ParseError Text          -- ^ Failed when parsing the response
                | EmptyError               -- ^ Empty error to serve as a zero element for Monoid.

-- * Orion config

data OrionConfig = OrionConfig {
  _orionUrl      :: Text,
  _fiwareService :: Text} deriving (Show, Eq)

defaultOrionConfig = OrionConfig {
  _orionUrl      = "http://localhost:1026",
  _fiwareService = "waziup"}

-- | Run an Orion monad within IO.
runOrion :: Orion a -> OrionConfig -> IO (Either OrionError a)
runOrion o conf = runExceptT $ runReaderT o conf

-- * Entities

newtype EntityId = EntityId {unEntityId :: Text} deriving (Show, Eq, Generic, ToJSON, FromJSON)
type EntityType = Text

data Entity = Entity {
  entId    :: EntityId,
  entType  :: EntityType,
  entAttrs :: Map AttributeId Attribute
  } deriving (Generic, Show)

instance ToJSON Entity where
   toJSON (Entity entId entType attrs) = mergeAeson $ [object ["id" .= entId, "type" .= entType], toJSON attrs]

instance FromJSON Entity where
  parseJSON o@(Object v) = do
    eId   <- v .: "id"
    eType <- v .: "type"
    attrs <- parseJSON $ Object (HML.delete "id" $ HML.delete "type" v) 
    return $ Entity (EntityId eId) eType attrs where

-- * Attributes

newtype AttributeId = AttributeId {unAttributeId :: Text} deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey, IsString)
type AttributeType = Text

data Attribute = Attribute {
  attType     :: AttributeType,
  attValue    :: Maybe Value,
  attMetadata :: Map MetadataId Metadata
  } deriving (Generic, Show)

instance ToJSON Attribute where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

instance FromJSON Attribute where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

-- * Metadata

newtype MetadataId = MetadataId {unMeetadataId :: Text} deriving (Show, Eq, Ord, Generic, ToJSONKey, FromJSONKey, IsString)
type MetadataType = Text

data Metadata = Metadata {
  metType  :: Maybe MetadataType,
  metValue :: Maybe Value
  } deriving (Generic, Show)

instance ToJSON Metadata where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

instance FromJSON Metadata where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = unCapitalize . drop 3}


-- * Subscriptions

newtype SubId = SubId {unSubId :: Text} deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | one subscription
data Subscription = Subcription {
  subId          :: SubId,       -- ^ id of the notification (attributed by the server)
  subDescription :: Text,                -- ^ Description of the notification
  subSubject     :: SubSubject,        -- ^ 
  subNotif       :: SubNotif,  -- ^ 
  subThrottling  :: Double              -- ^ minimum interval between two messages in seconds
  } deriving (Show, Eq, Generic)

instance ToJSON Subscription where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

instance FromJSON Subscription where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

data SubSubject = SubSubject {
  subEntities  :: [SubEntities],
  subCondition :: SubCondition
  } deriving (Show, Eq, Generic)

instance ToJSON SubSubject where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

instance FromJSON SubSubject where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

data SubEntities = SubEntities {
  subIdPattern :: Text,
  subType      :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON SubEntities where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

instance FromJSON SubEntities where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

data SubNotif = SubNotif {
  subHttp :: Map String String, 
  subAttrs :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON SubNotif where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

instance FromJSON SubNotif where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

data SubCondition = SubCondition {
  subExpression :: Map String String
  } deriving (Show, Eq, Generic)

instance ToJSON SubCondition where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

instance FromJSON SubCondition where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = unCapitalize . drop 3}

-- Miscellaneous

type Path = Text

mergeAeson :: [Value] -> Value
mergeAeson = Object . HML.unions . map (\(Object x) -> x)

unCapitalize :: String -> String
unCapitalize (c:cs) = toLower c : cs
unCapitalize [] = []

makeLenses ''OrionConfig
