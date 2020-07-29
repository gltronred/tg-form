-- | Types of all entities

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TFB.Types where

import Data.Aeson
import Data.Map.Strict (Map,empty)
import Data.Text (Text)
import GHC.Generics
import Telegram.Bot.API.Types (UserId(..),Location(..))

data NamedCoord = NamedCoord
  { ncName :: Text
  , ncLat :: Double
  , ncLon :: Double
  } deriving (Eq,Show,Read,Generic)

instance FromJSON NamedCoord where
  parseJSON = genericParseJSON $ jsonOpts 2 0
instance ToJSON NamedCoord where
  toEncoding = genericToEncoding $ jsonOpts 2 0

data LocPrecision
  = PrecCoord
  | PrecCity
  | PrecMunicip
  | PrecSubregion
  | PrecRegion
  deriving (Eq,Show,Read,Generic)

instance FromJSON LocPrecision where
  parseJSON = genericParseJSON $ jsonOpts 4 0
instance ToJSON LocPrecision where
  toEncoding = genericToEncoding $ jsonOpts 4 0

data FieldType
  = FieldInt
  | FieldNum
  | FieldText
  | FieldEnum [[Text]]
  | FieldLocation LocPrecision
  | FieldTime
  | FieldSource
  | FieldWelcome
  | FieldThanks
  deriving (Eq,Show,Read,Generic)

instance FromJSON FieldType where
  parseJSON = genericParseJSON $ jsonOpts 5 0
instance ToJSON FieldType where
  toEncoding = genericToEncoding $ jsonOpts 5 0

deriving instance Read UserId

data FieldVal
  = ValInt Int
  | ValNum Double
  | ValText Text
  | ValEnum Text
  | ValLocation NamedCoord
  | ValTime Int
  | ValUser UserId
  | ValVoid
  deriving (Eq,Show,Read,Generic)

instance FromJSON FieldVal where
  parseJSON = genericParseJSON $ jsonOpts 3 3
instance ToJSON FieldVal where
  toEncoding = genericToEncoding $ jsonOpts 3 3

data FieldDef = FieldDef
  { fdName :: Text
  , fdDesc :: Text
  , fdType :: FieldType
  } deriving (Eq,Show,Read,Generic)

instance FromJSON FieldDef where
  parseJSON = genericParseJSON $ jsonOpts 5 2
instance ToJSON FieldDef where
  toEncoding = genericToEncoding $ jsonOpts 5 2

data FormConfig = FormConfig
  { cfgCode :: Text
  , cfgDocumentId :: Text
  , cfgConfigSheet :: Text
  , cfgResultSheet :: Text
  , cfgAuthor :: UserId
  , cfgFields :: [FieldDef]
  } deriving (Eq,Show,Read,Generic)

instance FromJSON FormConfig where
  parseJSON = genericParseJSON $ jsonOpts 10 3
instance ToJSON FormConfig where
  toEncoding = genericToEncoding $ jsonOpts 10 3

data Config = Config
  { cfgConnection :: String
  , cfgPoolSize :: Maybe Int
  , cfgGeoFile :: Maybe FilePath
  , cfgToken :: Text
  , cfgServiceMail :: Text
  , cfgManual :: Text
  } deriving (Eq,Show,Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON $ jsonOpts 6 3
instance ToJSON Config where
  toEncoding = genericToEncoding $ jsonOpts 6 3

data State
  = NotStarted
  | PreparingSheet
    { stSrc :: UserId
    , stDocId :: Maybe Text
    }
  | Answered
    { stSrc :: UserId
    , stForm :: FormConfig
    , stCurrent :: Int
    , stAnswers :: Map Text FieldVal
    }
  deriving (Eq,Show,Read,Generic)

getCurrent :: State -> Int
getCurrent Answered{ stCurrent=n } = n
getCurrent _ = 0

getAnswers :: State -> Map Text FieldVal
getAnswers Answered{ stAnswers=m } = m
getAnswers _ = empty

deriving instance Read Location

data Answer = Answer
  { ansText :: Text
  , ansLocation :: Maybe Location
  , ansUserId :: UserId
  } deriving (Read,Show)

data Action
  = NoOp
  | Start (Maybe Text) UserId -- /start [<code>] - starts form <code> or preparing (no code)
  | Help                      -- /help
  | Ans Answer                -- <text> - give answer <text> to current question
  | Cancel                    -- /cancel - remove current answers and restart form
  | NewForm UserId Text       -- /newform - remove current answers and start creating form
  | Parsed FieldDef FieldVal
  | GoForm FormConfig UserId
  | AskCurrent
  deriving (Read,Show)

data MsgItem
  = MsgInfo FormConfig (Map Text FieldVal)
  deriving (Eq,Show,Read,Generic)

jsonOpts :: Int -> Int -> Options
jsonOpts m k = defaultOptions
  { fieldLabelModifier = camelTo2 '-' . drop k
  , constructorTagModifier = camelTo2 '-' . drop m
  }
