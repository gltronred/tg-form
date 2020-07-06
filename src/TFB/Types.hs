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
import Telegram.Bot.API.Types (UserId(..))

data FieldType
  = FieldInt -- OpType
  | FieldFloat -- OpType
  | FieldText
  deriving (Eq,Show,Read,Generic)

instance FromJSON FieldType where
  parseJSON = genericParseJSON $ jsonOpts 5 0
instance ToJSON FieldType where
  toEncoding = genericToEncoding $ jsonOpts 5 0

deriving instance Read UserId

data FieldVal
  = ValInt Int
  | ValFloat Double
  | ValText Text
  | ValTime Int
  | ValUser Text
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

data QuestionDef = QuestionDef
  { qdText :: Text
  , qdAnswer :: [Text]
  , qdError :: Maybe Text
  } deriving (Eq,Show,Read,Generic)

instance FromJSON QuestionDef where
  parseJSON = genericParseJSON $ jsonOpts 8 2
instance ToJSON QuestionDef where
  toEncoding = genericToEncoding $ jsonOpts 8 2

data BotCfg = BotCfg
  { bcToken :: Text
  } deriving (Eq,Show,Generic)

instance FromJSON BotCfg where
  parseJSON = genericParseJSON $ jsonOpts 3 2
instance ToJSON BotCfg where
  toEncoding = genericToEncoding $ jsonOpts 3 2

data TargetCfg = TargetCfg
  { tcSheets :: Text
  } deriving (Eq,Show,Read,Generic)

instance FromJSON TargetCfg where
  parseJSON = genericParseJSON $ jsonOpts 6 2
instance ToJSON TargetCfg where
  toEncoding = genericToEncoding $ jsonOpts 6 2

data Config = Config
  { cfgTimeField :: Text
  , cfgSourceField :: Text
  , cfgFields :: [FieldDef]
  , cfgGeoFile :: FilePath
  , cfgWelcome :: Text
  , cfgRegisterButton :: Maybe Text
  , cfgRegisterAnswer :: Maybe Text
  , cfgTrustAnswer :: Maybe Text
  , cfgLocationText :: Maybe Text
  , cfgQuestions :: [QuestionDef]
  , cfgBot :: BotCfg
  , cfgTargets :: TargetCfg
  , cfgResult :: Text
  } deriving (Eq,Show,Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON $ jsonOpts 6 3
instance ToJSON Config where
  toEncoding = genericToEncoding $ jsonOpts 6 3

getFieldNames :: Config -> [Text]
getFieldNames cfg = cfgTimeField cfg : cfgSourceField cfg : map fdName (cfgFields cfg)

data State
  = NotStarted
  | Answered
    { stSrc :: Text
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

data MsgItem
  = MsgInfo (Map Text FieldVal)
  | MsgTrust (Int, Text, Text)
  deriving (Eq,Show,Read,Generic)

jsonOpts :: Int -> Int -> Options
jsonOpts m k = defaultOptions
  { fieldLabelModifier = camelTo2 '-' . drop k
  , constructorTagModifier = camelTo2 '-' . drop m
  }
