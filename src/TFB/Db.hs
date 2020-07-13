-- | Database module

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TFB.Db where

import TFB.Types

import Data.Int
import Data.Text (Text)
import Database.Groundhog.Converters
import Database.Groundhog.TH
import Database.Groundhog.Sqlite
import GHC.Generics
import Telegram.Bot.API.Types (UserId(..))

data PreFormConfig = PFC
  { pfcCode :: Text
  , pfcConfigSheet :: Text
  , pfcResultSheet :: Text
  , pfcWelcome :: UserId
  } deriving (Eq,Show,Generic)

data PreFieldDef = PFD
  { pfdName :: Text
  , pfdDesc :: Text
  , pfdType :: FieldType
  , pfdForm :: DefaultKey PreFormConfig
  } deriving (Generic)

userIdConverter :: Converter UserId Int32
userIdConverter = makeConverter (\(UserId x) -> x) UserId

mkPersist defaultCodegenConfig [groundhog|
- entity: PreFormConfig
- entity: PreFieldDef
- primitive: FieldType
  converter: jsonConverter
- primitive: UserId
  converter: userIdConverter
|]
