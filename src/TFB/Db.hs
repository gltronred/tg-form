{------------------------------------------------------------------------------

    tg-form - Telegram bot for creating forms from Google Sheets
    Copyright (C) 2020  Mansur Ziiatdinov

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>

------------------------------------------------------------------------------}

-- | Database module

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TFB.Db
  ( runDbConn
  , withDb
  , loadForm
  , saveForm
  ) where

import TFB.Types
import TFB.Env

import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import Data.Text (Text, toUpper)
import Data.Time
import Database.Groundhog.Converters
import Database.Groundhog.TH
import Database.Groundhog.Sqlite
import GHC.Generics
import Telegram.Bot.API.Types (UserId(..))

data PreFormConfig = PFC
  { preCode :: Text
  , preDocumentId :: Text
  , preConfigSheet :: Text
  , preResultSheet :: Text
  , preAuthor :: UserId
  , preUpdate :: UTCTime
  } deriving (Eq,Show,Generic)

data PreFieldDef = PFD
  { preOrder :: Int
  , preName :: Text
  , preDesc :: Text
  , preType :: FieldType
  , preForm :: DefaultKey PreFormConfig
  } deriving (Generic)

userIdConverter :: Converter UserId Int32
userIdConverter = makeConverter (\(UserId x) -> x) UserId

mkPersist defaultCodegenConfig [groundhog|
- entity: PreFormConfig
  keys:
  - name: FormCode
    fields:
    - name: preCode
  constructors:
  - name: PFC
    uniques:
    - name: FormCode
      fields: [preCode]
- entity: PreFieldDef
- primitive: FieldType
  converter: jsonConverter
- primitive: UserId
  converter: userIdConverter
|]

toForm :: PreFormConfig -> [FieldDef] -> FormConfig
toForm PFC{ preCode=c
          , preDocumentId=doc
          , preConfigSheet=cs
          , preResultSheet=rs
          , preAuthor=a
          } fds
  = FormConfig{ cfgCode=c
              , cfgDocumentId=doc
              , cfgConfigSheet=cs
              , cfgResultSheet=rs
              , cfgAuthor=a
              , cfgFields=fds
              }

toField :: PreFieldDef -> FieldDef
toField PFD{ preName=n
           , preDesc=d
           , preType=t
           } = FieldDef{ fdName=n
                       , fdDesc=d
                       , fdType=t
                       }

fromForm :: UTCTime -> FormConfig -> PreFormConfig
fromForm t f = PFC
  { preCode = cfgCode f
  , preDocumentId = cfgDocumentId f
  , preConfigSheet = cfgConfigSheet f
  , preResultSheet = cfgResultSheet f
  , preAuthor = cfgAuthor f
  , preUpdate = t
  }

fromField :: DefaultKey PreFormConfig -> (Int,FieldDef) -> PreFieldDef
fromField k (i,f) = PFD
  { preOrder = i
  , preName = fdName f
  , preDesc = fdDesc f
  , preType = fdType f
  , preForm = k
  }

loadForm :: Text -> Connection -> IO (Maybe FormConfig)
loadForm code = runDbConn $ do
  mpre <- project (AutoKeyField, PFCConstructor) $ (PreCodeField ==. toUpper code) `limitTo` 1
  case mpre of
    [] -> pure Nothing
    (preId, form):_ -> do
      preFields <- select $ (PreFormField ==. preId) `orderBy` [Asc PreOrderField]
      pure $ Just $ toForm form $ map toField preFields

saveForm :: FormConfig -> Connection -> IO ()
saveForm form = runDbConn $ do
  now <- liftIO getCurrentTime
  ek <- insertBy FormCode $ fromForm now form
  let k = either id id ek
  delete $ PreFormField ==. k
  forM_ (zip [1..] $ cfgFields form) $ insert . fromField k

withDb :: String -> Int -> (Connection -> IO ()) -> IO ()
withDb connStr sz act = withSqlitePool connStr sz $ \conn -> do
  flip runDbConn conn $ do
    runMigration $ do
      migrate (undefined :: PreFormConfig)
      migrate (undefined :: PreFieldDef)
  act conn
