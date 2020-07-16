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
import Data.Text (Text)
import Data.Time
import Database.Groundhog.Converters
import Database.Groundhog.TH
import Database.Groundhog.Sqlite
import GHC.Generics
import Telegram.Bot.API.Types (UserId(..))

data PreFormConfig = PFC
  { preCode :: Text
  , preConfigSheet :: Text
  , preResultSheet :: Text
  , preWelcome :: Text
  , preThanks :: Text
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
          , preConfigSheet=cs
          , preResultSheet=rs
          , preWelcome=w
          , preThanks=t
          , preAuthor=a
          } fds
  = FormConfig{ cfgCode=c
              , cfgConfigSheet=cs
              , cfgResultSheet=rs
              , cfgWelcome=w
              , cfgThanks=t
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
  , preConfigSheet = cfgConfigSheet f
  , preResultSheet = cfgResultSheet f
  , preWelcome = cfgWelcome f
  , preThanks = cfgThanks f
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
  mpre <- project (AutoKeyField, PFCConstructor) $ (PreCodeField ==. code) `limitTo` 1
  case mpre of
    [] -> pure Nothing
    [(preId, form)] -> do
      preFields <- select $ (PreFormField ==. preId) `orderBy` [Asc PreOrderField]
      pure $ Just $ toForm form $ map toField preFields

saveForm :: FormConfig -> Connection -> IO ()
saveForm form = runDbConn $ do
  now <- liftIO getCurrentTime
  ek <- insertBy FormCode $ fromForm now form
  let k = either id id ek
  forM_ (zip [1..] $ cfgFields form) $ insert . fromField k

withDb :: String -> Int -> (Connection -> IO ()) -> IO ()
withDb connStr sz act = withSqlitePool connStr sz $ \conn -> do
  flip runDbConn conn $ do
    runMigration $ do
      migrate (undefined :: PreFormConfig)
      migrate (undefined :: PreFieldDef)
  act conn
