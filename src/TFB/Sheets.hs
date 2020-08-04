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

-- | Interacting with Google sheets

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TFB.Sheets where

import TFB.Types ( FieldType(..)
                 , FieldDef(..)
                 , FieldVal(..)
                 , NamedCoord(..)
                 , FormConfig(..)
                 , MsgItem(..))
import TFB.Env (TFB,Env(Env,envQueue))

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Network.Google.Sheets
import Network.Google hiding (Env, Error)
import System.IO (stdout)

sheetWorker :: Env TFB -> IO ()
sheetWorker Env{ envQueue=mq } = do
  forever $ do
    msg <- atomically $ readTBQueue mq
    print msg
    case msg of
      MsgInfo form ans -> appendRow form ans

parseConfig :: Text -> Text -> IO [FieldDef]
parseConfig docId sheetName = do
  let range = "A:D"
      req = spreadsheetsValuesGet docId $ sheetName <> "!" <> range
  lgr <- newLogger Debug stdout
  env <- newEnv
         <&> (envLogger .~ lgr)
         . (envScopes .~ spreadsheetsScope)
  resp <- runResourceT . runGoogle env $ send req
  let res2log :: Result FieldDef -> IO (Maybe FieldDef)
      res2log (Success x) = pure $ Just x
      res2log (Error e) = putStrLn ("Can't parse field: " <> e) >> pure Nothing
  fmap catMaybes $ forM (resp ^. vrValues) $ \case
    [nameV, descV, fieldV, paramV] -> res2log $ FieldDef
      <$> fromJSON nameV
      <*> fromJSON descV
      <*> case fromJSON fieldV :: Result Text of
            Success "enum" -> FieldEnum . parseEnumOptions <$> fromJSON paramV
            Success "location" -> FieldLocation <$> fromJSON paramV
            Success ty -> Error $ "Wrong type: " <> T.unpack ty
            Error e -> Error e
    [nameV, descV, fieldV] -> res2log $ FieldDef
      <$> fromJSON nameV
      <*> fromJSON descV
      <*> case fromJSON fieldV :: Result Text of
            Success "int" -> pure FieldInt
            Success "num" -> pure FieldNum
            Success "text"-> pure FieldText
            Success "time" -> pure FieldTime
            Success "user" -> pure FieldSource
            Success "welcome" -> pure FieldWelcome
            Success "thanks" -> pure FieldThanks
            Success ty -> Error $ "Wrong type: " <> T.unpack ty
            Error e -> Error e
    row -> putStrLn ("Can't parse " <> show row) >> pure Nothing

parseEnumOptions :: Text -> [[Text]]
parseEnumOptions = map (T.splitOn ";") . T.splitOn ";;"

appendRow :: FormConfig -> Map Text FieldVal -> IO Text
appendRow FormConfig{ cfgDocumentId=docId
                    , cfgResultSheet=sheetName
                    , cfgFields=fs } ans = do
  let fields = map fdName fs
      orderedAns = mapMaybe (`M.lookup` ans) fields
  appendGS docId sheetName orderedAns

appendGS :: Text -> Text -> [FieldVal] -> IO Text
appendGS sheetId name vals = do
  raws <- encodeVals vals
  let range = name <> "!A:" <> (T.pack $ pure $ chr $ ord 'A' + length raws - 1)
      values = valueRange
        & vrValues .~ [raws]
        & vrRange .~ Just range
        & vrMajorDimension .~ Just VRMDRows
      req = spreadsheetsValuesAppend sheetId values range
        & svaValueInputOption .~ Just "USER_ENTERED"
  lgr <- newLogger Debug stdout
  env <- newEnv
         <&> (envLogger .~ lgr)
         . (envScopes .~ spreadsheetsScope)
  resp <- runResourceT . runGoogle env $ send req
  pure $ T.pack $ show resp

encodeVals :: [FieldVal] -> IO [Value]
encodeVals vals = do
  rs <- mapM toJV vals
  let raws = concat rs
  pure raws

toJV :: FieldVal -> IO [Value]
toJV (ValInt n) = pure $ pure $ toJSON n
toJV (ValNum d) = pure $ pure $ toJSON d
toJV (ValText t) = pure $ pure $ toJSON t
toJV (ValEnum t) = pure $ pure $ toJSON t
toJV (ValLocation NamedCoord{ ncLat=lat, ncLon=lon }) = pure [toJSON lat, toJSON lon]
toJV (ValTime ts) = pure $ pure $ toJSON ts
toJV (ValUser u) = pure $ pure $ toJSON u
toJV ValVoid = pure []
