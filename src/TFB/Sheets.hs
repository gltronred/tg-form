-- | Interacting with Google sheets

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TFB.Sheets where

import TFB.Types ( FieldType(..)
                 , FieldDef(..)
                 , FieldVal(..)
                 , FormConfig(..)
                 , MsgItem(..))
import TFB.Env (TFB,Env(Env,envConfig,envQueue))

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as ET
import Network.Google.Sheets
import Network.Google hiding (Env, Error)
import System.IO (stdout)

sheetWorker :: Env TFB -> IO ()
sheetWorker Env{ envConfig=cfg, envQueue=mq } = do
  forever $ do
    msg <- atomically $ readTBQueue mq
    print msg
    case msg of
      MsgInfo form ans -> appendRow form ans

parseConfig :: Text -> Text -> IO [FieldDef]
parseConfig docId sheet = do
  let range = "A:D"
      req = spreadsheetsValuesGet docId $ sheet <> "!" <> range
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
                    , cfgResultSheet=sheet
                    , cfgFields=fs } ans = do
  let fields = map fdName fs
      orderedAns = map (ans M.!) fields
  appendGS docId sheet orderedAns

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
toJV (ValLocation (lat,lon)) = pure [toJSON lat, toJSON lon]
toJV (ValTime ts) = pure $ pure $ toJSON ts
toJV (ValUser u) = pure $ pure $ toJSON u
toJV ValVoid = pure []
