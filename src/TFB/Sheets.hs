-- | Interacting with Google sheets

{-# LANGUAGE OverloadedStrings #-}

module TFB.Sheets where

import TFB.Types (FieldType(..), FieldVal(..), Config(..), MsgItem(..))
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
import Network.Google hiding (Env)
import System.IO (stdout)

sheetWorker :: Env TFB -> IO ()
sheetWorker Env{ envConfig=cfg, envQueue=mq } = do
  forever $ do
    let sheetId = "TODO"
    msg <- atomically $ readTBQueue mq
    print msg
    case msg of
      MsgInfo ans -> appendRow cfg sheetId ans

appendRow :: Config -> Text -> Map Text FieldVal -> IO Text
appendRow cfg sheetId ans = do
  let fields = [] -- getFieldNames cfg
      orderedAns = map (ans M.!) fields
  appendGS sheetId "raw" orderedAns

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
toJV (ValTime ts) = pure $ pure $ toJSON ts
toJV (ValUser u) = pure $ pure $ toJSON u
