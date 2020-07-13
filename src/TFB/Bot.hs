-- | Main telegram bot module

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TFB.Bot where

import TFB.Types
import TFB.Sheets

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read
import Data.Time.Clock.POSIX
import GHC.Float (float2Double)
import System.Exit
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

initialModel :: State
initialModel = NotStarted

location :: UpdateParser Location
location = mkParser $ updateMessage >=> messageLocation

user :: UpdateParser UserId
user = mkParser $ fmap (fmap userId) $ updateMessage >=> messageFrom

cmd :: Text -> UpdateParser Text
cmd name = do
  t <- text
  case T.words t of
    (w : ws) | w == "/" <> name -> pure (T.unwords ws)
    _ -> empty

plaintext :: UpdateParser Text
plaintext = do
  t <- text
  if "/" `T.isPrefixOf` t
    then empty
    else pure t

updateToAction :: State -> Update -> Maybe Action
updateToAction NotStarted = parseUpdate $
      Start Nothing <$ cmd "start"
  <|> Help <$ cmd "help"
  <|> callbackQueryDataRead
updateToAction _st = parseUpdate $
      Help <$ cmd "help"
  <|> Ans <$> plaintext
  <|> callbackQueryDataRead

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) (x:_)  0 = Just x
(!?) (_:xs) n = xs !? (n-1)

runParser :: Reader a -> Text -> Either String a
runParser p t = fst <$> p t

parseFieldVal :: FieldType -> Text -> Either String FieldVal
parseFieldVal field txt = case field of
  FieldInt -> ValInt <$> runParser decimal txt
  FieldNum -> ValNum <$> runParser double txt
  FieldText -> Right $ ValText txt

addAnswer :: FieldDef -> Map Text FieldVal -> Text -> Either String (Map Text FieldVal)
addAnswer field olds txt = case parseFieldVal (fdType field) txt of
  Left e -> Left e
  Right v -> Right $ M.insert (fdName field) v olds

mkQuestion :: FieldDef -> ReplyMessage
mkQuestion field = toReplyMessage (fdDesc field)
  -- where
  --   regKeyboard = ReplyKeyboardMarkup
  --     { replyKeyboardMarkupKeyboard =
  --       [ [ KeyboardButton regButtonName Nothing (Just True) ] ]
  --     , replyKeyboardMarkupResizeKeyboard = Just True
  --     , replyKeyboardMarkupOneTimeKeyboard = Just True
  --     , replyKeyboardMarkupSelective = Nothing
  --     }

handleAction :: Config -> TBQueue MsgItem -> Action -> State -> Eff Action State
handleAction cfg mq act st@NotStarted = case act of
  NoOp -> pure st
  Start Nothing -> st <# do
    -- prepare sheet here!
    reply (toReplyMessage "Welcome!")
    pure NoOp
  Start (Just code) -> st <# do
    -- get form here!
    reply (toReplyMessage "Some questions")
    pure $ NoOp
  GoForm form -> pure $ Answered
    { stSrc = ""
    , stForm = form
    , stCurrent = 0
    , stAnswers = M.empty
    }
  -- Help and others
  _ -> st <# do
    reply $ toReplyMessage $ botUsage cfg
    pure NoOp
handleAction cfg mq act st@Answered{ stForm=form, stCurrent=current, stAnswers=olds } = case act of
  NoOp -> pure st
  AskCurrent -> let
    mfield = cfgFields form !? current
    in case mfield of
         Nothing -> NotStarted <# do
           reply $ toReplyMessage $ cfgThanks form
           pure NoOp
         Just field -> st <# do
           reply $ mkQuestion field
           pure NoOp
  Ans t -> let
    mfield = cfgFields form !? current
    in case mfield of
         Nothing -> pure NotStarted
         Just field -> case addAnswer field (getAnswers st) t of
           Left err -> st <# do
             reply (toReplyMessage $ T.pack err)
             pure AskCurrent
           Right ans -> let
             idx = current + 1
             st' = if idx < length (cfgFields form)
                   then st { stCurrent = idx, stAnswers = ans }
                   else NotStarted
             in st' <# do
               when (st' == NotStarted) $ do
                 liftIO $ atomically $ writeTBQueue mq $ MsgInfo ans
               pure AskCurrent
  -- Help and others
  _ -> st <# do
    reply $ toReplyMessage $ botUsage cfg
    pure NoOp

botUsage :: Config -> Text
botUsage cfg = T.concat $
  [ "This bot collects statistics and writes it into Google Sheets\n\n"
  , "Your telegram user "
  , "\n"
  , "Bot also saves time and your approximate location\n\n"
  , "Author provided the following description: \n"
  , "\n"
  , "Questions asked and statistics collected:\n"
  ]
  where
    mkFieldDesc :: Map Text FieldDef -> Text -> Text
    mkFieldDesc fields name = let
      field = fields M.! name
      in T.concat [ " + ", name, " - ", fdDesc field, " - ", mkTypeDesc $ fdType field, "\n" ]
    mkTypeDesc FieldInt = "integer"
    mkTypeDesc FieldNum = "floating point number"
    mkTypeDesc FieldText = "text"

collectBot :: Config -> TBQueue MsgItem -> BotApp State Action
collectBot cfg mq = BotApp
  { botInitialModel = initialModel
  , botAction = flip updateToAction
  , botHandler = handleAction cfg mq
  , botJobs = []
  }

run :: Config -> IO ()
run cfg@Config{ cfgToken = token } = do
  mq <- newTBQueueIO 1000
  _ <- forkIO $ sheetWorker cfg mq
  env <- defaultTelegramClientEnv $ Token token
  startBot_ (conversationBot updateChatId $ collectBot cfg mq) env

readConfigOrDie :: FilePath -> IO Config
readConfigOrDie cfgFile = do
  ecfg <- eitherDecodeFileStrict cfgFile
  case ecfg of
    Left err -> die err
    Right cfg -> pure cfg
