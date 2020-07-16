-- | Main telegram bot module

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TFB.Bot where

import TFB.Types
import TFB.Env
import TFB.Db
import TFB.Sheets

import Colog (simpleMessageAction)
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read
import Data.Time.Clock.POSIX
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
      (\c u -> Start (if T.null c then Nothing else Just c) u) <$> cmd "start" <*> user
  <|> Help <$ cmd "help"
  <|> (\c u -> Start (Just c) u) <$> callbackQueryDataRead <*> user
updateToAction _st = parseUpdate $
      Help <$ cmd "help"
  <|> Ans <$> plaintext

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
  FieldEnum lopts -> case find (==txt) $ concat lopts of
    Nothing -> Left "Please, choose one of provided options"
    Just v  -> Right $ ValEnum v
  FieldLocation _prec -> Left "TODO: implement parsing location"
  FieldTime -> Left "Something went wrong: should not parse current time"
  FieldSource -> Left "Something went wrong: should not parse your user id"

addAnswer :: FieldDef -> Map Text FieldVal -> Text -> Either String (Map Text FieldVal)
addAnswer field olds txt = case parseFieldVal (fdType field) txt of
  Left e -> Left e
  Right v -> Right $ M.insert (fdName field) v olds

mkQuestion :: FieldDef -> UserId -> IO (Either FieldVal ReplyMessage)
mkQuestion field uid = case fdType field of
  FieldTime -> Left . ValTime . round <$> getPOSIXTime
  FieldSource -> pure $ Left $ ValUser uid
  FieldLocation prec -> pure $ Right msg { replyMessageReplyMarkup = mkKbd $ locKbd prec }
  FieldEnum opts -> pure $ Right msg { replyMessageReplyMarkup = mkKbd $ optKbd opts }
  FieldInt -> pure $ Right msg
  FieldNum -> pure $ Right msg
  FieldText -> pure $ Right msg
  where
    msg = toReplyMessage (fdDesc field)
    mkKbd kbd = Just $ SomeReplyKeyboardMarkup $ ReplyKeyboardMarkup
      { replyKeyboardMarkupKeyboard = kbd
      , replyKeyboardMarkupResizeKeyboard = Just True
      , replyKeyboardMarkupOneTimeKeyboard = Just True
      , replyKeyboardMarkupSelective = Nothing
      }
    optKbd = map optKbdRow
    optKbdRow = map (\t -> KeyboardButton t Nothing Nothing)
    locKbd prec = [ [ KeyboardButton (locBtn prec) Nothing (Just True) ] ]
    locBtn = ("Send my " <> ) . \case
      PrecCoord -> "coordinates"
      PrecCity -> "city"
      PrecMunicip -> "municipality"
      PrecRegion -> "region"

handleAction :: Env TFB -> Action -> State -> Eff Action State
handleAction env@Env{ envConfig=cfg } act st@NotStarted = case act of
  NoOp -> pure st
  Start Nothing u -> st <# do
    -- prepare sheet here!
    reply $ toReplyMessage addNewFormText
    pure NoOp
  Start (Just code) u -> st <# do
    -- get form here!
    let conn = envConn env
    mform <- liftIO $ loadForm code conn
    case mform of
      Nothing -> do
        reply $ toReplyMessage "Form not found!"
        pure NoOp
      Just form -> do
        reply (toReplyMessage $ cfgWelcome form)
        pure $ GoForm form u
  GoForm form u -> Answered
    { stSrc = u
    , stForm = form
    , stCurrent = 0
    , stAnswers = M.empty
    } <# do
    pure AskCurrent
  -- Cancel and Stop
  Cancel -> become NotStarted
  Stop -> become NotStarted
  -- Help and wrong commands
  Parsed _ _ -> usage cfg st Nothing
  AskCurrent -> usage cfg st Nothing
  Ans _ -> usage cfg st Nothing
  Help -> usage cfg st Nothing
handleAction env@Env{ envConfig=cfg } act st@PreparingSheet{} = case act of
  NoOp -> pure st
  Help -> usage cfg st Nothing
handleAction Env{ envConfig=cfg, envQueue=mq } act st@Answered{ stForm=form, stCurrent=current } = case act of
  NoOp -> pure st
  AskCurrent -> let
    mfield = cfgFields form !? current
    in case mfield of
         Nothing -> NotStarted <# do
           reply $ toReplyMessage $ cfgThanks form
           pure NoOp
         Just field -> st <# do
           q <- liftIO $ mkQuestion field $ stSrc st
           case q of
             Left v -> pure $ Parsed field v
             Right t -> reply t >> pure NoOp
  Parsed field v -> let
    k = fdName field
    ans = M.insert k v $ stAnswers st
    idx = current + 1
    st' = st { stCurrent = idx, stAnswers = ans }
    in st' <# do
      when (idx >= length (cfgFields form)) $ do
        liftIO $ atomically $ writeTBQueue mq $ MsgInfo ans
      pure AskCurrent
  Ans t -> let
    mfield = cfgFields form !? current
    in case mfield of
         Nothing -> pure NotStarted
         Just field -> case parseFieldVal (fdType field) t of
           Left err -> st <# do
             reply (toReplyMessage $ T.pack err)
             pure AskCurrent
           Right v -> st <# do
             pure $ Parsed field v
  -- Help and others
  Help -> usage cfg st $ Just form

usage cfg st mform = st <# do
  reply $ toReplyMessage $ botUsage cfg st mform
  pure NoOp

become st = st <# do
  reply $ toReplyMessage "Cancelling all your answers so far"
  pure NoOp

addNewFormText :: Text
addNewFormText = T.intercalate "\n"
  [ "To add a new form please do the following:"
  , "(see <...TODO...> for full manual)"
  , "1. Create a spreadsheet with configuration and result sheets"
  , "2. Share your spreadsheet with demo-bot@...TODO..."
  , "3. Send me the address of this spreadsheet"
  , ""
  , "Remember that Google limits the number of API calls"
  , "Consult <https://sr.ht/~rd/tg-form...TODO...> for details and upgrading to paid account"
  ]

botUsage :: Config -> State -> Maybe FormConfig -> Text
botUsage cfg st form = T.concat $
  [ "This bot collects answers and writes it into Google Sheets\n\n"
  , "Your telegram user "
  , "\n"
  , "Bot also saves time and your approximate location\n\n"
  , "Author provided the following description: \n"
  , "\n"
  , "Questions asked and statistics collected:\n"
  , T.pack $ show st
  ]
  where
    mkFieldDesc :: Map Text FieldDef -> Text -> Text
    mkFieldDesc fields name = let
      field = fields M.! name
      in T.concat [ " + ", name, " - ", fdDesc field, " - ", mkTypeDesc $ fdType field, "\n" ]
    mkTypeDesc FieldInt = "integer"
    mkTypeDesc FieldNum = "floating point number"
    mkTypeDesc FieldText = "text"

collectBot :: Env TFB -> BotApp State Action
collectBot env = BotApp
  { botInitialModel = initialModel
  , botAction = flip updateToAction
  , botHandler = handleAction env
  , botJobs = []
  }

run :: Config -> IO ()
run cfg@Config{ cfgToken=token
              , cfgConnection=connStr
              , cfgPoolSize=msz
              } = withDb connStr (fromMaybe 10 msz) $ \conn -> do
  mq <- newTBQueueIO 1000
  let environ = Env { envLogger = simpleMessageAction
                    , envConn = conn
                    , envQueue = mq
                    , envConfig = cfg
                    }
  _ <- forkIO $ sheetWorker environ
  env <- defaultTelegramClientEnv $ Token token
  startBot_ (conversationBot updateChatId $ collectBot environ) env
