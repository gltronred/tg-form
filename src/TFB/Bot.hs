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
import TFB.Parser (updateToAction)
import TFB.Sheets

import Colog (simpleMessageAction)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
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

initialModel :: State
initialModel = NotStarted

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) (x:_)  0 = Just x
(!?) (_:xs) n = xs !? (n-1)

runParser :: Reader a -> Text -> Either String a
runParser p t = fst <$> p t

parseFieldVal :: FieldType -> Answer -> Either String FieldVal
parseFieldVal field ans@Answer{ ansText=txt } = case field of
  FieldInt -> ValInt <$> runParser decimal txt
  FieldNum -> ValNum <$> runParser double txt
  FieldText -> Right $ ValText txt
  FieldEnum lopts -> case find (==txt) $ concat lopts of
    Nothing -> Left "Please, choose one of provided options"
    Just v  -> Right $ ValEnum v
  -- TODO: use precision to get city, region etc.
  FieldLocation prec -> case ansLocation ans of
    Nothing -> Left "Please send your location"
    Just loc -> Right $ ValLocation (realToFrac $ locationLatitude loc, realToFrac $ locationLongitude loc)
  FieldTime -> Left "Something went wrong: should not parse current time"
  FieldSource -> Left "Something went wrong: should not parse your user id"
  FieldWelcome -> Left "Something went wrong: should not parse welcome text"
  FieldThanks -> Left "Something went wrong: should not parse thanks text"

mkQuestion :: FieldDef -> UserId -> IO (Either FieldVal ReplyMessage)
mkQuestion field uid = case fdType field of
  FieldTime -> Left . ValTime . round <$> getPOSIXTime
  FieldSource -> pure $ Left $ ValUser uid
  FieldWelcome -> pure $ Left ValVoid
  FieldThanks -> pure $ Left ValVoid
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

fieldByType :: FieldType -> Text -> FormConfig -> Text
fieldByType ty def FormConfig{ cfgFields=fs } = case find ((==ty) . fdType) fs of
  Nothing -> def
  Just FieldDef{ fdDesc=t } -> t

welcomeField :: FormConfig -> Text
welcomeField f = fieldByType FieldWelcome defWelcome f
  where defWelcome = "Please fill the form by " <> T.pack (show $ cfgAuthor f)

thanksField :: FormConfig -> Text
thanksField = fieldByType FieldThanks "Thank you for participating!"

handleAction :: Env TFB -> Action -> State -> Eff Action State
handleAction env@Env{ } act st@NotStarted = case act of
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
        reply $ toReplyMessage $ welcomeField form
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
  Help -> usage st Nothing
  Ans _ -> usage st Nothing
  Parsed _ _ -> usage st Nothing
  AskCurrent -> usage st Nothing
handleAction env@Env{ } act st@PreparingSheet{} = case act of
  NoOp -> pure st
  -- Cancel and stop
  Cancel -> become NotStarted
  Stop -> become NotStarted
  -- Help and wrong commands
  Help -> usage st Nothing
  Start _ _ -> usage st Nothing
  GoForm _ _ -> usage st Nothing
  Parsed _ _ -> usage st Nothing
  AskCurrent -> usage st Nothing
handleAction Env{ envQueue=mq } act st@Answered{ stForm=form, stCurrent=current } = case act of
  NoOp -> pure st
  AskCurrent -> let
    mfield = cfgFields form !? current
    in case mfield of
         Nothing -> NotStarted <# do
           reply $ toReplyMessage $ thanksField form
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
        liftIO $ atomically $ writeTBQueue mq $ MsgInfo form ans
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
  -- Cancel and stop
  Cancel -> become $ st { stCurrent = 0, stAnswers = M.empty }
  Stop -> become NotStarted
  -- Help and wrong commands
  Help -> usage st $ Just form
  Start _ _ -> usage st $ Just form
  GoForm _ _ -> usage st $ Just form

usage :: State -> Maybe FormConfig -> Eff Action State
usage st mform = st <# do
  reply $ toReplyMessage $ botUsage st mform
  pure NoOp

become :: State -> Eff Action State
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

botUsage :: State -> Maybe FormConfig -> Text
botUsage st mform = T.intercalate "\n" $
  [ "This bot collects answers and writes it into Google Sheets"
  , ""
  , "You can create your own form: see <...TODO...> for full manual"
  , ""
  ] ++ stateDesc st ++ maybe [""] (formDesc $ getAnswers st) mform
  where
    stateDesc :: State -> [Text]
    stateDesc NotStarted = "You have not started working with bot" : cmds 0
    stateDesc PreparingSheet{} = "You are preparing your form" : cmds 3
    stateDesc Answered{} = "You are filling form" : cmds 2
    cmds :: Int -> [Text]
    cmds k = map snd $ filter ((`testBit` k) . fst)
      [ ((7 :: Int), "Available commands: ")
      , (1, "- /start - to start creating form")
      , (1, "- /start <code> - to start filling form <code>")
      , (7, "- /help - show this text")
      , (2, "- /cancel - cancel your answers and start filling form again")
      , (6, "- /stop - stop this bot (then you can fill another form or create your own)")
      , (6, "All other text is interpreted as answer to the current question")
      ]
    -- TODO: add form author and title
    formDesc :: Map Text FieldVal -> FormConfig -> [Text]
    formDesc ans f = "You are filling a form with following fields: " :
                     map (mkFieldDesc ans) (cfgFields f)
    mkFieldDesc :: Map Text FieldVal -> FieldDef -> Text
    mkFieldDesc ans FieldDef{ fdName=name, fdDesc=desc, fdType=ty } = let
      val = case M.lookup name ans of
        Nothing -> ""
        Just v -> "- your answer: " <> mkValDesc v
      in T.concat [ " + ", name, " - ", desc, " - ", mkTypeDesc ty, val ]
    mkTypeDesc :: FieldType -> Text
    mkTypeDesc FieldInt = "integer"
    mkTypeDesc FieldNum = "number"
    mkTypeDesc FieldText = "free form text"
    mkTypeDesc (FieldEnum _) = "fixed answer options"
    mkTypeDesc (FieldLocation prec) = "location"
    mkTypeDesc FieldTime = "time"
    mkTypeDesc FieldSource = "telegram user id"
    mkValDesc :: FieldVal -> Text
    mkValDesc v = T.pack $ show v

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
