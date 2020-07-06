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

deriving instance Read Location

data Action
  = NoOp
  | Start
  | Help
  | Register UserId Location
  | Ans Text
  | Trust Text Text
  | Me UserId
  deriving (Read,Show)

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
      Register <$> user <*> location
  <|> Start <$ cmd "start"
  <|> Help <$ cmd "help"
  <|> const Me <$> cmd "me" <*> user
  <|> callbackQueryDataRead
updateToAction _st = parseUpdate $
      Register <$> user <*> location
  <|> Help <$ cmd "help"
  <|> flip Trust <$> cmd "trust" <*> fmap (T.pack . show . (\(UserId u) -> u)) user
  <|> const Me <$> cmd "me" <*> user
  <|> Ans <$> plaintext
  <|> callbackQueryDataRead

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) (x:_)  0 = Just x
(!?) (_:xs) n = xs !? (n-1)

mkErrorMsg :: [Text] -> String -> Maybe QuestionDef -> Text
mkErrorMsg allFields err mqd = case qdError =<< mqd of
  Just t -> t <> "\n" <> T.pack err
  Nothing -> T.concat
    [ "Parse error: "
    , T.pack err
    , ". Please write following fields: "
    , T.intercalate ", " (maybe allFields qdAnswer mqd)
    , " delimiting them by space" ]

runParser :: Reader a -> Text -> Either String a
runParser p t = fst <$> p t

parseFieldVal :: (FieldDef, Text) -> Either String (Text, FieldVal)
parseFieldVal (field, txt) = (fdName field,) <$> case fdType field of
  FieldInt -> ValInt <$> runParser decimal txt
  FieldFloat -> ValFloat <$> runParser double txt
  FieldText -> Right $ ValText txt

addAnswer :: Config -> Int -> Map Text FieldVal -> Text -> Either String (Map Text FieldVal)
addAnswer cfg idx olds txt = let
  mquestion = cfgQuestions cfg !? idx
  fields = case mquestion of
    Nothing -> cfgFields cfg
    Just question -> map (\name -> fromJust . find ((==name) . fdName) $ cfgFields cfg) $ qdAnswer question
  ws = T.words txt
  in if length ws == length fields
     then (olds `M.union`) . M.fromList <$> mapM parseFieldVal (zip fields ws)
     else if length fields == 1 && fdType (head fields) `elem` [FieldText]
          then (olds `M.union`) . uncurry M.singleton <$> parseFieldVal (head fields, txt)
          else Left "Incorrect number of fields"

addRequiredFields :: Config -> Text -> Map Text FieldVal -> IO (Map Text FieldVal)
addRequiredFields cfg usr olds = do
  let tn = cfgTimeField cfg
      un = cfgSourceField cfg
  ts <- round <$> getPOSIXTime
  pure $
    M.insert tn (ValTime ts) $
    M.insert un (ValUser usr) $
    olds

handleAction :: Config -> TBQueue MsgItem -> Action -> State -> Eff Action State
handleAction cfg mq act st = case act of
  NoOp -> pure st
  Help -> st <# do
    reply $ toReplyMessage $ botUsage cfg
    pure NoOp
  Start -> st <# do
    reply (toReplyMessage $ cfgWelcome cfg)
      { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup regKeyboard) }
    pure NoOp
  Register (UserId uid) geo -> let
    src = T.pack $ show uid
    st' = Answered src 0 M.empty
    in st' <# do
    reply (toReplyMessage q0)
    pure NoOp
  Trust src tgt -> st <# do
    reply $ toReplyMessage $ trustAnswer tgt
    ts <- round <$> liftIO getPOSIXTime
    liftIO $ atomically $ writeTBQueue mq $ MsgTrust (ts, src, tgt)
    pure NoOp
  Me (UserId uid) -> st <# do
    reply $ toReplyMessage $ meAnswer uid
    pure NoOp
  Ans t -> case addAnswer cfg (getCurrent st) (getAnswers st) t of
    Left err -> st <# do
      let fieldNames = map fdName $ cfgFields cfg
      reply (toReplyMessage $ mkErrorMsg fieldNames err $ cfgQuestions cfg !? getCurrent st)
      pure NoOp
    Right ans -> let
      idx = getCurrent st + 1
      nextQuestion = qdText <$> cfgQuestions cfg !? idx
      st'' = case nextQuestion of
        Nothing -> Answered (stSrc st) 0 M.empty
        Just _ -> Answered
          { stSrc = stSrc st
          , stCurrent = idx
          , stAnswers = ans
          }
      in st'' <# case nextQuestion of
                   Nothing -> do
                     res <- liftIO $ addRequiredFields cfg (stSrc st) ans
                     liftIO $ atomically $ writeTBQueue mq $ MsgInfo res
                     reply (toReplyMessage "sent")
                     pure NoOp
                   Just nq -> do
                     reply (toReplyMessage nq)
                     pure NoOp
  where
    regKeyboard = ReplyKeyboardMarkup
      { replyKeyboardMarkupKeyboard =
        [ [ KeyboardButton regButtonName Nothing (Just True) ] ]
      , replyKeyboardMarkupResizeKeyboard = Just True
      , replyKeyboardMarkupOneTimeKeyboard = Just True
      , replyKeyboardMarkupSelective = Nothing
      }
    regButtonName = fromMaybe "Register location" $ cfgRegisterButton cfg
    regAnswer loc = T.concat
      [ fromMaybe "Thank you!" (cfgRegisterAnswer cfg), "\n"
      ]
    q0 = maybe "" (("\n" <>) . qdText) $ listToMaybe $ cfgQuestions cfg
    trustAnswer tgt = fromMaybe "Started trusting user-id " (cfgTrustAnswer cfg) <> tgt
    meAnswer uid = let
      u = T.pack $ show uid
      in "Your userid: " <> u <> ". Someone can use `/trust " <> u <> "` to show that they trust you"

botUsage :: Config -> Text
botUsage cfg = T.concat $
  [ "This bot collects statistics and writes it into Google Sheets\n\n"
  , "Your telegram user "
  , "\n"
  , "Bot also saves time and your approximate location\n\n"
  , "Results are published here: ", cfgResult cfg, "\n"
  , "Author provided the following description: \n"
  , cfgWelcome cfg, "\n"
  , "\n"
  , "Questions asked and statistics collected:\n"
  ] ++ map mkQuestionDesc (zip [1..] $ cfgQuestions cfg)
  where
    mkQuestionDesc :: (Int, QuestionDef) -> Text
    mkQuestionDesc (i, qd) = let
      fields = M.fromList $ map (\f -> (fdName f, f)) $ cfgFields cfg
      in T.concat $
         [ T.pack $ show i, ". ", qdText qd, "\n" ] ++
         map (mkFieldDesc fields) (qdAnswer qd)
    mkFieldDesc :: Map Text FieldDef -> Text -> Text
    mkFieldDesc fields name = let
      field = fields M.! name
      in T.concat [ " + ", name, " - ", fdDesc field, " - ", mkTypeDesc $ fdType field, "\n" ]
    mkTypeDesc FieldInt = "integer"
    mkTypeDesc FieldFloat = "floating point number"
    mkTypeDesc FieldText = "text"

collectBot :: Config -> TBQueue MsgItem -> BotApp State Action
collectBot cfg mq = BotApp
  { botInitialModel = initialModel
  , botAction = flip updateToAction
  , botHandler = handleAction cfg mq
  , botJobs = []
  }

run :: Config -> IO ()
run cfg@Config{ cfgBot = BotCfg{ bcToken = token } } = do
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
