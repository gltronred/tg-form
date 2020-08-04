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

-- | Parsing telegram messages

{-# LANGUAGE OverloadedStrings #-}

module TFB.Parser where

import TFB.Types

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Telegram.Bot.API
import Telegram.Bot.Simple.UpdateParser

updateText :: UpdateParser (Maybe Text)
updateText = mkParser $ fmap messageText <$> updateMessage

location :: UpdateParser (Maybe Location)
location = mkParser $ fmap messageLocation <$> updateMessage

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
  t <- fromMaybe "" <$> updateText
  if "/" `T.isPrefixOf` t
    then empty
    else pure t

answer :: UpdateParser Answer
answer = Answer <$> plaintext <*> location <*> user

updateToAction :: State -> Update -> Maybe Action
updateToAction NotStarted = parseUpdate $
      (\c u -> Start (if T.null c then Nothing else Just c) u) <$> cmd "start" <*> user
  <|> Help <$ cmd "help"
  <|> NewForm <$> user <*> cmd "newform"
  <|> (\c u -> Start (Just c) u) <$> callbackQueryDataRead <*> user
updateToAction _st = parseUpdate $
      (\c u -> Start (if T.null c then Nothing else Just c) u) <$> cmd "start" <*> user
  <|> Help <$ cmd "help"
  <|> Cancel <$ cmd "cancel"
  <|> NewForm <$> user <*> cmd "newform"
  <|> Ans <$> answer
