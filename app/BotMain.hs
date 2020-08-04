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
module Main where

import TFB.Types (Config)
import TFB.Bot (run)

import Data.Aeson
import System.Environment
import System.Exit

usage :: IO ()
usage = do
  putStrLn "Usage: tg-form-bot <config.json>"

readConfigOrDie :: FilePath -> IO Config
readConfigOrDie cfgFile = do
  ecfg <- eitherDecodeFileStrict cfgFile
  case ecfg of
    Left err -> die err
    Right cfg -> pure cfg

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cfgFile] -> readConfigOrDie cfgFile >>= run
    _ -> usage
