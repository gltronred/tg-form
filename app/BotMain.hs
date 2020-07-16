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
