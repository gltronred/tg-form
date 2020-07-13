module Main where

import TFB.Bot

import System.Environment

usage :: IO ()
usage = do
  putStrLn "Usage: tg-form-bot <config.json>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cfgFile] -> readConfigOrDie cfgFile >>= run
    _ -> usage
