module Main where

import CEC.Bot

import System.Environment

usage :: IO ()
usage = do
  putStrLn "Usage: tgcec <config.json>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cfgFile] -> readConfigOrDie cfgFile >>= run
    _ -> usage
