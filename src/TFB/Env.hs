-- | Environment to run bot

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module TFB.Env where

import TFB.Types
import TFB.Geo (GeoDb)

import Colog
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Pool
import Database.Groundhog.Sqlite

type Connection = Pool Sqlite

data Env m = Env
  { envLogger :: LogAction m Message
  , envConn :: Connection
  , envQueue :: TBQueue MsgItem
  , envConfig :: Config
  , envGeoDb :: GeoDb
  }

instance HasLog (Env m) Message m where
  getLogAction = envLogger
  {-# INLINE getLogAction #-}
  setLogAction new env = env { envLogger = new }
  {-# INLINE setLogAction #-}

newtype TFB a = TFB {
  runTFB :: ReaderT (Env TFB) IO a
  } deriving (Functor,Applicative,Monad,MonadIO,MonadReader (Env TFB))
