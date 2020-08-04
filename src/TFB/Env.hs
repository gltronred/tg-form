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
