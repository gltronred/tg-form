-- | Load and work with geolocation

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TFB.Geo where

import TFB.Types

import qualified Data.ByteString.Lazy as B
import Data.Csv
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import Data.Vector (Vector,minimumBy,empty)
import GHC.Generics
import System.Exit

data Geo = Geo
  { geoId :: Text
  , geoName :: Text
  , geoLat :: Double
  , geoLon :: Double
  , geoName3 :: Maybe Text
  , geoLat3 :: Maybe Double
  , geoLon3 :: Maybe Double
  , geoName2 :: Maybe Text
  , geoLat2 :: Maybe Double
  , geoLon2 :: Maybe Double
  , geoName1 :: Text
  , geoLat1 :: Double
  , geoLon1 :: Double
  } deriving (Eq,Show,Read,Generic)

instance FromRecord Geo
instance ToRecord Geo

type GeoDb = Vector Geo

emptyGeoDb :: GeoDb
emptyGeoDb = empty

loadGeoData :: FilePath -> IO GeoDb
loadGeoData fname = do
  contents <- B.readFile fname
  let ecsv = decode HasHeader contents
  case ecsv of
    Left e -> die e
    Right csv -> pure csv

findNearest :: GeoDb -> Double -> Double -> (NamedCoord, Geo)
findNearest db lat lon = (NamedCoord "" lat lon, minimumBy (comparing dist) db)
  where dist Geo{ geoLat=lt, geoLon=ln } = (lt - lat)^2 + (ln - lon)^2

geo2loc :: LocPrecision -> (NamedCoord, Geo) -> NamedCoord
geo2loc p (c,g) = case p of
  PrecCoord -> c
  PrecCity -> NamedCoord (geoName g) (geoLat g) (geoLon g)
  PrecMunicip -> case catMaybes [ NamedCoord <$> geoName3 g <*> geoLat3 g <*> geoLon3 g
                                , NamedCoord <$> geoName2 g <*> geoLat2 g <*> geoLon2 g
                                ] of
                   [] -> geo2loc PrecRegion (c,g)
                   x:_-> x
  PrecDistrict -> case geoName2 g of
                    Nothing -> geo2loc PrecRegion (c,g)
                    Just x  -> fromJust $ NamedCoord x <$> geoLat2 g <*> geoLon2 g
  PrecRegion -> NamedCoord (geoName1 g) (geoLat1 g) (geoLon1 g)
