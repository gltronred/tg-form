-- | Load and work with geolocation

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module TFB.Geo where

import TFB.Types

import qualified Data.ByteString.Lazy as B
import Data.Csv
import Data.Ord
import Data.Text (Text)
import Data.Vector (Vector,minimumBy)
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

loadGeoData :: FilePath -> IO GeoDb
loadGeoData fname = do
  contents <- B.readFile fname
  let ecsv = decode HasHeader contents
  case ecsv of
    Left e -> die e
    Right csv -> pure csv

findNearest :: GeoDb -> Double -> Double -> Loc
findNearest db lat lon = geo2loc $ minimumBy (comparing dist) db
  where dist Geo{ geoLat=lt, geoLon=ln } = (lt - lat)^2 + (ln - lon)^2
        geo2loc g = Loc (geoId g) (geoName g) (geoName3 g) (geoName2 g) (geoName1 g)
