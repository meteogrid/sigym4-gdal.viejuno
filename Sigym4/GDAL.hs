{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Sigym4.GDAL (
    createGeoreferencedDataset
  , module GDAL
  , module OSR
) where

import Data.Proxy (Proxy(..))
import Sigym4.Geometry
import OSGeo.GDAL as GDAL
import OSGeo.OSR as OSR


createGeoreferencedDataset ::
     forall srid t. (KnownNat srid, HasDataset Dataset ReadWrite t)
  => GeoReference V2 srid -> String -> DriverOptions -> Int -> FilePath
  -> IO (Dataset ReadWrite t)
createGeoreferencedDataset geoRef dName dOpts nBands fPath = do
  let Size (V2 nx ny) = grSize geoRef
      gt              = toGDALGeoTrans $ grTransform geoRef
      srs             = fromEPSG . fromIntegral $ gSrid (Proxy :: Proxy srid)
  ds <- create dName fPath nx ny nBands dOpts
  setDatasetGeotransform ds gt
  case srs of
    Left e     -> fail $ show e
    Right srs' -> setDatasetProjection ds (toWkt srs')
  return ds

toGDALGeoTrans :: GeoTransform V2 srid -> GDAL.Geotransform
toGDALGeoTrans gt = Geotransform a b c d e f
  where
    V2 (V2 b e) (V2 c f) = gtMatrix gt
    V2 a d               = gtOrigin gt
