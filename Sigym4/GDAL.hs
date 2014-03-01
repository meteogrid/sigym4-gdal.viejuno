{-# LANGUAGE RecordWildCards #-}
module Sigym4.GDAL (
    createGeoreferencedDataset
  , module GDAL
  , module OSR
) where

import Sigym4.Geometry
import OSGeo.GDAL as GDAL
import OSGeo.OSR as OSR


createGeoreferencedDataset geoRef dName dOpts nBands fPath = do
  let Size (V2 nx ny) = grSize geoRef
      gt              = toGDALGeoTrans $ grTransform geoRef
      srs             = case grSrs geoRef of
                          SrsProj4 proj4 -> fromProj4 proj4
                          SrsEPSG epsg   -> fromEPSG epsg
  ds <- create dName fPath nx ny nBands dOpts
  setDatasetGeotransform ds gt
  case srs of
    Left e     -> fail $ show e
    Right srs' -> setDatasetProjection ds (toWkt srs')
  return ds

toGDALGeoTrans :: GeoTransform V2 -> GDAL.Geotransform
toGDALGeoTrans gt = Geotransform a b c d e f
  where
    V2 (V2 b e) (V2 c f) = gtMatrix gt
    V2 a d               = gtOrigin gt
