{-# LANGUAGE RecordWildCards #-}
module Sigym4.GDAL (
    createGeoreferencedDataset
  , module X
) where

import Sigym4.GeoReference
import OSGeo.GDAL as X
import OSGeo.OSR as X


createGeoreferencedDataset geoRef dName dOpts nBands fPath = do
  let Shape nx ny = shape geoRef
      gt          = geoTransform geoRef
  ds <- create dName fPath nx ny nBands dOpts
  setDatasetGeotransform ds gt
  case fromProj4 (srs geoRef) of
    Right sr -> do
      setDatasetProjection ds (toWkt sr)
    _ -> return()
  return ds

geoTransform :: GeoReference -> Geotransform
geoTransform gr@GeoReference{..} = Geotransform x0 dx 0 y1 0 (-dy)
  where
    Shape dx dy = pixelShape gr
    x0 = minx extent
    y1 = maxy extent
