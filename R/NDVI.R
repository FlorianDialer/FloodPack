#Calculate NDVI

NDVI <- function(raster){

  red_band <- terra::names(raster)[grepl("B04", terra::names(raster))]
  nir_band <- terra::names(raster)[grepl("B08", terra::names(raster))]

  NDVI_raster <- (raster[[nir_band]]-raster[[red_band]])/(raster[[nir_band]]+raster[[red_band]])

  return(NDVI_raster)

}
