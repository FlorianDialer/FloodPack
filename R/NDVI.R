#' NDVI Calculation
#'
#' @param raster Spatraster
#'
#' @returns Spatraster
#'
#' @importFrom terra names

NDVI <- function(raster){

  #Grabbing the correct bands for calculation
  red_band <- terra::names(raster)[grepl("B04", terra::names(raster))]
  nir_band <- terra::names(raster)[grepl("B08", terra::names(raster))]


  #Creating Index
  NDVI_raster <- (raster[[nir_band]]-raster[[red_band]])/(raster[[nir_band]]+raster[[red_band]])

  return(NDVI_raster)

}
