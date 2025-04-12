#Calculate SFMI

SFMI <- function(raster){

  #Grabbing the correct bands for calculation
  blue_band <- terra::names(raster)[grepl("B02", terra::names(raster))]
  green_band <- terra::names(raster)[grepl("B03", terra::names(raster))]
  red_band <- terra::names(raster)[grepl("B04", terra::names(raster))]
  nir_band <- terra::names(raster)[grepl("B08", terra::names(raster))]


  #Creating Index
  SFMI_raster <- (raster[[green_band]]+sqrt((raster[[blue_band]]+raster[[green_band]]+raster[[red_band]]))-raster[[nir_band]])/(raster[[green_band]]+sqrt((raster[[blue_band]]+raster[[green_band]]+raster[[red_band]]))+raster[[nir_band]])

  return(SFMI_raster)

}
