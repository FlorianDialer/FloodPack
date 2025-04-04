# SFMI Idea from: https://www.sciencedirect.com/science/article/pii/S0098300424002255

# https://gis.stackexchange.com/questions/404441/converting-a-degree-slope-raster-to-a-percent-slope-raster-without-a-dem-using

SFMI <- function(aoi, path_to_rasters = file.path(getwd(), "final_data")) {

  #Get final raster files for processing
  files_in_final_data <- list.files(path_to_rasters, full.names = TRUE)

  pre_flood_raster_path <- files_in_final_data[grepl("pre", files_in_final_data)]

  if (length(pre_flood_raster_path)>1) warning("There can only be one pre_flood raster!")

  flood_rasters_path <- files_in_final_data[!grepl("pre", files_in_final_data)]



  #Download Elevation Data

  #Prepare Query - extract Latitude and Longitude values
  aoi <- sf::st_read(aoi, quiet = TRUE)
  aoi <- sf::st_transform(aoi, "EPSG:4326")
  bbox <- sf::st_bbox(aoi)
  lon_min <- floor(bbox[1])
  lon_max <- ceiling(bbox[3])
  lat_min <- floor(bbox[2])
  lat_max <- ceiling(bbox[4])

  elevation_data_directory <- file.path(getwd(), "elevation_data")

  if (!dir.exists(elevation_data_directory)) {
    dir.create(elevation_data_directory)
  }

  # Nested Loop through all longitudes and latitudes to cover AOI fully
  for (longitude in c(lon_min:lon_max)) {
    for (latitude in c(lat_min:lat_max)) {
      geodata::elevation_3s(lon = longitude, lat = latitude, path = elevation_data_directory, quiet = TRUE)
    }
  }

  elevation_path <- file.path(elevation_data_directory, "elevation")
  elevation_tiles <- as.list(list.files(elevation_path, pattern = "\\.tif$", full.names = TRUE, recursive = FALSE))

  elev_rasters <- list()

  for (i in seq_along(elevation_tiles)) {

    elevation_tile <- terra::rast(elevation_tiles[[i]])

    elev_rasters[[i]] <- elevation_tile
  }

  if (length(elev_rasters)==1) {
    full_elevation_data <- elev_rasters[[1]]
  } else {
    full_elevation_data <- do.call(terra::merge, elev_rasters)
  }


  #Convert elevation data to S2 raster CRS
  tile_crs <- terra::crs(terra::rast(pre_flood_raster_path))
  full_elevation_data_new_crs <- terra::project(full_elevation_data, tile_crs)
  aoi <- sf::st_transform(aoi, crs = tile_crs)

  cropped_elevation_data <- terra::crop(full_elevation_data_new_crs, aoi)
  final_elevation_data <- terra::mask(cropped_elevation_data, aoi)


  #Convert Elevation Data to Slope for Mask

  slope <- terra::terrain(final_elevation_data, v="slope", neighbors=8, unit="degrees")

  percent_slope <- tan(slope*pi/180)*100 #based on YamCham (2021) Stack Exchange

  percent_slope_gt_15 <- percent_slope>15 #mask slope greater 15% according to paper


  #Calculate NDVI

  NDVI <- function(raster){

    red_band <- terra::names(raster)[grepl("B04", terra::names(raster))]
    nir_band <- terra::names(raster)[grepl("B08", terra::names(raster))]

    NDVI_raster <- (raster[[nir_band]]-raster[[red_band]])/(raster[[nir_band]]+raster[[red_band]])

    return(NDVI_raster)

  }


  pre_flood_raster <- terra::rast(pre_flood_raster_path)
  pre_flood_NDVI <- NDVI(raster = pre_flood_raster)
  pre_flood_NDVI_gt_04 <- pre_flood_NDVI>0.4


  flood_raster_NDVI_list <- list()

  for (i in seq_along(flood_rasters_path)) {
    flood_raster <- terra::rast(flood_rasters_path[i])
    flood_NDVI <- NDVI(raster = flood_raster)
    flood_NDVI_gt_04 <- flood_NDVI>0.4

    flood_raster_NDVI_list[[i]] <- flood_NDVI_gt_04
  }



}


