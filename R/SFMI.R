# SFMI Idea from: https://www.sciencedirect.com/science/article/pii/S0098300424002255

SFMI <- function(aoi, path_to_rasters = file.path(getwd(), "final_data")) {

  #Get final raster files for processing
  files_in_final_data <- list.files(path_to_rasters, full.names = TRUE)

  pre_flood_raster <- files_in_final_data[grepl("pre", files_in_final_data)]

  if (length(pre_flood_raster)>1) warning("There can only be one pre_flood raster!")

  flood_rasters <- files_in_final_data[!grepl("pre", files_in_final_data)]



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


}


