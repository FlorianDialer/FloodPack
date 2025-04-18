#' Calculate Flood Areas based on Sentinel-2 Bands
#'
#' @param aoi Area of Interest as the Path to a Vector File .shp or .gpkg; Warning: Always the first Feature gets selected --> If you have multiple Geometries unionize them beforehand
#' @param path_to_rasters Optional: If you changed the processed Files Location or want to use your own Data specify the Directory as a String
#'
#' @returns Returns Flood Areas inside the AOI in Raster and Vector format named ascending in the Working Directory in the folder "flood_data" ; also returns an Elevation Folder in the Working Directory necessary for processing which can be ignored/deleted
#' @export
#'
#' @examples
#'
#'aoi <- "path/to/file.shp"
#'aoi <- "path/to/file.gpkg"
#'
#' path_to_rasters <- "path/to/folder/containing/processed-data"
#'
#'
#' @importFrom sf st_read st_transform st_bbox
#' @importFrom geodata elevation_3s
#' @importFrom terra rast merge crs project crop resample mask terrain time writeRaster as.polygons writeVector
#' @importFrom stringr str_pad

SFMI_flood_calculation <- function(aoi, path_to_rasters = file.path(getwd(), "processed_data")) {

  #Get final raster files for processing
  files_in_final_data <- list.files(path_to_rasters, full.names = TRUE)

  pre_flood_raster_path <- files_in_final_data[grepl("pre", files_in_final_data)]

  #Checking for correct amount of pre_flood files
  if (length(pre_flood_raster_path)>1) stop("There can only be one pre_flood raster!")

  if (length(pre_flood_raster_path)==0) stop('Missing pre_flood raster! Please provide a raster that includes "pre" in its name.')

  flood_rasters_path <- files_in_final_data[!grepl("pre", files_in_final_data)]


  #Download Elevation Data

  #Prepare Query - extract Latitude and Longitude values for elevation download
  aoi <- sf::st_read(aoi, quiet = TRUE)
  aoi <- sf::st_transform(aoi, "EPSG:4326")
  bbox <- sf::st_bbox(aoi[1,])
  lon_min <- floor(bbox[1])
  lon_max <- ceiling(bbox[3])
  lat_min <- floor(bbox[2])
  lat_max <- ceiling(bbox[4])

  #Creating folders for elevation data and SFMI data
  elevation_data_directory <- file.path(getwd(), "elevation_data")

  if (!dir.exists(elevation_data_directory)) {
    dir.create(elevation_data_directory)
  }

  SFMI_data_directory <- file.path(getwd(), "flood_data")

  if (!dir.exists(SFMI_data_directory)) {
    dir.create(SFMI_data_directory)
  }


  message("Downloading SRTM Elevation Data. This may take a while, depending on server availability...")

  # Nested looping through all longitudes and latitudes to cover AOI fully and downloading corresponding SRTM tiles
  for (longitude in c(lon_min:lon_max)) {
    for (latitude in c(lat_min:lat_max)) {
      geodata::elevation_3s(lon = longitude, lat = latitude, path = elevation_data_directory)
    }
  }

  #Grabbing all elevation files
  elevation_path <- file.path(elevation_data_directory, "elevation")
  elevation_tiles <- as.list(list.files(elevation_path, pattern = "\\.tif$", full.names = TRUE, recursive = FALSE))

  #Creating SpatRaster from elevation data
  elev_rasters <- list()

  for (i in seq_along(elevation_tiles)) {

    elevation_tile <- terra::rast(elevation_tiles[[i]])

    elev_rasters[[i]] <- elevation_tile
  }

  #Merging elevation tiles if necessary
  if (length(elev_rasters)==1) {
    full_elevation_data <- elev_rasters[[1]]
  } else {
    full_elevation_data <- do.call(terra::merge, elev_rasters)
  }


  #Convert elevation data to flood file raster CRS
  message("Processing Elevation Data...")

  pre_flood_raster <- terra::rast(pre_flood_raster_path)
  tile_crs <- terra::crs(pre_flood_raster)
  full_elevation_data_new_crs <- terra::project(full_elevation_data, tile_crs)

  #Cropping and Masking to AOI as well as resampling to 10m from 30m for same extent
  cropped_elevation_data <- terra::crop(full_elevation_data_new_crs, pre_flood_raster)
  resampled_elevation_data <- terra::resample(cropped_elevation_data, pre_flood_raster)
  final_elevation_data <- terra::mask(resampled_elevation_data, pre_flood_raster)


  #Convert Elevation Data to Slope in Percent for Mask
  slope <- terra::terrain(final_elevation_data[[1]], v="slope", neighbors=8, unit="degrees")

  #Based on YamCham (2021) Stack Exchange: https://gis.stackexchange.com/questions/404441/converting-a-degree-slope-raster-to-a-percent-slope-raster-without-a-dem-using
  percent_slope <- tan(slope*pi/180)*100

  #Mask slope greater 15% according to SFMI paper
  percent_slope_gt_15 <- percent_slope>15


  #NDVI Calculations for all raster files
  message("Calculating NDVI...")

  flood_raster_NDVI_list <- list()

  for (i in seq_along(flood_rasters_path)) {
    flood_raster <- terra::rast(flood_rasters_path[i])
    flood_NDVI <- NDVI(raster = flood_raster)
    flood_NDVI_gt_04 <- flood_NDVI>0.4

    flood_raster_NDVI_list[[i]] <- flood_NDVI_gt_04
  }



  #SFMI Calculations for all raster files
  message("Calculating SFMI...")

  pre_flood_SFMI <- SFMI(raster = pre_flood_raster)

  #Values greater 0 are water areas based on SFMI paper
  pre_flood_SFMI_gt_0 <- pre_flood_SFMI>0


  flood_raster_SFMI_list <- list()

  for (i in seq_along(flood_rasters_path)) {
    flood_raster <- terra::rast(flood_rasters_path[i])
    flood_SFMI <- SFMI(raster = flood_raster)

    #Values greater 0 are water areas based on SFMI paper
    flood_SFMI_gt_0 <- flood_SFMI>0

    flood_raster_SFMI_list[[i]] <- flood_SFMI_gt_0
  }


  #Calculate binary mask for water/flood areas
  message("Creating Final Flood Areas...")

  final_SFMI_list <- list()
  final_polygons <- list()

  for (i in seq_along(flood_raster_SFMI_list)) {

    #Removing water areas from before a flood
    difference_raster <- flood_raster_SFMI_list[[i]] - pre_flood_SFMI_gt_0

    #Removing NDVI>0.4 and Slope>15% as according to SFMI paper
    final_SFMI <- difference_raster - (flood_raster_NDVI_list[[i]] + percent_slope_gt_15)

    #Masking based on left-over areas, mask-value 0 removes areas where SFMI mask is not 1
    final_SFMI_binary <- final_SFMI == 1

    final_SFMI_binary_masked <- terra::mask(final_SFMI, final_SFMI_binary, maskvalues = 0)

    #Extracting date information for polygons as it would be lost otherwise
    date_info <- as.character(terra::time(final_SFMI_binary_masked))

    #Writing raster files
    final_SFMI_list[[i]] <- final_SFMI_binary_masked
    terra::writeRaster(final_SFMI_list[[i]], filename = paste0(SFMI_data_directory, "/SFMI_flood_", stringr::str_pad(i, width = 2, side = "left", pad = "0"), ".TIF"), overwrite = TRUE)

    #Converting to Polygons, appending data information and writing features to disk
    final_polygons[[i]] <- terra::as.polygons(final_SFMI_binary_masked)

    final_polygons[[i]]$date <- date_info

    terra::writeVector(final_polygons[[i]], filename = paste0(SFMI_data_directory, "/SFMI_flood_", stringr::str_pad(i, width = 2, side = "left", pad = "0"), ".gpkg"), overwrite = TRUE)

  }
  message("Calculations Complete!")
}


