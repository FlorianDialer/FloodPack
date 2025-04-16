#' Processing of Sentinel-2 Bands with Cloud Masking and AOI Cropping/Masking
#'
#' @param aoi Area of Interest as the Path to a Vector File .shp or .gpkg
#' @param condition Specifies the Images Depiction, either pre_flood or flood01, flood02... flood10... as a String with NO File Extension
#'
#' @returns The processed Sentinel-2 bands cropped and masked to your AOI, including Cloud Masking and Mosaic Creation in the Working Directory in the folder "processed-data"
#' @export
#'
#' @examples
#'
#' aoi <- "link-to-file.gpkg"
#'
#' condition <- "flood_01"
#'
#'
#' @importFrom tools file_ext
#' @importFrom sf st_read st_transform
#' @importFrom terra rast crs project resample mask crop nlyr time mosaic writeRaster
#' @importFrom XML xmlParse xmlRoot xmlValue
#' @importFrom glue glue

S2_data_processing <- function(aoi, condition) {

  message("Reading in Data...")

  #Reading AOI files and checking for correct extension
  if(tools::file_ext(aoi) != "shp" & tools::file_ext(aoi) != "gpkg") stop("The provided AOI file needs to be in format .shp or .gpkg!")

  aoi <- sf::st_read(aoi, quiet = TRUE)
  aoi <- aoi[1,]

  #Reading Tiles and grabbing files by their extension
  tiles <- list.dirs(path = file.path(getwd(), "raw_data", condition), full.names = T)
  tiles <- tiles[-1]

  if(length(tiles)==0) stop("Unvalid condition provided!")


  xmls <- lapply(tiles, function(tiles) {
    list.files(path = tiles, pattern = "\\.xml$", full.names = TRUE, recursive = FALSE)
  })

  jp2s <- lapply(tiles, function(tiles) {
    list.files(path = tiles, pattern = "\\.jp2$", full.names = TRUE, recursive = FALSE)
  })


  #Processing Tiles individually
  message("Applying Cloud Mask and Cropping to AOI...")

  rasters_list <- list()

  for (i in seq_along(jp2s)) {
    message("Currently processing tile ", i, " of ", length(jp2s))

    tile_jp2s <- jp2s[[i]]

    #Getting bands and Scene Classification Mask
    tile_bands<- tile_jp2s[!grepl("SCL", tile_jp2s)]
    tile_scl <- tile_jp2s[grepl("SCL", tile_jp2s)]

    tile_bands_rast <- terra::rast(tile_bands)

    #Making sure that all raster files have the same CRS
    if (terra::crs(tile_bands_rast) != terra::crs(terra::rast(jp2s[[1]][1]))) {
      tile_bands_rast <- terra::project(tile_bands_rast, terra::crs(terra::rast(jp2s[[1]][1])))
    }

    tile_SCL_rast <- terra::rast(tile_scl)

    if (terra::crs(tile_SCL_rast) != terra::crs(terra::rast(jp2s[[1]][1]))) {
      tile_SCL_rast <- terra::project(tile_SCL_rast, terra::crs(terra::rast(jp2s[[1]][1])))
    }

    #Resampling Scene Classification Mask due to 20m resolution (bands 10m), nearest neighbour because of discontinuous data
    tile_SCL_rast_resampled <- terra::resample(x = tile_SCL_rast, y = tile_bands_rast, method = "near")

    #Creating mask by setting values corresponding to vegetation, water and built-up to NA
    tile_SCL_rast_resampled[tile_SCL_rast_resampled == 4 | tile_SCL_rast_resampled == 5 | tile_SCL_rast_resampled == 6] <- NA

    #Masking with inverse TRUE so that other values get removed and only 4,5,6 persist
    masked_bands <- terra::mask(tile_bands_rast, tile_SCL_rast_resampled, inverse = TRUE)

    #Transforming AOI to CRS of raster layers for cropping and masking
    tile_crs <- terra::crs(masked_bands)

    aoi_with_tile_CRS <- sf::st_transform(aoi, crs = tile_crs)

    cropped_to_aoi <- terra::crop(masked_bands, aoi_with_tile_CRS)
    masked_to_aoi <- terra::mask(cropped_to_aoi, mask = aoi_with_tile_CRS)

    #Adding Time Stamp from meta_data file
    tile_xml <- xmls[[i]]

    tile_xml_information <- XML::xmlParse(tile_xml)
    tile_xml_roots <- XML::xmlRoot(tile_xml_information)
    production_time_info <- XML::xmlValue(tile_xml_roots[[1]][[1]][[1]])
    production_time <- as.vector(production_time_info)

    #Formatting as a time object, UTC to circumvent local time zone of user
    production_time_formatted <- strptime(production_time, format = "%Y-%m-%d", tz = "UTC")

    #Creating time object for every band in SpatRaster as global time setting not possible
    layers <- terra::nlyr(masked_to_aoi)
    date_of_production <- rep(production_time_formatted, layers)

    terra::time(masked_to_aoi) <- date_of_production

    rasters_list[[i]] <- masked_to_aoi

  }

  # Checking if mosaic creation is necessary, mosaic with min setting so that unclean cloud values get overwritten
  if(length(rasters_list) < 2){
    final_data_S2 <- rasters_list[[1]]
  } else  {
    message("Creating Raster Mosaic...")
    final_data_S2 <- do.call(terra::mosaic, c(rasters_list, fun = min))
  }

  #Creating folder for writing final raster object
  processed_data_directory <- file.path(getwd(), "processed_data")

  if (!dir.exists(processed_data_directory)) {
    dir.create(processed_data_directory)
  }

  message("Creating Final Raster...")
  terra::writeRaster(x = final_data_S2, filename = paste0(processed_data_directory, glue::glue("/{condition}.TIF")), overwrite = TRUE)
  message("Processing Complete!")

}


