#https://stackoverflow.com/questions/74934891/specifying-layers-when-using-rast-type-xyz-to-convert-data-frame-to-spatra


S2_data_processing <- function(path_to_temp_data_directory = getwd(), aoi, condition) {


  if(tools::file_ext(aoi) != "shp" & tools::file_ext(aoi) != "gpkg") warning("The provided AOI file needs to be in format .shp or .gpkg!")

  aoi <- sf::st_read(aoi, quiet = TRUE)
  aoi <- aoi[1,]


  tiles <- list.dirs(path = file.path(getwd(), "temp_data_directory", condition), full.names = T)
  tiles <- tiles[-1]

  if(length(tiles)==0) warning("Unvalid condition provided!")


  xmls <- lapply(tiles, function(tiles) {
    list.files(path = tiles, pattern = "\\.xml$", full.names = TRUE, recursive = FALSE)
  })

  jp2s <- lapply(tiles, function(tiles) {
    list.files(path = tiles, pattern = "\\.jp2$", full.names = TRUE, recursive = FALSE)
  })

  rasters_list <- list()

  for (i in seq_along(jp2s)) {
    tile_jp2s <- jp2s[[i]]

    tile_bands<- tile_jp2s[!grepl("SCL", tile_jp2s)]
    tile_scl <- tile_jp2s[grepl("SCL", tile_jp2s)]

    tile_bands_rast <- terra::rast(tile_bands)

    if (terra::crs(tile_bands_rast) != terra::crs(terra::rast(jp2s[[1]][1]))) {
      tile_bands_rast <- terra::project(tile_bands_rast, terra::crs(terra::rast(jp2s[[1]][1])))
    }

    tile_SCL_rast <- terra::rast(tile_scl)

    if (terra::crs(tile_SCL_rast) != terra::crs(terra::rast(jp2s[[1]][1]))) {
      tile_SCL_rast <- terra::project(tile_SCL_rast, terra::crs(terra::rast(jp2s[[1]][1])))
    }

    tile_SCL_rast_resampled <- terra::resample(x = tile_SCL_rast, y = tile_bands_rast, method = "near")

    tile_SCL_rast_resampled[tile_SCL_rast_resampled == 4 | tile_SCL_rast_resampled == 5 | tile_SCL_rast_resampled == 6] <- NA

    masked_bands <- terra::mask(tile_bands_rast, tile_SCL_rast_resampled, inverse = TRUE)


    tile_crs <- terra::crs(masked_bands)

    aoi_with_tile_CRS <- sf::st_transform(aoi, crs = tile_crs)

    cropped_to_aoi <- terra::crop(masked_bands, aoi_with_tile_CRS)
    masked_to_aoi <- terra::mask(cropped_to_aoi, mask = aoi_with_tile_CRS)

    #Adding Time Stamp

    tile_xml <- xmls[[i]]

    tile_xml_information <- XML::xmlParse(tile_xml)
    tile_xml_roots <- XML::xmlRoot(tile_xml_information)
    production_time_info <- XML::xmlValue(tile_xml_roots[[1]][[1]][[1]])
    production_time <- as.vector(production_time_info)

    production_time_formatted <- strptime(production_time, format = "%Y-%m-%d", tz = "UTC")

    layers <- terra::nlyr(masked_to_aoi)
    date_of_production <- rep(production_time_formatted, layers)

    terra::time(masked_to_aoi) <- date_of_production

    rasters_list[[i]] <- masked_to_aoi

  }


  if(length(rasters_list) < 2){
    final_data_S2 <- rasters_list[[1]]
  } else  {
    final_data_S2 <- do.call(terra::mosaic, c(rasters_list, fun = min))
  }

  final_data_directory <- file.path(getwd(), "final_data")

  if (!dir.exists(final_data_directory)) {
    dir.create(final_data_directory)
  }

  terra::writeRaster(x = final_data_S2, filename = paste0(final_data_directory, glue::glue("/{condition}.TIF")), overwrite = TRUE)


}


